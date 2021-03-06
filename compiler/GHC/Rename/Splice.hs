{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Rename.Splice (
        rnTopSpliceDecls,
        rnSpliceType, rnSpliceExpr, rnSplicePat, rnSpliceDecl,
        rnBracket,
        checkThLocalName
        , traceSplice, SpliceInfo(..)
  ) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Tc.Utils.Monad
import GHC.Driver.Env

import GHC.Rename.Env
import GHC.Rename.Utils   ( HsDocContext(..), newLocalBndrRn )
import GHC.Rename.Unbound ( isUnboundName )
import GHC.Rename.Module  ( rnSrcDecls, findSplice )
import GHC.Rename.Pat     ( rnPat )
import GHC.Types.Basic    ( TopLevelFlag, isTopLevel )
import GHC.Types.SourceText ( SourceText(..) )
import GHC.Utils.Outputable
import GHC.Unit.Module
import GHC.Types.SrcLoc
import GHC.Rename.HsType ( rnLHsType )

import Control.Monad    ( unless, when )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Writer ( WriterT (..), mapWriterT, tell )

import {-# SOURCE #-} GHC.Rename.Expr ( rnLExpr )

import GHC.Driver.Session
import GHC.Data.FastString
import GHC.Utils.Error  ( dumpIfSet_dyn_printer, DumpFormat (..) )
import GHC.Utils.Lens.Monad
import GHC.Utils.Panic
import GHC.Tc.Utils.Env ( checkWellStaged, tcMetaTy )
import GHC.Driver.Hooks
import GHC.Builtin.Names.TH ( liftName, quoteExpName, quotePatName, quoteDecName, quoteTypeName
                            , decsQTyConName, expQTyConName, patQTyConName, typeQTyConName, )

import {-# SOURCE #-} GHC.Tc.Gen.Expr   ( tcCheckExpr )
import {-# SOURCE #-} GHC.Tc.Gen.Splice
    ( runMetaD
    , runMetaE
    , runMetaP
    , runMetaT
    , tcTopSpliceExpr
    )

import GHC.Tc.Utils.Zonk

import GHCi.RemoteTypes ( ForeignRef )
import qualified Language.Haskell.TH as TH (Q)

import qualified GHC.LanguageExtensions as LangExt

{-
************************************************************************
*                                                                      *
        Template Haskell brackets
*                                                                      *
************************************************************************
-}

rnBracket :: HsExpr GhcPs -> HsBracket GhcPs -> WriterT FreeVars RnM (HsExpr GhcRn)
rnBracket e br_body
  = addErrCtxt (quotationCtxtDoc br_body) $ WriterT $
    do { -- Check that -XTemplateHaskellQuotes is enabled and available
         thQuotesEnabled <- xoptM LangExt.TemplateHaskellQuotes
       ; unless thQuotesEnabled $
           failWith ( vcat
                      [ text "Syntax error on" <+> ppr e
                      , text ("Perhaps you intended to use TemplateHaskell"
                              ++ " or TemplateHaskellQuotes") ] )

         -- Check for nested brackets
       ; cur_stage <- getStage
       ; case cur_stage of
           { Splice Typed   -> checkTc (isTypedBracket br_body)
                                       illegalUntypedBracket
           ; Splice Untyped -> checkTc (not (isTypedBracket br_body))
                                       illegalTypedBracket
           ; RunSplice _    ->
               -- See Note [RunSplice ThLevel] in GHC.Tc.Types.
               pprPanic "rnBracket: Renaming bracket when running a splice"
                        (ppr e)
           ; Comp           -> return ()
           ; Brack {}       -> failWithTc illegalBracket
           }

         -- Brackets are desugared to code that mentions the TH package
       ; recordThUse

       ; runWriterT $ case isTypedBracket br_body of
            True  -> do { traceRn "Renaming typed TH bracket" mempty
                        ; HsBracket noExtField <$>
                          setStage (Brack cur_stage RnPendingTyped) (rn_bracket cur_stage br_body) }

            False -> do { traceRn "Renaming untyped TH bracket" mempty
                        ; ps_var <- newMutVar []
                        ; HsRnBracketOut noExtField
                          <$> setStage (Brack cur_stage (RnPendingUntyped ps_var)) (rn_bracket cur_stage br_body)
                          <*> readMutVar ps_var }
       }

rn_bracket :: ThStage -> HsBracket GhcPs -> WriterT FreeVars RnM (HsBracket GhcRn)
rn_bracket outer_stage br@(VarBr x flg rdr_name) = WriterT
  [ (VarBr x flg name, unitFV name)
  | name <- lookupOccRn rdr_name
  , this_mod <- getModule

  , () <- when (flg && nameIsLocalOrFrom this_mod name) $
             -- Type variables can be quoted in TH. See #5721.
                 lookupLocalOccThLvl_maybe name >>= \ case
                        { Nothing -> return ()      -- Can happen for data constructors,
                                                    -- but nothing needs to be done for them

                        ; Just (top_lvl, bind_lvl)  -- See Note [Quoting names]
                             | isTopLevel top_lvl
                             -> when (isExternalName name) (keepAlive name)
                             | otherwise
                             -> do { traceRn "rn_bracket VarBr"
                                      (ppr name <+> ppr bind_lvl
                                                <+> ppr outer_stage)
                                   ; checkTc (thLevel outer_stage + 1 == bind_lvl)
                                             (quotedNameStageErr br) }
                        }
  ]

rn_bracket _ (ExpBr x e) = ExpBr x <$> rnLExpr e

rn_bracket _ (PatBr x p) = rnPat ThPatQuote p $ pure . PatBr x

rn_bracket _ (TypBr x t) = TypBr x <$> rnLHsType TypBrCtx t

rn_bracket _ (DecBrL x decls) = WriterT
  [ (DecBrG x group', duUses (tcg_dus tcg_env))
  | group <- groupDecls decls
  , (tcg_env, group') <- locally (env_gblL . tcg_dusL) (pure emptyDUs) (rnSrcDecls group)
    -- The emptyDUs is so that we just collect uses for this
    -- group alone in the call to rnSrcDecls below
    -- Discard the tcg_env; it contains only extra info about fixity
  , () <- traceRn "rn_bracket dec" (ppr (tcg_dus tcg_env) $$ ppr (duUses (tcg_dus tcg_env))) ]
  where
    groupDecls :: [LHsDecl GhcPs] -> RnM (HsGroup GhcPs)
    groupDecls decls
      = do { (group, mb_splice) <- findSplice decls
           ; case mb_splice of
           { Nothing -> return group
           ; Just (splice, rest) ->
                 over hs_splcdsL (noLoc splice :) . appendGroups group <$> groupDecls rest } }

rn_bracket _ (DecBrG {}) = panic "rn_bracket: unexpected DecBrG"

rn_bracket _ (TExpBr x e) = TExpBr x <$> rnLExpr e

quotationCtxtDoc :: HsBracket GhcPs -> SDoc
quotationCtxtDoc br_body = hang (text "In the Template Haskell quotation") 2 (ppr br_body)

illegalBracket :: SDoc
illegalBracket =
    text "Template Haskell brackets cannot be nested" <+>
    text "(without intervening splices)"

illegalTypedBracket :: SDoc
illegalTypedBracket =
    text "Typed brackets may only appear in typed splices."

illegalUntypedBracket :: SDoc
illegalUntypedBracket =
    text "Untyped brackets may only appear in untyped splices."

quotedNameStageErr :: HsBracket GhcPs -> SDoc
quotedNameStageErr br
  = sep [ text "Stage error: the non-top-level quoted name" <+> ppr br
        , text "must be used at the same stage at which it is bound" ]


{-
*********************************************************
*                                                      *
                Splices
*                                                      *
*********************************************************

Note [Free variables of typed splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider renaming this:
        f = ...
        h = ...$(thing "f")...

where the splice is a *typed* splice.  The splice can expand into
literally anything, so when we do dependency analysis we must assume
that it might mention 'f'.  So we simply treat all locally-defined
names as mentioned by any splice.  This is terribly brutal, but I
don't see what else to do.  For example, it'll mean that every
locally-defined thing will appear to be used, so no unused-binding
warnings.  But if we miss the dependency, then we might typecheck 'h'
before 'f', and that will crash the type checker because 'f' isn't in
scope.

Currently, I'm not treating a splice as also mentioning every import,
which is a bit inconsistent -- but there are a lot of them.  We might
thereby get some bogus unused-import warnings, but we won't crash the
type checker.  Not very satisfactory really.

Note [Renamer errors]
~~~~~~~~~~~~~~~~~~~~~
It's important to wrap renamer calls in checkNoErrs, because the
renamer does not fail for out of scope variables etc. Instead it
returns a bogus term/type, so that it can report more than one error.
We don't want the type checker to see these bogus unbound variables.
-}

rnSpliceGen :: (HsSplice GhcRn -> WriterT FreeVars RnM a)
                                            -- Outside brackets, run splice
            -> (HsSplice GhcRn -> (PendingRnSplice, a))
                                            -- Inside brackets, make it pending
            -> HsSplice GhcPs
            -> WriterT FreeVars RnM a
rnSpliceGen run_splice pend_splice splice = addErrCtxt (spliceCtxt splice) $ getStage >>= \ case
        Brack pop_stage RnPendingTyped ->
             do { lift $ checkTc is_typed_splice illegalUntypedSplice
                ; snd . pend_splice <$> setStage pop_stage (rnSplice splice) }

        Brack pop_stage (RnPendingUntyped ps_var)
          -> do { lift $ checkTc (not is_typed_splice) illegalTypedSplice
                ; splice' <- setStage pop_stage $ rnSplice splice
                ; let (pending_splice, result) = pend_splice splice'
                ; result <$ updMutVar ps_var (pending_splice :) }

        _ -> do { lift $ checkTopSpliceAllowed splice
                ; splice' <- mapWriterT checkNoErrs $ setStage (Splice splice_type) $
                      rnSplice splice
                  -- checkNoErrs: don't attempt to run the splice if
                  -- renaming it failed; otherwise we get a cascade of
                  -- errors from e.g. unbound variables
                ; run_splice splice' }
   where
     is_typed_splice = isTypedSplice splice
     splice_type | is_typed_splice = Typed | otherwise = Untyped


-- Nested splices are fine without TemplateHaskell because they
-- are not executed until the top-level splice is run.
checkTopSpliceAllowed :: HsSplice GhcPs -> RnM ()
checkTopSpliceAllowed splice = do
  let (herald, ext) = spliceExtension splice
  extEnabled <- xoptM ext
  unless extEnabled
    (failWith $ text herald <+> text "are not permitted without" <+> ppr ext)
  where
     spliceExtension :: HsSplice GhcPs -> (String, LangExt.Extension)
     spliceExtension (HsQuasiQuote {}) = ("Quasi-quotes", LangExt.QuasiQuotes)
     spliceExtension (HsTypedSplice {}) = ("Top-level splices", LangExt.TemplateHaskell)
     spliceExtension (HsUntypedSplice {}) = ("Top-level splices", LangExt.TemplateHaskell)
     spliceExtension s@(HsSpliced {}) = pprPanic "spliceExtension" (ppr s)

------------------

-- | Returns the result of running a splice and the modFinalizers collected
-- during the execution.
--
-- See Note [Delaying modFinalizers in untyped splices].
runRnSplice :: UntypedSpliceFlavour
            -> (LHsExpr GhcTc -> TcRn res)
            -> (res -> SDoc)    -- How to pretty-print res
                                -- Usually just ppr, but not for [Decl]
            -> HsSplice GhcRn   -- Always untyped
            -> TcRn (res, [ForeignRef (TH.Q ())])
runRnSplice flavour run_meta ppr_res splice
  = do { hooks <- hsc_hooks <$> getTopEnv
       ; splice' <- fromMaybe pure (runRnSpliceHook hooks) splice

       ; let the_expr = case splice' of
                HsUntypedSplice _ _ _ e   ->  e
                HsQuasiQuote _ _ q qs str -> mkQuasiQuoteExpr flavour q qs str
                HsTypedSplice {}          -> pprPanic "runRnSplice" (ppr splice)
                HsSpliced {}              -> pprPanic "runRnSplice" (ppr splice)

             -- Typecheck the expression
       ; meta_exp_ty   <- tcMetaTy meta_ty_name
       ; zonked_q_expr <- zonkTopLExpr =<<
                            tcTopSpliceExpr Untyped
                              (tcCheckExpr the_expr meta_exp_ty)

             -- Run the expression
       ; mod_finalizers_ref <- newMutVar []
       ; result <- setStage (RunSplice mod_finalizers_ref) $ run_meta zonked_q_expr
       ; mod_finalizers <- readMutVar mod_finalizers_ref
       ; (result, mod_finalizers) <$ traceSplice SpliceInfo
           { spliceDescription = what
           , spliceIsDecl      = is_decl
           , spliceSource      = Just the_expr
           , spliceGenerated   = ppr_res result } }

  where
    meta_ty_name = case flavour of
                       UntypedExpSplice  -> expQTyConName
                       UntypedPatSplice  -> patQTyConName
                       UntypedTypeSplice -> typeQTyConName
                       UntypedDeclSplice -> decsQTyConName
    what = case flavour of
                  UntypedExpSplice  -> "expression"
                  UntypedPatSplice  -> "pattern"
                  UntypedTypeSplice -> "type"
                  UntypedDeclSplice -> "declarations"
    is_decl = case flavour of
                 UntypedDeclSplice -> True
                 _                 -> False

------------------
makePending :: UntypedSpliceFlavour
            -> HsSplice GhcRn
            -> PendingRnSplice
makePending flavour (HsUntypedSplice _ _ n e)
  = PendingRnSplice flavour n e
makePending flavour (HsQuasiQuote _ n quoter q_span quote)
  = PendingRnSplice flavour n (mkQuasiQuoteExpr flavour quoter q_span quote)
makePending _ splice@(HsTypedSplice {})
  = pprPanic "makePending" (ppr splice)
makePending _ splice@(HsSpliced {})
  = pprPanic "makePending" (ppr splice)

------------------
mkQuasiQuoteExpr :: UntypedSpliceFlavour -> Name -> SrcSpan -> FastString
                 -> LHsExpr GhcRn
-- Return the expression (quoter "...quote...")
-- which is what we must run in a quasi-quote
mkQuasiQuoteExpr flavour quoter q_span quote
  = L q_span $ HsApp noExtField (L q_span
             $ HsApp noExtField (L q_span (HsVar noExtField (L q_span quote_selector)))
                                quoterExpr)
                    quoteExpr
  where
    quoterExpr = L q_span $! HsVar noExtField $! (L q_span quoter)
    quoteExpr  = L q_span $! HsLit noExtField $! HsString NoSourceText quote
    quote_selector = case flavour of
                       UntypedExpSplice  -> quoteExpName
                       UntypedPatSplice  -> quotePatName
                       UntypedTypeSplice -> quoteTypeName
                       UntypedDeclSplice -> quoteDecName

---------------------
rnSplice :: HsSplice GhcPs -> WriterT FreeVars RnM (HsSplice GhcRn)
-- Not exported...used for all
rnSplice (HsTypedSplice x hasParen splice_name expr)
  = do  { loc <- getSrcSpanM
        ; n' <- lift $ newLocalBndrRn (L loc splice_name)
        ; HsTypedSplice x hasParen n' <$> rnLExpr expr }

rnSplice (HsUntypedSplice x hasParen splice_name expr)
  = do  { loc <- getSrcSpanM
        ; n' <- lift $ newLocalBndrRn (L loc splice_name)
        ; HsUntypedSplice x hasParen n' <$> rnLExpr expr }

rnSplice (HsQuasiQuote x splice_name quoter q_loc quote)
  = WriterT
  [ (HsQuasiQuote x splice_name' quoter' q_loc quote, unitFV quoter')
  | loc <- getSrcSpanM
  , splice_name' <- newLocalBndrRn (L loc splice_name)
    -- Rename the quoter; akin to the HsVar case of rnExpr
  , quoter' <- lookupOccRn quoter
  , this_mod <- getModule
  , () <- when (nameIsLocalOrFrom this_mod quoter') $ checkThLocalName quoter' ]

rnSplice splice@(HsSpliced {}) = pprPanic "rnSplice" (ppr splice)

---------------------
rnSpliceExpr :: HsSplice GhcPs -> WriterT FreeVars RnM (HsExpr GhcRn)
rnSpliceExpr = rnSpliceGen run_expr_splice pend_expr_splice
  where
    pend_expr_splice :: HsSplice GhcRn -> (PendingRnSplice, HsExpr GhcRn)
    pend_expr_splice rn_splice
        = (makePending UntypedExpSplice rn_splice, HsSpliceE noExtField rn_splice)

    run_expr_splice :: HsSplice GhcRn -> WriterT FreeVars RnM (HsExpr GhcRn)
    run_expr_splice rn_splice
      | isTypedSplice rn_splice   -- Run it later, in the type checker
      = do {  -- Ugh!  See Note [Splices] above
             traceRn "rnSpliceExpr: typed expression splice" mempty
           ; lcl_rdr <- getLocalRdrEnv
           ; gbl_rdr <- getGlobalRdrEnv
           ; let gbl_names = mkNameSet [gre_name gre | gre <- globalRdrEnvElts gbl_rdr
                                                     , isLocalGRE gre]
                 lcl_names = mkNameSet (localRdrEnvElts lcl_rdr)

           ; HsSpliceE noExtField rn_splice <$ tell (plusFV lcl_names gbl_names) }

      | otherwise  -- Run it here, see Note [Running splices in the Renamer]
      = do { traceRn "rnSpliceExpr: untyped expression splice" mempty
           ; (rn_expr, mod_finalizers) <-
                lift $ runRnSplice UntypedExpSplice runMetaE ppr rn_splice
           ; HsPar noExtField .
             fmap ( HsSpliceE noExtField
                  . HsSpliced noExtField (ThModFinalizers mod_finalizers)
                  . HsSplicedExpr ) <$> checkNoErrs `mapWriterT` rnLExpr rn_expr
             -- See Note [Delaying modFinalizers in untyped splices].
           }

{- Note [Running splices in the Renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Splices used to be run in the typechecker, which led to (#4364). Since the
renamer must decide which expressions depend on which others, and it cannot
reliably do this for arbitrary splices, we used to conservatively say that
splices depend on all other expressions in scope. Unfortunately, this led to
the problem of cyclic type declarations seen in (#4364). Instead, by
running splices in the renamer, we side-step the problem of determining
dependencies: by the time the dependency analysis happens, any splices have
already been run, and expression dependencies can be determined as usual.

However, see (#9813), for an example where we would like to run splices
*after* performing dependency analysis (that is, after renaming). It would be
desirable to typecheck "non-splicy" expressions (those expressions that do not
contain splices directly or via dependence on an expression that does) before
"splicy" expressions, such that types/expressions within the same declaration
group would be available to `reify` calls, for example consider the following:

> module M where
>   data D = C
>   f = 1
>   g = $(mapM reify ['f, 'D, ''C] ...)

Compilation of this example fails since D/C/f are not in the type environment
and thus cannot be reified as they have not been typechecked by the time the
splice is renamed and thus run.

These requirements are at odds: we do not want to run splices in the renamer as
we wish to first determine dependencies and typecheck certain expressions,
making them available to reify, but cannot accurately determine dependencies
without running splices in the renamer!

Indeed, the conclusion of (#9813) was that it is not worth the complexity
to try and
 a) implement and maintain the code for renaming/typechecking non-splicy
    expressions before splicy expressions,
 b) explain to TH users which expressions are/not available to reify at any
    given point.

-}

{- Note [Delaying modFinalizers in untyped splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When splices run in the renamer, 'reify' does not have access to the local
type environment (#11832, [1]).

For instance, in

> let x = e in $(reify (mkName "x") >>= runIO . print >> [| return () |])

'reify' cannot find @x@, because the local type environment is not yet
populated. To address this, we allow 'reify' execution to be deferred with
'addModFinalizer'.

> let x = e in $(do addModFinalizer (reify (mkName "x") >>= runIO . print)
                    [| return () |]
                )

The finalizer is run with the local type environment when type checking is
complete.

Since the local type environment is not available in the renamer, we annotate
the tree at the splice point [2] with @HsSpliceE (HsSpliced finalizers e)@ where
@e@ is the result of splicing and @finalizers@ are the finalizers that have been
collected during evaluation of the splice [3]. In our example,

> HsLet
>   (x = e)
>   (HsSpliceE $ HsSpliced [reify (mkName "x") >>= runIO . print]
>                          (HsSplicedExpr $ return ())
>   )

When the typechecker finds the annotation, it inserts the finalizers in the
global environment and exposes the current local environment to them [4, 5, 6].

> addModFinalizersWithLclEnv [reify (mkName "x") >>= runIO . print]

References:

[1] https://gitlab.haskell.org/ghc/ghc/wikis/template-haskell/reify
[2] 'rnSpliceExpr'
[3] 'GHC.Tc.Gen.Splice.qAddModFinalizer'
[4] 'GHC.Tc.Gen.Expr.tcExpr' ('HsSpliceE' ('HsSpliced' ...))
[5] 'GHC.Tc.Gen.HsType.tc_hs_type' ('HsSpliceTy' ('HsSpliced' ...))
[6] 'GHC.Tc.Gen.Pat.tc_pat' ('SplicePat' ('HsSpliced' ...))

-}

----------------------
rnSpliceType :: HsSplice GhcPs -> WriterT FreeVars RnM (HsType GhcRn)
rnSpliceType = rnSpliceGen (WriterT . run_type_splice) pend_type_splice
  where
    pend_type_splice rn_splice
       = ( makePending UntypedTypeSplice rn_splice
         , HsSpliceTy noExtField rn_splice)

    run_type_splice rn_splice
      = do { traceRn "rnSpliceType: untyped type splice" mempty
           ; (hs_ty2, mod_finalizers) <-
                runRnSplice UntypedTypeSplice runMetaT ppr rn_splice
           ; checkNoErrs $ runWriterT
              [ -- Wrap the result of the splice in parens so that we don't
                -- lose the outermost location set by runQuasiQuote (#7918)
                HsParTy noExtField
                  $ HsSpliceTy noExtField
                  . HsSpliced noExtField (ThModFinalizers mod_finalizers)
                  . HsSplicedTy <$> hs_ty3
              | let doc = SpliceTypeCtx hs_ty2
              , hs_ty3 <- rnLHsType doc hs_ty2 ]
                                    -- checkNoErrs: see Note [Renamer errors]
             -- See Note [Delaying modFinalizers in untyped splices].
      }

{- Note [Partial Type Splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial Type Signatures are partially supported in TH type splices: only
anonymous wild cards are allowed.

  -- ToDo: SLPJ says: I don't understand all this

Normally, named wild cards are collected before renaming a (partial) type
signature. However, TH type splices are run during renaming, i.e. after the
initial traversal, leading to out of scope errors for named wild cards. We
can't just extend the initial traversal to collect the named wild cards in TH
type splices, as we'd need to expand them, which is supposed to happen only
once, during renaming.

Similarly, the extra-constraints wild card is handled right before renaming
too, and is therefore also not supported in a TH type splice. Another reason
to forbid extra-constraints wild cards in TH type splices is that a single
signature can contain many TH type splices, whereas it mustn't contain more
than one extra-constraints wild card. Enforcing would this be hard the way
things are currently organised.

Anonymous wild cards pose no problem, because they start out without names and
are given names during renaming. These names are collected right after
renaming. The names generated for anonymous wild cards in TH type splices will
thus be collected as well.

For more details about renaming wild cards, see GHC.Rename.HsType.rnHsSigWcType

Note that partial type signatures are fully supported in TH declaration
splices, e.g.:

     [d| foo :: _ => _
         foo x y = x == y |]

This is because in this case, the partial type signature can be treated as a
whole signature, instead of as an arbitrary type.

-}


----------------------
-- | Rename a splice pattern. See Note [rnSplicePat]
rnSplicePat :: HsSplice GhcPs -> WriterT FreeVars RnM (Either (Pat GhcPs) (Pat GhcRn))
rnSplicePat = rnSpliceGen run_pat_splice pend_pat_splice
  where
    pend_pat_splice :: HsSplice GhcRn ->
                       (PendingRnSplice, Either b (Pat GhcRn))
    pend_pat_splice rn_splice
      = (makePending UntypedPatSplice rn_splice
        , Right (SplicePat noExtField rn_splice))

    run_pat_splice :: HsSplice GhcRn ->
                      WriterT FreeVars RnM (Either (Pat GhcPs) (Pat GhcRn))
    run_pat_splice rn_splice = lift $
      [ -- Wrap the result of the quasi-quoter in parens so that we don't
        -- lose the outermost location set by runQuasiQuote (#7918)
        Left $ ParPat noExtField $
        SplicePat noExtField . HsSpliced noExtField (ThModFinalizers mod_finalizers) .
        HsSplicedPat <$>
        pat
      | () <- traceRn "rnSplicePat: untyped pattern splice" mempty
      , (pat, mod_finalizers) <- runRnSplice UntypedPatSplice runMetaP ppr rn_splice
        -- See Note [Delaying modFinalizers in untyped splices].
      ]

----------------------
rnSpliceDecl :: SpliceDecl GhcPs -> WriterT FreeVars RnM (SpliceDecl GhcRn)
rnSpliceDecl (SpliceDecl _ (L loc splice) flg)
  = rnSpliceGen run_decl_splice pend_decl_splice splice
  where
    pend_decl_splice rn_splice
       = ( makePending UntypedDeclSplice rn_splice
         , SpliceDecl noExtField (L loc rn_splice) flg)

    run_decl_splice rn_splice  = pprPanic "rnSpliceDecl" (ppr rn_splice)

rnTopSpliceDecls :: HsSplice GhcPs -> WriterT FreeVars RnM [LHsDecl GhcPs]
-- Declaration splice at the very top level of the module
rnTopSpliceDecls splice = WriterT
  [ (decls, fvs)
  | () <- checkTopSpliceAllowed splice
  , (rn_splice, fvs) <- checkNoErrs $
                               setStage (Splice Untyped) $
                               runWriterT $ rnSplice splice
    -- As always, be sure to checkNoErrs above lest we end up with
    -- holes making it to typechecking, hence #12584.
    --
    -- Note that we cannot call checkNoErrs for the whole duration of rnTopSpliceDecls. The
    -- reason is that checkNoErrs changes the local environment to temporarily contain a new
    -- reference to store errors, and add_mod_finalizers would
    -- cause this reference to be stored after checkNoErrs finishes.
    -- This is checked by test TH_finalizer.
  , () <- traceRn "rnTopSpliceDecls: untyped declaration splice" mempty
  , (decls, mod_finalizers) <- checkNoErrs $
               runRnSplice UntypedDeclSplice runMetaD ppr_decls rn_splice
  , () <- add_mod_finalizers_now mod_finalizers ]
   where
     ppr_decls :: [LHsDecl GhcPs] -> SDoc
     ppr_decls ds = vcat (map ppr ds)

     -- Adds finalizers to the global environment instead of delaying them
     -- to the type checker.
     --
     -- Declaration splices do not have an interesting local environment so
     -- there is no point in delaying them.
     --
     -- See Note [Delaying modFinalizers in untyped splices].
     add_mod_finalizers_now :: [ForeignRef (TH.Q ())] -> RnM ()
     add_mod_finalizers_now []             = return ()
     add_mod_finalizers_now mod_finalizers = do
       th_modfinalizers_var <- tcg_th_modfinalizers <$> getGblEnv
       env <- getLclEnv
       updMutVar th_modfinalizers_var $ (:) (env, ThModFinalizers mod_finalizers)


{-
Note [rnSplicePat]
~~~~~~~~~~~~~~~~~~
Renaming a pattern splice is a bit tricky, because we need the variables
bound in the pattern to be in scope in the RHS of the pattern. This scope
management is effectively done by using continuation-passing style in
GHC.Rename.Pat, through the CpsRn monad. We don't wish to be in that monad here
(it would create import cycles and generally conflict with renaming other
splices), so we really want to return a (Pat RdrName) -- the result of
running the splice -- which can then be further renamed in GHC.Rename.Pat, in
the CpsRn monad.

The problem is that if we're renaming a splice within a bracket, we
*don't* want to run the splice now. We really do just want to rename
it to an HsSplice Name. Of course, then we can't know what variables
are bound within the splice. So we accept any unbound variables and
rename them again when the bracket is spliced in.  If a variable is brought
into scope by a pattern splice all is fine.  If it is not then an error is
reported.

In any case, when we're done in rnSplicePat, we'll either have a
Pat RdrName (the result of running a top-level splice) or a Pat Name
(the renamed nested splice). Thus, the awkward return type of
rnSplicePat.
-}

spliceCtxt :: HsSplice GhcPs -> SDoc
spliceCtxt splice = hang (text "In the" <+> what) 2 (ppr splice)
  where
    what = case splice of
             HsUntypedSplice {} -> text "untyped splice:"
             HsTypedSplice   {} -> text "typed splice:"
             HsQuasiQuote    {} -> text "quasi-quotation:"
             HsSpliced       {} -> text "spliced expression:"

-- | The splice data to be logged
data SpliceInfo
  = SpliceInfo
    { spliceDescription  :: String
    , spliceSource       :: Maybe (LHsExpr GhcRn) -- Nothing <=> top-level decls
                                                  --        added by addTopDecls
    , spliceIsDecl       :: Bool    -- True <=> put the generate code in a file
                                    --          when -dth-dec-file is on
    , spliceGenerated    :: SDoc
    }
        -- Note that 'spliceSource' is *renamed* but not *typechecked*
        -- Reason (a) less typechecking crap
        --        (b) data constructors after type checking have been
        --            changed to their *wrappers*, and that makes them
        --            print always fully qualified

-- | outputs splice information for 2 flags which have different output formats:
-- `-ddump-splices` and `-dth-dec-file`
traceSplice :: SpliceInfo -> TcM ()
traceSplice (SpliceInfo { spliceDescription = sd, spliceSource = mb_src
                        , spliceGenerated = gen, spliceIsDecl = is_decl })
  = do { loc <- case mb_src of
                   Nothing        -> getSrcSpanM
                   Just (L loc _) -> return loc
       ; traceOptTcRn Opt_D_dump_splices (spliceDebugDoc loc)

       ; when is_decl $  -- Raw material for -dth-dec-file
         do { dflags <- getDynFlags
            ; liftIO $ dumpIfSet_dyn_printer alwaysQualify dflags Opt_D_th_dec_file
                                             "" FormatHaskell (spliceCodeDoc loc) } }
  where
    -- `-ddump-splices`
    spliceDebugDoc :: SrcSpan -> SDoc
    spliceDebugDoc loc
      = let code = case mb_src of
                     Nothing -> ending
                     Just e  -> nest 2 (ppr (stripParensLHsExpr e)) : ending
            ending = [ text "======>", nest 2 gen ]
        in  hang (ppr loc <> colon <+> text "Splicing" <+> text sd) 2 (sep code)

    -- `-dth-dec-file`
    spliceCodeDoc :: SrcSpan -> SDoc
    spliceCodeDoc loc
      = vcat [ text "--" <+> ppr loc <> colon <+> text "Splicing" <+> text sd, gen ]

illegalTypedSplice :: SDoc
illegalTypedSplice = text "Typed splices may not appear in untyped brackets"

illegalUntypedSplice :: SDoc
illegalUntypedSplice = text "Untyped splices may not appear in typed brackets"

checkThLocalName :: Name -> RnM ()
checkThLocalName name
  | isUnboundName name   -- Do not report two errors for
  = return ()            --   $(not_in_scope args)

  | otherwise
  = do  { traceRn "checkThLocalName" (ppr name)
        ; getStageAndBindLevel name >>= \ case
            { Nothing -> pure () -- Not a locally-bound thing
            ; Just (top_lvl, bind_lvl, use_stage) ->
    do  { let use_lvl = thLevel use_stage
        ; checkWellStaged (quotes (ppr name)) bind_lvl use_lvl
        ; traceRn "checkThLocalName" (ppr name <+> ppr bind_lvl
                                               <+> ppr use_stage
                                               <+> ppr use_lvl)
        ; checkCrossStageLifting top_lvl bind_lvl use_stage use_lvl name } } }

--------------------------------------
checkCrossStageLifting :: TopLevelFlag -> ThLevel -> ThStage -> ThLevel
                       -> Name -> TcM ()
-- We are inside brackets, and (use_lvl > bind_lvl)
-- Now we must check whether there's a cross-stage lift to do
-- Examples   \x -> [| x |]
--            [| map |]
--
-- This code is similar to checkCrossStageLifting in GHC.Tc.Gen.Expr, but
-- this is only run on *untyped* brackets.

checkCrossStageLifting top_lvl bind_lvl use_stage use_lvl name
  | Brack _ (RnPendingUntyped ps_var) <- use_stage   -- Only for untyped brackets
  , use_lvl > bind_lvl                               -- Cross-stage condition
  = check_cross_stage_lifting top_lvl name ps_var
  | otherwise
  = return ()

check_cross_stage_lifting :: TopLevelFlag -> Name -> TcRef [PendingRnSplice] -> TcM ()
check_cross_stage_lifting top_lvl name ps_var
  | isTopLevel top_lvl
        -- Top-level identifiers in this module,
        -- (which have External Names)
        -- are just like the imported case:
        -- no need for the 'lifting' treatment
        -- E.g.  this is fine:
        --   f x = x
        --   g y = [| f 3 |]
  = when (isExternalName name) (keepAlive name)
    -- See Note [Keeping things alive for Template Haskell]

  | otherwise
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [| h x |]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the SplicePointName, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same SplicePointName, but that doesn't
        -- matter, although it's a mite untidy.
    do  { traceRn "checkCrossStageLifting" (ppr name)

          -- Construct the (lift x) expression
        ; let lift_expr   = nlHsApp (nlHsVar liftName) (nlHsVar name)
              pend_splice = PendingRnSplice UntypedExpSplice name lift_expr

          -- Update the pending splices
        ; updMutVar ps_var (pend_splice :) }

{-
Note [Keeping things alive for Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f x = x+1
  g y = [| f 3 |]

Here 'f' is referred to from inside the bracket, which turns into data
and mentions only f's *name*, not 'f' itself. So we need some other
way to keep 'f' alive, lest it get dropped as dead code.  That's what
keepAlive does. It puts it in the keep-alive set, which subsequently
ensures that 'f' stays as a top level binding.

This must be done by the renamer, not the type checker (as of old),
because the type checker doesn't typecheck the body of untyped
brackets (#8540).

A thing can have a bind_lvl of outerLevel, but have an internal name:
   foo = [d| op = 3
             bop = op + 1 |]
Here the bind_lvl of 'op' is (bogusly) outerLevel, even though it is
bound inside a bracket.  That is because we don't even record
binding levels for top-level things; the binding levels are in the
LocalRdrEnv.

So the occurrence of 'op' in the rhs of 'bop' looks a bit like a
cross-stage thing, but it isn't really.  And in fact we never need
to do anything here for top-level bound things, so all is fine, if
a bit hacky.

For these chaps (which have Internal Names) we don't want to put
them in the keep-alive set.

Note [Quoting names]
~~~~~~~~~~~~~~~~~~~~
A quoted name 'n is a bit like a quoted expression [| n |], except that we
have no cross-stage lifting (c.f. GHC.Tc.Gen.Expr.thBrackId).  So, after incrementing
the use-level to account for the brackets, the cases are:

        bind > use                      Error
        bind = use+1                    OK
        bind < use
                Imported things         OK
                Top-level things        OK
                Non-top-level           Error

where 'use' is the binding level of the 'n quote. (So inside the implied
bracket the level would be use+1.)

Examples:

  f 'map        -- OK; also for top-level defns of this module

  \x. f 'x      -- Not ok (bind = 1, use = 1)
                -- (whereas \x. f [| x |] might have been ok, by
                --                               cross-stage lifting

  \y. [| \x. $(f 'y) |] -- Not ok (bind =1, use = 1)

  [| \x. $(f 'x) |]     -- OK (bind = 2, use = 1)
-}
