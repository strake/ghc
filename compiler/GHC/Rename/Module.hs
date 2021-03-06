{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Main pass of renamer
-}

module GHC.Rename.Module (
        rnSrcDecls, findSplice
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Rename.Expr( rnLExpr )
import {-# SOURCE #-} GHC.Rename.Splice ( rnSpliceDecl, rnTopSpliceDecls )

import GHC.Hs
import GHC.Types.FieldLabel
import GHC.Types.Name.Reader
import GHC.Rename.HsType
import GHC.Rename.Bind
import GHC.Rename.Env
import GHC.Rename.Utils ( HsDocContext(..), bindLocalNames
                        , checkDupRdrNames, bindLocalNamesFV, bindLocalNamesFVW
                        , checkShadowedRdrNames, warnUnusedTypePatterns
                        , newLocalBndrsRn, withHsDocContext )
import GHC.Rename.Unbound ( mkUnboundName, notInScopeErr )
import GHC.Rename.Names
import GHC.Tc.Gen.Annotation ( annCtxt )
import GHC.Tc.Utils.Monad

import GHC.Types.ForeignCall ( CCallTarget(..) )
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Avail
import GHC.Utils.Outputable
import GHC.Data.Bag
import GHC.Types.Basic  ( pprRuleName, TypeOrKind(..) )
import GHC.Data.FastString
import GHC.Types.SrcLoc as SrcLoc
import GHC.Driver.Session
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Driver.Env ( HscEnv, hsc_dflags )
import GHC.Data.List.SetOps ( findDupsEq, removeDups, equivClasses )
import GHC.Data.Graph.Directed ( SCC, flattenSCC, flattenSCCs, Node(..)
                               , stronglyConnCompFromEdgedVerticesUniq )
import GHC.Types.Unique.Set
import GHC.Data.OrdList
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Writer ( WriterT (..), listen, writer )
import Data.Foldable ( toList )
import Data.Functor.Reader.Class
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe ( isNothing, isJust )
import qualified Data.Set as Set ( difference, fromList )
import Lens.Micro ( _1 )

{- | @rnSourceDecl@ "renames" declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:

* Checks that tyvars are used properly. This includes checking
  for undefined tyvars, and tyvars in contexts that are ambiguous.
  (Some of this checking has now been moved to module @TcMonoType@,
  since we don't have functional dependency information at this point.)

* Checks that all variable occurrences are defined.

* Checks the @(..)@ etc constraints in the export list.

Brings the binders of the group into scope in the appropriate places;
does NOT assume that anything is in scope already
-}
rnSrcDecls :: HsGroup GhcPs -> RnM (TcGblEnv, HsGroup GhcRn)
-- Rename a top-level HsGroup; used for normal source files *and* hs-boot files
rnSrcDecls group@(HsGroup { hs_valds   = val_decls,
                            hs_splcds  = splice_decls,
                            hs_tyclds  = tycl_decls,
                            hs_derivds = deriv_decls,
                            hs_fixds   = fix_decls,
                            hs_warnds  = warn_decls,
                            hs_annds   = ann_decls,
                            hs_fords   = foreign_decls,
                            hs_defds   = default_decls,
                            hs_ruleds  = rule_decls,
                            hs_docs    = docs }) = do
  { -- (A) Process the top-level fixity declarations, creating a mapping from
    --     FastStrings to FixItems. Also checks for duplicates.
    --     See Note [Top-level fixity signatures in an HsGroup] in GHC.Hs.Decls
    local_fix_env <- makeMiniFixityEnv $ hsGroupTopLevelFixitySigs group

  ; -- (B) Bring top level binders (and their fixities) into scope,
    --     *except* for the value bindings, which get done in step (D)
    --     with collectHsIdBinders. However *do* include
    --
    --        * Class ops, data constructors, and record fields,
    --          because they do not have value declarations.
    --
    --        * For hs-boot files, include the value signatures
    --          Again, they have no value declarations
    --
    (tc_envs, tc_bndrs) <- getLocalNonValBinders local_fix_env group

  ; setEnvs tc_envs $ do

  { failIfErrsM -- No point in continuing if (say) we have duplicate declarations

  ; -- (D1) Bring pattern synonyms into scope.
    --      Need to do this before (D2) because rnTopBindsLHS
    --      looks up those pattern synonyms (#9889)

    extendPatSynEnv val_decls local_fix_env $ \pat_syn_bndrs -> do

  { -- (D2) Rename the left-hand sides of the value bindings.
    --     This depends on everything from (B) being in scope.
    --     It uses the fixity env from (A) to bind fixities for view patterns.
    new_lhs <- rnTopBindsLHS local_fix_env val_decls

  ; -- Bind the LHSes (and their fixities) in the global rdr environment
    let { id_bndrs = collectHsIdBinders new_lhs }  -- Excludes pattern-synonym binders
                                                   -- They are already in scope
  ; traceRn "rnSrcDecls" (ppr id_bndrs)
  ; tc_envs <- extendGlobalRdrEnvRn (map avail id_bndrs) local_fix_env
  ; setEnvs tc_envs $ do
  { --  Now everything is in scope, as the remaining renaming assumes.

    -- (E) Rename type and class decls
    --     (note that value LHSes need to be in scope for default methods)
    --
    -- You might think that we could build proper def/use information
    -- for type and class declarations, but they can be involved
    -- in mutual recursion across modules, and we only do the SCC
    -- analysis for them in the type checker.
    -- So we content ourselves with gathering uses only; that
    -- means we'll only report a declaration as unused if it isn't
    -- mentioned at all.  Ah well.
    traceRn "Start rnTyClDecls" (ppr tycl_decls)
  ; (rn_tycl_decls, src_fvs1) <- runWriterT $ rnTyClDecls tycl_decls

  ; -- (F) Rename Value declarations right-hand sides
    traceRn "Start rnmono" mempty
  ; let { val_bndr_set = mkNameSet id_bndrs `unionNameSet` mkNameSet pat_syn_bndrs }
  ; is_boot <- tcIsHsBootOrSig
  ; (rn_val_decls, bind_dus) <-
    -- For an hs-boot, use tc_bndrs (which collects how we're renamed
    -- signatures), since val_bndr_set is empty (there are no x = ...
    -- bindings in an hs-boot.)
    bool (rnValBindsRHS (TopSigCtxt val_bndr_set)) (rnTopBindsBoot tc_bndrs) is_boot new_lhs
  ; traceRn "finish rnmono" (ppr rn_val_decls)

  ; -- (G) Rename Fixity and deprecations

    -- Rename fixity declarations and error if we try to
    -- fix something from another module (duplicates were checked in (A))
    let all_bndrs = tc_bndrs `unionNameSet` val_bndr_set
  ; rn_fix_decls <- traverse (traverse (rnSrcFixityDecl (TopSigCtxt all_bndrs))) fix_decls

  ; -- Rename deprec decls;
    -- check for duplicates and ensure that deprecated things are defined locally
    -- at the moment, we don't keep these around past renaming
    rn_warns <- rnSrcWarnDecls all_bndrs warn_decls

  ; -- (H) Rename Everything else

    (rn_group, src_fvs2) <- runWriterT
      [ HsGroup
          { hs_ext     = noExtField
          , hs_valds   = rn_val_decls
          , hs_splcds  = rn_splice_decls
          , hs_tyclds  = rn_tycl_decls
          , hs_derivds = rn_deriv_decls
          , hs_fixds   = rn_fix_decls
          , hs_warnds  = [] -- warns are returned in the tcg_env (see below) not in the HsGroup
          , hs_fords  = rn_foreign_decls
          , hs_annds  = rn_ann_decls
          , hs_defds  = rn_default_decls
          , hs_ruleds = rn_rule_decls
          , hs_docs   = docs }
      | rn_rule_decls <- setXOptM LangExt.ScopedTypeVariables $ (traverse . wrapLocM) rnHsRuleDecls rule_decls
        -- Inside RULES, scoped type variables are on
      , rn_foreign_decls <- (traverse . wrapLocM) rnHsForeignDecl foreign_decls
      , rn_ann_decls <- (traverse . wrapLocM) rnAnnDecl ann_decls
      , rn_default_decls <- (traverse . wrapLocM) rnDefaultDecl default_decls
      , rn_deriv_decls <- (traverse . wrapLocM) rnSrcDerivDecl deriv_decls
      , rn_splice_decls <- (traverse . wrapLocM) rnSpliceDecl splice_decls
      ]

  ; last_tcg_env <- getGblEnv
  ; -- (I) Compute the results and return
    let tcf_bndrs = hsTyClForeignBinders rn_tycl_decls (hs_fords rn_group)
        other_def  = (Just (mkNameSet tcf_bndrs), emptyNameSet)
        other_fvs  = plusFVs [src_fvs1, src_fvs2] ;
                -- It is tiresome to gather the binders from type and class decls

        src_dus = unitOL other_def `plusDU` bind_dus `plusDU` usesOnly other_fvs ;
                -- Instance decls may have occurrences of things bound in bind_dus
                -- so we must put other_fvs last

        final_tcg_env = over tcg_warnsL (`plusWarns` rn_warns) . over tcg_dusL (`plusDU` src_dus) $ last_tcg_env
                        -- we return the deprecs in the env, not in the HsGroup above
  ; (final_tcg_env, rn_group) <$ traceRn "finish rnSrc" (ppr rn_group) <* traceRn "finish Dus" (ppr src_dus) }}}}

{-
*********************************************************
*                                                       *
        Source-code deprecations declarations
*                                                       *
*********************************************************

Check that the deprecated names are defined, are defined locally, and
that there are no duplicate deprecations.

It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.
-}

-- checks that the deprecations are defined locally, and that there are no duplicates
rnSrcWarnDecls :: NameSet -> [LWarnDecls GhcPs] -> RnM Warnings
rnSrcWarnDecls _ [] = pure NoWarnings
rnSrcWarnDecls bndr_set decls' =
  [ WarnSome (join pairs_s)
    -- check for duplicates
  | () <- for_ warn_rdr_dups \ ~(L loc rdr :| lrdr':_) -> addErrAt loc (dupWarnDecl lrdr' rdr)
  , pairs_s <- traverse (addLocM rn_deprec) decls ]
 where
   decls = concatMap (wd_warnings . unLoc) decls'

   sig_ctxt = TopSigCtxt bndr_set

   rn_deprec (Warning _ rdr_names txt) =
       -- ensures that the names are defined locally
     [ [(rdrNameOcc rdr, txt) | (rdr, _) <- names]
     | names <- concatMapM (lookupLocalTcNames sig_ctxt what . unLoc) rdr_names ]

   what = text "deprecation"

   warn_rdr_dups = findDupRdrNames $ decls >>= \ (L _ (Warning _ ns _)) -> ns

findDupRdrNames :: [Located RdrName] -> [NonEmpty (Located RdrName)]
findDupRdrNames = findDupsEq ((==) `on` rdrNameOcc . unLoc)

-- look for duplicates among the OccNames;
-- we check that the names are defined above
-- invt: the lists returned by findDupsEq always have at least two elements

dupWarnDecl :: Located RdrName -> RdrName -> SDoc
-- Located RdrName -> DeprecDecl RdrName -> SDoc
dupWarnDecl d rdr_name
  = vcat [text "Multiple warning declarations for" <+> quotes (ppr rdr_name),
          text "also at " <+> ppr (getLoc d)]

{-
*********************************************************
*                                                      *
\subsection{Annotation declarations}
*                                                      *
*********************************************************
-}

rnAnnDecl :: AnnDecl GhcPs -> WriterT FreeVars RnM (AnnDecl GhcRn)
rnAnnDecl ann@(HsAnnotation _ s provenance expr) = addErrCtxt (annCtxt ann) $
    HsAnnotation noExtField s <$> rnAnnProvenance provenance <*> setStage (Splice Untyped) (rnLExpr expr)

rnAnnProvenance :: AnnProvenance RdrName
                -> WriterT FreeVars RnM (AnnProvenance Name)
rnAnnProvenance provenance = WriterT $ traverse lookupTopBndrRn provenance <???> \ provenance' ->
    (provenance', foldMap unitFV (annProvenanceName_maybe provenance'))

{-
*********************************************************
*                                                      *
\subsection{Default declarations}
*                                                      *
*********************************************************
-}

rnDefaultDecl :: DefaultDecl GhcPs -> WriterT FreeVars RnM (DefaultDecl GhcRn)
rnDefaultDecl (DefaultDecl _ tys) = DefaultDecl noExtField <$> traverse (rnLHsType DefaultDeclCtx) tys

{-
*********************************************************
*                                                      *
\subsection{Foreign declarations}
*                                                      *
*********************************************************
-}

rnHsForeignDecl :: ForeignDecl GhcPs -> WriterT FreeVars RnM (ForeignDecl GhcRn)
rnHsForeignDecl ForeignImport { fd_name = name, fd_sig_ty = ty, fd_fi = spec } =
  [ ForeignImport { fd_i_ext = noExtField, fd_name = name', fd_sig_ty = ty', fd_fi = spec' }
  | topEnv :: HscEnv <- getTopEnv
  , name' <- lift $ wrapLocM lookupTopBndrRn name
  , ty' <- rnHsSigType (ForeignDeclCtx name) TypeLevel Nothing ty

    -- Mark any PackageTarget style imports as coming from the current package
  , let unitId = homeUnit $ hsc_dflags topEnv
        spec'  = patchForeignImport unitId spec ]

rnHsForeignDecl ForeignExport { fd_name = name, fd_sig_ty = ty, fd_fe = spec } = WriterT
  [ ( ForeignExport { fd_e_ext = noExtField, fd_name = name', fd_sig_ty = ty', fd_fe = spec }
    , fvs `addOneFV` unLoc name' )
  | name' <- wrapLocM lookupOccRn name
  , (ty', fvs) <- runWriterT $ rnHsSigType (ForeignDeclCtx name) TypeLevel Nothing ty
    -- NB: a foreign export is an *occurrence site* for name, so we add it to the free-variable
    --     list.  It might, for example, be imported from another module.
  ]

-- | For Windows DLLs we need to know what packages imported symbols are from
--      to generate correct calls. Imported symbols are tagged with the current
--      package, so if they get inlined across a package boundary we'll still
--      know where they're from.
--
patchForeignImport :: Unit -> ForeignImport -> ForeignImport
patchForeignImport unit (CImport cconv safety fs spec src)
        = CImport cconv safety fs (patchCImportSpec unit spec) src

patchCImportSpec :: Unit -> CImportSpec -> CImportSpec
patchCImportSpec unit spec = case spec of
        CFunction callTarget    -> CFunction $ patchCCallTarget unit callTarget
        _                       -> spec

patchCCallTarget :: Unit -> CCallTarget -> CCallTarget
patchCCallTarget unit callTarget = case callTarget of
  StaticTarget src label Nothing isFun
                              -> StaticTarget src label (Just unit) isFun
  _                           -> callTarget

{-
*********************************************************
*                                                      *
\subsection{Instance declarations}
*                                                      *
*********************************************************
-}

rnSrcInstDecl :: InstDecl GhcPs -> WriterT FreeVars RnM (InstDecl GhcRn)
rnSrcInstDecl TyFamInstD { tfid_inst = tfi } =
  [ TyFamInstD { tfid_ext = noExtField, tfid_inst = tfi' }
  | tfi' <- rnTyFamInstDecl NonAssocTyFamEqn tfi ]

rnSrcInstDecl DataFamInstD { dfid_inst = dfi } =
  [ DataFamInstD { dfid_ext = noExtField, dfid_inst = dfi' }
  | dfi' <- rnDataFamInstDecl NonAssocTyFamEqn dfi ]

rnSrcInstDecl ClsInstD { cid_inst = cid } =
  [ ClsInstD { cid_d_ext = noExtField, cid_inst = cid' }
  | () <- traceRn "rnSrcIstDecl {" (ppr cid)
  , cid' <- rnClsInstDecl cid
  , () <- traceRn "rnSrcIstDecl end }" mempty ]

rnClsInstDecl :: ClsInstDecl GhcPs -> WriterT FreeVars RnM (ClsInstDecl GhcRn)
rnClsInstDecl ClsInstDecl
  { cid_poly_ty = inst_ty, cid_binds = mbinds, cid_sigs = uprags
  , cid_tyfam_insts = ats, cid_overlap_mode = oflag, cid_datafam_insts = adts } = WriterT
  [ (ClsInstDecl
      { cid_ext = noExtField, cid_poly_ty = inst_ty', cid_binds = mbinds', cid_sigs = uprags'
      , cid_tyfam_insts = ats', cid_overlap_mode = oflag, cid_datafam_insts = adts' }, all_fvs)
  | (inst_ty', inst_fvs) <- runWriterT $
        rnHsSigType (GenericCtx $ text "an instance declaration") TypeLevel inf_err inst_ty
  , let (ktv_names, _, head_ty') = splitLHsInstDeclTy inst_ty'
  , cls <- case hsTyGetAppHead_maybe head_ty' of
             Just (L _ cls) -> pure cls
             Nothing ->
               [ mkUnboundName (mkTcOccFS (fsLit "<class>")) |
               -- The instance is malformed. We'd still like
               -- to make *some* progress (rather than failing outright), so
               -- we report an error and continue for as long as we can.
               -- Importantly, this error should be thrown before we reach the
               -- typechecker, lest we encounter different errors that are
               -- hopelessly confusing (such as the one in #16114).
               () <- addErrAt (getLoc (hsSigType inst_ty)) $
                 hang (text "Illegal class instance:" <+> quotes (ppr inst_ty))
                    2 (vcat [ text "Class instances must be of the form"
                            , nest 2 $ text "context => C ty_1 ... ty_n"
                            , text "where" <+> quotes (char 'C')
                              <+> text "is a class"
                            ]) ]

          -- Rename the bindings
          -- The typechecker (not the renamer) checks that all
          -- the bindings are for the right class
          -- (Slightly strangely) when scoped type variables are on, the
          -- forall-d tyvars scope over the method bindings too
  , (mbinds', uprags', meth_fvs) <- rnMethodBinds False cls ktv_names mbinds uprags

    -- Rename the associated types, and type signatures
    -- Both need to have the instance type variables in scope
  , () <- traceRn "rnSrcInstDecl" (ppr inst_ty' $$ ppr ktv_names)
  , ((ats', adts'), more_fvs) <- runWriterT $ bindLocalNamesFVW ktv_names $ (,)
    <$> rnATInstDecls rnTyFamInstDecl cls ktv_names ats
    <*> rnATInstDecls rnDataFamInstDecl cls ktv_names adts

  , let all_fvs = meth_fvs `plusFV` more_fvs `plusFV` inst_fvs
    -- We return the renamed associated data type declarations so that they can be entered into
    -- the list of type declarations for the binding group, but we also keep a copy in the
    -- instance.  The latter is needed for well-formedness checks in the type checker (eg, to
    -- ensure that all ATs of the instance actually receive a declaration).
    -- NB: Even the copies in the instance declaration carry copies of the instance context
    --     after renaming.  This is a bit strange, but should not matter (and it would be more
    --     work to remove the context).
  ]
  where
    inf_err = Just (text "Inferred type variables are not allowed")

rnFamInstEqn :: HsDocContext
             -> AssocTyFamInfo
             -> FreeKiTyVars
             -- ^ Kind variables from the equation's RHS to be implicitly bound
             -- if no explicit forall.
             -> FamInstEqn GhcPs a
             -> (HsDocContext -> a -> WriterT FreeVars RnM b)
             -> WriterT FreeVars RnM (FamInstEqn GhcRn b)
rnFamInstEqn doc atfi rhs_kvars HsIB
  { hsib_body = FamEqn
      { feqn_tycon  = tycon
      , feqn_bndrs  = mb_bndrs
      , feqn_pats   = pats
      , feqn_fixity = fixity
      , feqn_rhs    = payload } } rn_payload = WriterT
    do { tycon' <- lookupFamInstName mb_cls tycon

         -- all_imp_vars represent the implicitly bound type variables. This is empty if we have
         -- an explicit `forall` (see Note [forall-or-nothing rule] in GHC.Rename.HsType), which
         -- means ignoring:
         --
         -- - pat_kity_vars_with_dups, the variables mentioned in the LHS of the equation, and
         -- - rhs_kvars, the kind variables mentioned in an outermost kind signature on the RHS
         --   of the equation. (See Note [Implicit quantification in type synonyms] in
         --   GHC.Rename.HsType for why these are implicitly quantified in the absence of an
         --   explicit forall).
         --
         -- For example:
         --
         -- @
         -- type family F a b
         -- type instance forall a b c. F [(a, b)] c = a -> b -> c
         --   -- all_imp_vars = []
         -- type instance F [(a, b)] c = a -> b -> c
         --   -- all_imp_vars = [a, b, c]
         -- @
       ; all_imp_vars <- forAllOrNothing (isJust mb_bndrs) $
           -- No need to filter out explicit binders (the 'mb_bndrs = Just
           -- explicit_bndrs' case) because there must be none if we're going
           -- to implicitly bind anything, per the previous comment.
           pat_kity_vars_with_dups ++ rhs_kvars

       ; runWriterT $ rnImplicitBndrs mb_cls all_imp_vars $ \all_imp_var_names' ->
         bindLHsTyVarBndrs doc WarnUnusedForalls
                           Nothing (fromMaybe [] mb_bndrs) $ \bndrs' -> WriterT
         -- Note: If we pass mb_cls instead of Nothing here, bindLHsTyVarBndrs will use class
         --  variables for any names the user meant to bring in scope here. This is an explicit
         --  forall, so we want fresh names, not class variables.
         --  Thus: always pass Nothing
      [ (HsIB
          { hsib_ext = all_imp_var_names -- Note [Wildcards in family instances]
          , hsib_body = FamEqn
              { feqn_ext    = noExtField
              , feqn_tycon  = tycon'
              , feqn_bndrs  = bndrs' <$ mb_bndrs
              , feqn_pats   = pats'
              , feqn_fixity = fixity
              , feqn_rhs    = payload' } }, all_fvs)
      | (pats', pat_fvs) <- runWriterT $ rnLHsTypeArg (FamPatCtx tycon) `traverse` pats
      , (payload', rhs_fvs) <- runWriterT $ rn_payload doc payload

        -- Report unused binders on the LHS
        -- See Note [Unused type variables in family instances]
      , let -- The SrcSpan that rnImplicitBndrs will attach to each Name will
            -- span the entire type family instance, which will be reflected in
            -- -Wunused-type-patterns warnings. We can be a little more precise
            -- than that by pointing to the LHS of the instance instead, which
            -- is what lhs_loc corresponds to.
            all_imp_var_names = set nameLocL lhs_loc <$> all_imp_var_names'

            groups :: [NonEmpty (Located RdrName)]
            groups = equivClasses cmpLocated pat_kity_vars_with_dups
      , nms_dups <- traverse (lookupOccRn . unLoc) [ tv | (tv :| (_:_)) <- groups ]
            -- Add to the used variables
            --  a) any variables that appear *more than once* on the LHS
            --     e.g.   F a Int a = Bool
            --  b) for associated instances, the variables of the instance decl.  See
            --     Note [Unused type variables in family instances]
      , let nms_used = extendNameSetList rhs_fvs $ inst_tvs ++ nms_dups
            all_nms = all_imp_var_names ++ hsLTyVarNames bndrs'
      , () <- warnUnusedTypePatterns all_nms nms_used

      , let all_fvs = (rhs_fvs `plusFV` pat_fvs) `addOneFV` unLoc tycon'
            -- type instance => use, hence addOneFV
      ] }
  where
    -- The parent class, if we are dealing with an associated type family instance.
    mb_cls = case atfi of
      NonAssocTyFamEqn     -> Nothing
      AssocTyFamDeflt cls  -> Just cls
      AssocTyFamInst cls _ -> Just cls

    -- The type variables from the instance head, if we are dealing with an
    -- associated type family instance.
    inst_tvs = case atfi of
      NonAssocTyFamEqn          -> []
      AssocTyFamDeflt _         -> []
      AssocTyFamInst _ inst_tvs -> inst_tvs

    pat_kity_vars_with_dups = extractHsTyArgRdrKiTyVars pats
             -- It is crucial that extractHsTyArgRdrKiTyVars return
             -- duplicate occurrences, since they're needed to help
             -- determine unused binders on the LHS.

    -- The SrcSpan of the LHS of the instance. For example, lhs_loc would be
    -- the highlighted part in the example below:
    --
    --   type instance F a b c = Either a b
    --                   ^^^^^
    lhs_loc = case map lhsTypeArgSrcSpan pats ++ map getLoc rhs_kvars of
      []         -> panic "rnFamInstEqn.lhs_loc"
      [loc]      -> loc
      (loc:locs) -> loc <> last locs

rnTyFamInstDecl :: AssocTyFamInfo
                -> TyFamInstDecl GhcPs
                -> WriterT FreeVars RnM (TyFamInstDecl GhcRn)
rnTyFamInstDecl atfi = tfid_eqnL $ rnTyFamInstEqn atfi NotClosedTyFam

-- | Tracks whether we are renaming:
--
-- 1. A type family equation that is not associated
--    with a parent type class ('NonAssocTyFamEqn')
--
-- 2. An associated type family default declaration ('AssocTyFamDeflt')
--
-- 3. An associated type family instance declaration ('AssocTyFamInst')
data AssocTyFamInfo
  = NonAssocTyFamEqn
  | AssocTyFamDeflt Name   -- Name of the parent class
  | AssocTyFamInst  Name   -- Name of the parent class
                    [Name] -- Names of the tyvars of the parent instance decl

-- | Tracks whether we are renaming an equation in a closed type family
-- equation ('ClosedTyFam') or not ('NotClosedTyFam').
data ClosedTyFamInfo
  = NotClosedTyFam
  | ClosedTyFam (Located RdrName) Name
                -- The names (RdrName and Name) of the closed type family

rnTyFamInstEqn :: AssocTyFamInfo
               -> ClosedTyFamInfo
               -> TyFamInstEqn GhcPs
               -> WriterT FreeVars RnM (TyFamInstEqn GhcRn)
rnTyFamInstEqn atfi ctf_info eqn@HsIB
  { hsib_body = FamEqn { feqn_tycon = tycon, feqn_rhs = rhs } }
  = do { let rhs_kvs = extractHsTyRdrTyVarsKindVars rhs
       ; eqn'@HsIB { hsib_body = FamEqn { feqn_tycon = L _ tycon' } }
           <- rnFamInstEqn (TySynCtx tycon) atfi rhs_kvs eqn rnLHsType
       ; eqn' <$ lift case ctf_info of
           NotClosedTyFam -> pure ()
           ClosedTyFam fam_rdr_name fam_name ->
             checkTc (fam_name == tycon') $
             withHsDocContext (TyFamilyCtx fam_rdr_name) $
             wrongTyFamName fam_name tycon' }

rnTyFamDefltDecl :: Name
                 -> TyFamDefltDecl GhcPs
                 -> WriterT FreeVars RnM (TyFamDefltDecl GhcRn)
rnTyFamDefltDecl cls = rnTyFamInstDecl (AssocTyFamDeflt cls)

rnDataFamInstDecl :: AssocTyFamInfo
                  -> DataFamInstDecl GhcPs
                  -> WriterT FreeVars RnM (DataFamInstDecl GhcRn)
rnDataFamInstDecl atfi = dfid_eqnL \ eqn@HsIB
  { hsib_body = FamEqn { feqn_tycon = tycon, feqn_rhs = rhs } } ->
    let rhs_kvs = extractDataDefnKindVars rhs
    in rnFamInstEqn (TyDataCtx tycon) atfi rhs_kvs eqn rnDataDefn

-- Renaming of the associated types in instances.

-- Rename associated type family decl in class
rnATDecls :: Name      -- Class
          -> [LFamilyDecl GhcPs]
          -> WriterT FreeVars RnM [LFamilyDecl GhcRn]
rnATDecls cls = (traverse . wrapLocM) (rnFamDecl (Just cls))

rnATInstDecls :: (AssocTyFamInfo ->           -- The function that renames
                  decl GhcPs ->               -- an instance. rnTyFamInstDecl
                  WriterT FreeVars RnM (decl GhcRn)) -- or rnDataFamInstDecl
              -> Name      -- Class
              -> [Name]
              -> [Located (decl GhcPs)]
              -> WriterT FreeVars RnM [Located (decl GhcRn)]
-- Used for data and type family defaults in a class decl
-- and the family instance declarations in an instance
--
-- NB: We allow duplicate associated-type decls;
--     See Note [Associated type instances] in GHC.Tc.TyCl.Instance
rnATInstDecls rnFun cls tv_ns = (traverse . wrapLocM) (rnFun (AssocTyFamInst cls tv_ns))
    -- See Note [Renaming associated types]

{- Note [Wildcards in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Wild cards can be used in type/data family instance declarations to indicate
that the name of a type variable doesn't matter. Each wild card will be
replaced with a new unique type variable. For instance:

    type family F a b :: *
    type instance F Int _ = Int

is the same as

    type family F a b :: *
    type instance F Int b = Int

This is implemented as follows: Unnamed wildcards remain unchanged after
the renamer, and then given fresh meta-variables during typechecking, and
it is handled pretty much the same way as the ones in partial type signatures.
We however don't want to emit hole constraints on wildcards in family
instances, so we turn on PartialTypeSignatures and turn off warning flag to
let typechecker know this.
See related Note [Wildcards in visible kind application] in GHC.Tc.Gen.HsType

Note [Unused type variables in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the flag -fwarn-unused-type-patterns is on, the compiler reports
warnings about unused type variables in type-family instances. A
tpye variable is considered used (i.e. cannot be turned into a wildcard)
when

 * it occurs on the RHS of the family instance
   e.g.   type instance F a b = a    -- a is used on the RHS

 * it occurs multiple times in the patterns on the LHS
   e.g.   type instance F a a = Int  -- a appears more than once on LHS

 * it is one of the instance-decl variables, for associated types
   e.g.   instance C (a,b) where
            type T (a,b) = a
   Here the type pattern in the type instance must be the same as that
   for the class instance, so
            type T (a,_) = a
   would be rejected.  So we should not complain about an unused variable b

As usual, the warnings are not reported for type variables with names
beginning with an underscore.

Extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699, #10586, #10982 and #11451.

Note [Renaming associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check that the RHS of the decl mentions only type variables that are explicitly
bound on the LHS.  For example, this is not ok
   class C a b where
      type F a x :: *
   instance C (p,q) r where
      type F (p,q) x = (x, r)   -- BAD: mentions 'r'
c.f. #5515

Kind variables, on the other hand, are allowed to be implicitly or explicitly
bound. As examples, this (#9574) is acceptable:
   class Funct f where
      type Codomain f :: *
   instance Funct ('KProxy :: KProxy o) where
      -- o is implicitly bound by the kind signature
      -- of the LHS type pattern ('KProxy)
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)
And this (#14131) is also acceptable:
    data family Nat :: k -> k -> *
    -- k is implicitly bound by an invisible kind pattern
    newtype instance Nat :: (k -> *) -> (k -> *) -> * where
      Nat :: (forall xx. f xx -> g xx) -> Nat f g
We could choose to disallow this, but then associated type families would not
be able to be as expressive as top-level type synonyms. For example, this type
synonym definition is allowed:
    type T = (Nothing :: Maybe a)
So for parity with type synonyms, we also allow:
    type family   T :: Maybe a
    type instance T = (Nothing :: Maybe a)

All this applies only for *instance* declarations.  In *class*
declarations there is no RHS to worry about, and the class variables
can all be in scope (#5862):
    class Category (x :: k -> k -> *) where
      type Ob x :: k -> Constraint
      id :: Ob x a => x a a
      (.) :: (Ob x a, Ob x b, Ob x c) => x b c -> x a b -> x a c
Here 'k' is in scope in the kind signature, just like 'x'.

Although type family equations can bind type variables with explicit foralls,
it need not be the case that all variables that appear on the RHS must be bound
by a forall. For instance, the following is acceptable:

   class C a where
     type T a b
   instance C (Maybe a) where
     type forall b. T (Maybe a) b = Either a b

Even though `a` is not bound by the forall, this is still accepted because `a`
was previously bound by the `instance C (Maybe a)` part. (see #16116).

In each case, the function which detects improperly bound variables on the RHS
is GHC.Tc.Validity.checkValidFamPats.
-}


{-
*********************************************************
*                                                      *
\subsection{Stand-alone deriving declarations}
*                                                      *
*********************************************************
-}

rnSrcDerivDecl :: DerivDecl GhcPs -> WriterT FreeVars RnM (DerivDecl GhcRn)
rnSrcDerivDecl (DerivDecl _ ty mds overlap) = WriterT
  [ (DerivDecl noExtField ty' mds' overlap, fvs)
  | standalone_deriv_ok <- xoptM LangExt.StandaloneDeriving
  , () <- unless standalone_deriv_ok (addErr standaloneDerivErr)
  , (mds', ty', fvs) <- rnLDerivStrategy DerivDeclCtx mds $
        runWriterT $ rnHsSigWcType DerivDeclCtx inf_err ty
  , () <- warnNoDerivStrat mds' loc ]
  where
    inf_err = Just (text "Inferred type variables are not allowed")
    loc = getLoc $ hsib_body $ hswc_body ty

standaloneDerivErr :: SDoc
standaloneDerivErr
  = hang (text "Illegal standalone deriving declaration")
       2 (text "Use StandaloneDeriving to enable this extension")

{-
*********************************************************
*                                                      *
\subsection{Rules}
*                                                      *
*********************************************************
-}

rnHsRuleDecls :: RuleDecls GhcPs -> WriterT FreeVars RnM (RuleDecls GhcRn)
rnHsRuleDecls HsRules { rds_src = src, rds_rules = rules } =
  [ HsRules { rds_ext = noExtField, rds_src = src, rds_rules = rn_rules }
  | rn_rules <- (traverse . wrapLocM) rnHsRuleDecl rules ]

rnHsRuleDecl :: RuleDecl GhcPs -> WriterT FreeVars RnM (RuleDecl GhcRn)
rnHsRuleDecl (HsRule { rd_name = rule_name
                     , rd_act  = act
                     , rd_tyvs = tyvs
                     , rd_tmvs = tmvs
                     , rd_lhs  = lhs
                     , rd_rhs  = rhs })
  = do { let rdr_names_w_loc = map (get_var . unLoc) tmvs
       ; lift $ checkDupRdrNames rdr_names_w_loc
       ; lift $ checkShadowedRdrNames rdr_names_w_loc
       ; names <- lift $ newLocalBndrsRn rdr_names_w_loc
       ; let doc = RuleCtx (snd $ unLoc rule_name)
       ; bindRuleTyVars doc tyvs $ \ tyvs' ->
         bindRuleTmVars doc tyvs' tmvs names $ \ tmvs' ->
  [ HsRule
      { rd_ext  = HsRuleRn fv_lhs' fv_rhs'
      , rd_name = rule_name
      , rd_act  = act
      , rd_tyvs = tyvs'
      , rd_tmvs = tmvs'
      , rd_lhs  = lhs'
      , rd_rhs  = rhs' }
  | (lhs', fv_lhs') <- listen $ rnLExpr lhs
  , (rhs', fv_rhs') <- listen $ rnLExpr rhs
  , () <- lift $ checkValidRule (snd $ unLoc rule_name) names lhs' fv_lhs' ] }
  where
    get_var :: RuleBndr GhcPs -> Located RdrName
    get_var (RuleBndrSig _ v _) = v
    get_var (RuleBndr _ v)      = v

bindRuleTmVars :: HsDocContext -> Maybe ty_bndrs
               -> [LRuleBndr GhcPs] -> [Name]
               -> ([LRuleBndr GhcRn] -> WriterT FreeVars RnM a)
               -> WriterT FreeVars RnM a
bindRuleTmVars doc tyvs vars names thing_inside
  = go vars names $ bindLocalNamesFVW names . thing_inside
  where
    go (L l (RuleBndr _ (L loc _)) : vars) (n : ns) thing_inside
      = go vars ns $ \ vars' ->
        thing_inside (L l (RuleBndr noExtField (L loc n)) : vars')

    go (L l (RuleBndrSig _ (L loc _) bsig) : vars) (n : ns) thing_inside
      = rnHsPatSigType bind_free_tvs doc Nothing bsig $ \ bsig' ->
        go vars ns $ \ vars' ->
        thing_inside (L l (RuleBndrSig noExtField (L loc n) bsig') : vars')

    go [] [] thing_inside = thing_inside []
    go vars names _ = pprPanic "bindRuleVars" (ppr vars $$ ppr names)

    bind_free_tvs = case tyvs of Nothing -> AlwaysBind
                                 Just _  -> NeverBind

bindRuleTyVars :: HsDocContext -> Maybe [LHsTyVarBndr () GhcPs]
               -> (Maybe [LHsTyVarBndr () GhcRn]  -> WriterT FreeVars RnM b)
               -> WriterT FreeVars RnM b
bindRuleTyVars doc (Just bndrs) f = bindLHsTyVarBndrs doc WarnUnusedForalls Nothing bndrs (f . Just)
bindRuleTyVars _ _ f = f Nothing

{-
Note [Rule LHS validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check the shape of a rewrite rule LHS.  Currently we only allow
LHSs of the form @(f e1 .. en)@, where @f@ is not one of the
@forall@'d variables.

We used restrict the form of the 'ei' to prevent you writing rules
with LHSs with a complicated desugaring (and hence unlikely to match);
(e.g. a case expression is not allowed: too elaborate.)

But there are legitimate non-trivial args ei, like sections and
lambdas.  So it seems simmpler not to check at all, and that is why
check_e is commented out.
-}

checkValidRule :: FastString -> [Name] -> LHsExpr GhcRn -> NameSet -> RnM ()
checkValidRule rule_name ids lhs' fv_lhs'
  = do  {       -- Check for the form of the LHS
          for_ (validRuleLhs ids lhs') $ failWithTc . badRuleLhsErr rule_name lhs'

                -- Check that LHS vars are all bound
        ; let bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs')]
        ; traverse_ (addErr . badRuleVar rule_name) bad_vars }

validRuleLhs :: [Name] -> LHsExpr GhcRn -> Maybe (HsExpr GhcRn)
-- Nothing => OK
-- Just e  => Not ok, and e is the offending sub-expression
validRuleLhs foralls = checkl
  where
    checkl = check . unLoc

    check (OpApp _ e1 op e2)              = checkl op `mplus` checkl_e e1
                                                      `mplus` checkl_e e2
    check (HsApp _ e1 e2)                 = checkl e1 `mplus` checkl_e e2
    check (HsAppType _ e _)               = checkl e
    check (HsVar _ lv)
      | (unLoc lv) `notElem` foralls      = Nothing
    check other                           = Just other  -- Failure

        -- Check an argument
    checkl_e _ = Nothing
    -- Was (check_e e); see Note [Rule LHS validity checking]

{-      Commented out; see Note [Rule LHS validity checking] above
    check_e (HsVar v)     = Nothing
    check_e (HsPar e)     = checkl_e e
    check_e (HsLit e)     = Nothing
    check_e (HsOverLit e) = Nothing

    check_e (OpApp e1 op _ e2)   = checkl_e e1 `mplus` checkl_e op `mplus` checkl_e e2
    check_e (HsApp e1 e2)        = checkl_e e1 `mplus` checkl_e e2
    check_e (NegApp e _)         = checkl_e e
    check_e (ExplicitList _ es)  = checkl_es es
    check_e other                = Just other   -- Fails

    checkl_es es = foldr (mplus . checkl_e) Nothing es
-}

badRuleVar :: FastString -> Name -> SDoc
badRuleVar name var
  = sep [text "Rule" <+> doubleQuotes (ftext name) <> colon,
         text "Forall'd variable" <+> quotes (ppr var) <+>
                text "does not appear on left hand side"]

badRuleLhsErr :: FastString -> LHsExpr GhcRn -> HsExpr GhcRn -> SDoc
badRuleLhsErr name lhs bad_e
  = sep [text "Rule" <+> pprRuleName name <> colon,
         nest 2 (vcat [err,
                       text "in left-hand side:" <+> ppr lhs])]
    $$
    text "LHS must be of form (f e1 .. en) where f is not forall'd"
  where
    err = case bad_e of
            HsUnboundVar _ uv -> notInScopeErr (mkRdrUnqual uv)
            _                 -> text "Illegal expression:" <+> ppr bad_e

{- **************************************************************
         *                                                      *
      Renaming type, class, instance and role declarations
*                                                               *
*****************************************************************

@rnTyDecl@ uses the `global name function' to create a new type
declaration in which local names have been replaced by their original
names, reporting any unknown names.

Renaming type variables is a pain. Because they now contain uniques,
it is necessary to pass in an association list which maps a parsed
tyvar to its @Name@ representation.
In some cases (type signatures of values),
it is even necessary to go over the type first
in order to get the set of tyvars used by it, make an assoc list,
and then go over it again to rename the tyvars!
However, we can also do some scoping checks at the same time.

Note [Dependency analysis of type, class, and instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected components of
type/class/instance decls, together with the role annotations for the
type/class declarations.  The renamer uses strongly connected
comoponent analysis to build these groups.  We do this for a number of
reasons:

* Improve kind error messages. Consider

     data T f a = MkT f a
     data S f a = MkS f (T f a)

  This has a kind error, but the error message is better if you
  check T first, (fixing its kind) and *then* S.  If you do kind
  inference together, you might get an error reported in S, which
  is jolly confusing.  See #4875


* Increase kind polymorphism.  See GHC.Tc.TyCl
  Note [Grouping of type and class declarations]

Why do the instance declarations participate?  At least two reasons

* Consider (#11348)

     type family F a
     type instance F Int = Bool

     data R = MkR (F Int)

     type Foo = 'MkR 'True

  For Foo to kind-check we need to know that (F Int) ~ Bool.  But we won't
  know that unless we've looked at the type instance declaration for F
  before kind-checking Foo.

* Another example is this (#3990).

     data family Complex a
     data instance Complex Double = CD {-# UNPACK #-} !Double
                                       {-# UNPACK #-} !Double

     data T = T {-# UNPACK #-} !(Complex Double)

  Here, to generate the right kind of unpacked implementation for T,
  we must have access to the 'data instance' declaration.

* Things become more complicated when we introduce transitive
  dependencies through imported definitions, like in this scenario:

      A.hs
        type family Closed (t :: Type) :: Type where
          Closed t = Open t

        type family Open (t :: Type) :: Type

      B.hs
        data Q where
          Q :: Closed Bool -> Q

        type instance Open Int = Bool

        type S = 'Q 'True

  Somehow, we must ensure that the instance Open Int = Bool is checked before
  the type synonym S. While we know that S depends upon 'Q depends upon Closed,
  we have no idea that Closed depends upon Open!

  To accommodate for these situations, we ensure that an instance is checked
  before every @TyClDecl@ on which it does not depend. That's to say, instances
  are checked as early as possible in @tcTyAndClassDecls@.

------------------------------------
So much for WHY.  What about HOW?  It's pretty easy:

(1) Rename the type/class, instance, and role declarations
    individually

(2) Do strongly-connected component analysis of the type/class decls,
    We'll make a TyClGroup for each SCC

    In this step we treat a reference to a (promoted) data constructor
    K as a dependency on its parent type.  Thus
        data T = K1 | K2
        data S = MkS (Proxy 'K1)
    Here S depends on 'K1 and hence on its parent T.

    In this step we ignore instances; see
    Note [No dependencies on data instances]

(3) Attach roles to the appropriate SCC

(4) Attach instances to the appropriate SCC.
    We add an instance decl to SCC when:
      all its free types/classes are bound in this SCC or earlier ones

(5) We make an initial TyClGroup, with empty group_tyclds, for any
    (orphan) instances that affect only imported types/classes

Steps (3) and (4) are done by the (mapAccumL mk_group) call.

Note [No dependencies on data instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   data family D a
   data instance D Int = D1
   data S = MkS (Proxy 'D1)

Here the declaration of S depends on the /data instance/ declaration
for 'D Int'.  That makes things a lot more complicated, especially
if the data instance is an associated type of an enclosing class instance.
(And the class instance might have several associated type instances
with different dependency structure!)

Ugh.  For now we simply don't allow promotion of data constructors for
data instances.  See Note [AFamDataCon: not promoting data family
constructors] in GHC.Tc.Utils.Env
-}


rnTyClDecls :: [TyClGroup GhcPs]
            -> WriterT FreeVars RnM [TyClGroup GhcRn]
-- Rename the declarations and do dependency analysis on them
rnTyClDecls tycl_ds
  = WriterT
    do { -- Rename the type/class, instance, and role declaraations
       ; tycls_w_fvs <- traverse (runWriterT . wrapLocM rnTyClDecl) (tyClGroupTyClDecls tycl_ds)
       ; let tc_names = mkNameSet (tcdName . unLoc . fst <$> tycls_w_fvs)
       ; kisigs_w_fvs <- rnStandaloneKindSignatures tc_names (tyClGroupKindSigs tycl_ds)
       ; instds_w_fvs <- traverse (runWriterT . wrapLocM rnSrcInstDecl) (tyClGroupInstDecls tycl_ds)
       ; role_annots  <- rnRoleAnnots tc_names (tyClGroupRoleDecls tycl_ds)

       -- Do SCC analysis on the type/class decls
       ; rdr_env <- getGlobalRdrEnv
       ; let tycl_sccs = depAnalTyClDecls rdr_env kisig_fv_env tycls_w_fvs
             role_annot_env = mkRoleAnnotEnv role_annots
             (kisig_env, kisig_fv_env) = mkKindSig_fv_env kisigs_w_fvs

             inst_ds_map = mkInstDeclFreeVarsMap rdr_env tc_names instds_w_fvs
             (init_inst_ds, rest_inst_ds) = getInsts [] inst_ds_map

             first_group
               | null init_inst_ds = []
               | otherwise = [TyClGroup { group_ext    = noExtField
                                        , group_tyclds = []
                                        , group_kisigs = []
                                        , group_roles  = []
                                        , group_instds = init_inst_ds }]

             (final_inst_ds, groups)
                = mapAccumL (mk_group role_annot_env kisig_env) rest_inst_ds tycl_sccs

             all_fvs = foldr (plusFV . snd) emptyFVs tycls_w_fvs  `plusFV`
                       foldr (plusFV . snd) emptyFVs instds_w_fvs `plusFV`
                       foldr (plusFV . snd) emptyFVs kisigs_w_fvs

             all_groups = first_group ++ groups

       ; massertPpr (null final_inst_ds)
                    (ppr instds_w_fvs
                     $$ ppr inst_ds_map
                     $$ ppr (flattenSCCs tycl_sccs)
                     $$ ppr final_inst_ds)

       ; (all_groups, all_fvs) <$ traceRn "rnTycl dependency analysis made groups" (ppr all_groups) }
  where
    mk_group :: RoleAnnotEnv
             -> KindSigEnv
             -> InstDeclFreeVarsMap
             -> SCC (LTyClDecl GhcRn)
             -> (InstDeclFreeVarsMap, TyClGroup GhcRn)
    mk_group role_env kisig_env inst_map scc
      = (inst_map', group)
      where
        tycl_ds              = flattenSCC scc
        bndrs                = map (tcdName . unLoc) tycl_ds
        roles                = getRoleAnnots bndrs role_env
        kisigs               = getKindSigs   bndrs kisig_env
        (inst_ds, inst_map') = getInsts      bndrs inst_map
        group = TyClGroup { group_ext    = noExtField
                          , group_tyclds = tycl_ds
                          , group_kisigs = kisigs
                          , group_roles  = roles
                          , group_instds = inst_ds }

-- | Free variables of standalone kind signatures.
newtype KindSig_FV_Env = KindSig_FV_Env (NameEnv FreeVars)

lookupKindSig_FV_Env :: KindSig_FV_Env -> Name -> FreeVars
lookupKindSig_FV_Env (KindSig_FV_Env e) name = fromMaybe emptyFVs (lookupNameEnv e name)

-- | Standalone kind signatures.
type KindSigEnv = NameEnv (LStandaloneKindSig GhcRn)

mkKindSig_fv_env :: [(LStandaloneKindSig GhcRn, FreeVars)] -> (KindSigEnv, KindSig_FV_Env)
mkKindSig_fv_env kisigs_w_fvs = (kisig_env, kisig_fv_env)
  where
    kisig_env = fmap fst compound_env
    kisig_fv_env = KindSig_FV_Env (fmap snd compound_env)
    compound_env :: NameEnv (LStandaloneKindSig GhcRn, FreeVars)
      = mkNameEnvWith (standaloneKindSigName . unLoc . fst) kisigs_w_fvs

getKindSigs :: [Name] -> KindSigEnv -> [LStandaloneKindSig GhcRn]
getKindSigs bndrs kisig_env = mapMaybe (lookupNameEnv kisig_env) bndrs

rnStandaloneKindSignatures
  :: NameSet  -- names of types and classes in the current TyClGroup
  -> [LStandaloneKindSig GhcPs]
  -> RnM [(LStandaloneKindSig GhcRn, FreeVars)]
rnStandaloneKindSignatures tc_names kisigs
  = do { let (no_dups, dup_kisigs) = removeDups (compare `on` get_name) kisigs
             get_name = standaloneKindSigName . unLoc
       ; traverse_ dupKindSig_Err dup_kisigs
       ; traverse (wrapLocFstM (rnStandaloneKindSignature tc_names)) no_dups
       }

rnStandaloneKindSignature
  :: NameSet  -- names of types and classes in the current TyClGroup
  -> StandaloneKindSig GhcPs
  -> RnM (StandaloneKindSig GhcRn, FreeVars)
rnStandaloneKindSignature tc_names (StandaloneKindSig _ v ki) = runWriterT
  [ StandaloneKindSig noExtField new_v new_ki
  | standalone_ki_sig_ok <- xoptM LangExt.StandaloneKindSignatures
  , () <- lift $ unless standalone_ki_sig_ok $ addErr standaloneKiSigErr
  , new_v <- lift $ lookupSigCtxtOccRn (TopSigCtxt tc_names) (text "standalone kind signature") v
  , let doc = StandaloneKindSigCtx (ppr v)
  , new_ki <- rnHsSigType doc KindLevel Nothing ki ]
  where
    standaloneKiSigErr :: SDoc
    standaloneKiSigErr =
      hang (text "Illegal standalone kind signature")
         2 (text "Did you mean to enable StandaloneKindSignatures?")

depAnalTyClDecls :: GlobalRdrEnv
                 -> KindSig_FV_Env
                 -> [(LTyClDecl GhcRn, FreeVars)]
                 -> [SCC (LTyClDecl GhcRn)]
-- See Note [Dependency analysis of type, class, and instance decls]
depAnalTyClDecls rdr_env kisig_fv_env ds_w_fvs
  = stronglyConnCompFromEdgedVerticesUniq edges
  where
    edges :: [ Node Name (LTyClDecl GhcRn) ]
    edges = [ DigraphNode d name (map (getParent rdr_env) (nonDetEltsUniqSet deps))
            | (d, fvs) <- ds_w_fvs,
              let { name = tcdName (unLoc d)
                  ; kisig_fvs = lookupKindSig_FV_Env kisig_fv_env name
                  ; deps = fvs `plusFV` kisig_fvs
                  }
            ]
            -- It's OK to use nonDetEltsUFM here as
            -- stronglyConnCompFromEdgedVertices is still deterministic
            -- even if the edges are in nondeterministic order as explained
            -- in Note [Deterministic SCC] in GHC.Data.Graph.Directed.

toParents :: GlobalRdrEnv -> NameSet -> NameSet
toParents rdr_env = nonDetStrictFoldUniqSet add emptyNameSet
  -- It's OK to use a non-deterministic fold because we immediately forget the
  -- ordering by creating a set
  where
    add n s = extendNameSet s (getParent rdr_env n)

getParent :: GlobalRdrEnv -> Name -> Name
getParent rdr_env n
  = case lookupGRE_Name rdr_env n of
      Just gre -> case gre_par gre of
                    ParentIs  { par_is = p } -> p
                    FldParent { par_is = p } -> p
                    _                        -> n
      Nothing -> n


{- ******************************************************
*                                                       *
       Role annotations
*                                                       *
****************************************************** -}

-- | Renames role annotations, returning them as the values in a NameEnv
-- and checks for duplicate role annotations.
-- It is quite convenient to do both of these in the same place.
-- See also Note [Role annotations in the renamer]
rnRoleAnnots :: NameSet
             -> [LRoleAnnotDecl GhcPs]
             -> RnM [LRoleAnnotDecl GhcRn]
rnRoleAnnots tc_names role_annots
  = do {  -- Check for duplicates *before* renaming, to avoid
          -- lumping together all the unboundNames
         let (no_dups, dup_annots) = removeDups (compare `on` get_name) role_annots
             get_name = roleAnnotDeclName . unLoc
       ; traverse_ dupRoleAnnotErr dup_annots
       ; traverse (wrapLocM rn_role_annot1) no_dups }
  where
    rn_role_annot1 (RoleAnnotDecl _ tycon roles) =
      [ RoleAnnotDecl noExtField tycon' roles
      | -- the name is an *occurrence*, but look it up only in the
        -- decls defined in this group (see #10263)
        tycon' <- lookupSigCtxtOccRn (RoleAnnotCtxt tc_names) (text "role annotation") tycon ]

dupRoleAnnotErr :: NonEmpty (LRoleAnnotDecl GhcPs) -> RnM ()
dupRoleAnnotErr list
  = addErrAt loc $
    hang (text "Duplicate role annotations for" <+>
          quotes (ppr $ roleAnnotDeclName first_decl) <> colon)
       2 (vcat $ map pp_role_annot $ toList sorted_list)
    where
      sorted_list = NE.sortBy cmp_loc list
      ((L loc first_decl) :| _) = sorted_list

      pp_role_annot (L loc decl) = hang (ppr decl)
                                      4 (text "-- written at" <+> ppr loc)

      cmp_loc = SrcLoc.leftmost_smallest `on` getLoc

dupKindSig_Err :: NonEmpty (LStandaloneKindSig GhcPs) -> RnM ()
dupKindSig_Err list
  = addErrAt loc $
    hang (text "Duplicate standalone kind signatures for" <+>
          quotes (ppr $ standaloneKindSigName first_decl) <> colon)
       2 (vcat $ map pp_kisig $ toList sorted_list)
    where
      sorted_list = NE.sortBy cmp_loc list
      L loc first_decl :| _ = sorted_list

      pp_kisig (L loc decl) =
        hang (ppr decl) 4 (text "-- written at" <+> ppr loc)

      cmp_loc = SrcLoc.leftmost_smallest `on` getLoc

{- Note [Role annotations in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must ensure that a type's role annotation is put in the same group as the
proper type declaration. This is because role annotations are needed during
type-checking when creating the type's TyCon. So, rnRoleAnnots builds a
NameEnv (LRoleAnnotDecl Name) that maps a name to a role annotation for that
type, if any. Then, this map can be used to add the role annotations to the
groups after dependency analysis.

This process checks for duplicate role annotations, where we must be careful
to do the check *before* renaming to avoid calling all unbound names duplicates
of one another.

The renaming process, as usual, might identify and report errors for unbound
names. This is done by using lookupSigCtxtOccRn in rnRoleAnnots (using
lookupGlobalOccRn led to #8485).
-}


{- ******************************************************
*                                                       *
       Dependency info for instances
*                                                       *
****************************************************** -}

----------------------------------------------------------
-- | 'InstDeclFreeVarsMap is an association of an
--   @InstDecl@ with @FreeVars@. The @FreeVars@ are
--   the tycon names that are both
--     a) free in the instance declaration
--     b) bound by this group of type/class/instance decls
type InstDeclFreeVarsMap = [(LInstDecl GhcRn, FreeVars)]

-- | Construct an @InstDeclFreeVarsMap@ by eliminating any @Name@s from the
--   @FreeVars@ which are *not* the binders of a @TyClDecl@.
mkInstDeclFreeVarsMap :: GlobalRdrEnv
                      -> NameSet
                      -> [(LInstDecl GhcRn, FreeVars)]
                      -> InstDeclFreeVarsMap
mkInstDeclFreeVarsMap rdr_env tycl_bndrs inst_ds_fvs
  = [ (inst_decl, toParents rdr_env fvs `intersectFVs` tycl_bndrs)
    | (inst_decl, fvs) <- inst_ds_fvs ]

-- | Get the @LInstDecl@s which have empty @FreeVars@ sets, and the
--   @InstDeclFreeVarsMap@ with these entries removed.
-- We call (getInsts tcs instd_map) when we've completed the declarations
-- for 'tcs'.  The call returns (inst_decls, instd_map'), where
--   inst_decls are the instance declarations all of
--              whose free vars are now defined
--   instd_map' is the inst-decl map with 'tcs' removed from
--               the free-var set
getInsts :: [Name] -> InstDeclFreeVarsMap
         -> ([LInstDecl GhcRn], InstDeclFreeVarsMap)
getInsts bndrs = mapEither pick_me
  where
    pick_me :: (LInstDecl GhcRn, FreeVars)
            -> Either (LInstDecl GhcRn) (LInstDecl GhcRn, FreeVars)
    pick_me (decl, fvs)
      | isEmptyNameSet depleted_fvs = Left decl
      | otherwise                   = Right (decl, depleted_fvs)
      where
        depleted_fvs = delFVs bndrs fvs

{- ******************************************************
*                                                       *
         Renaming a type or class declaration
*                                                       *
****************************************************** -}

rnTyClDecl :: TyClDecl GhcPs
           -> WriterT FreeVars RnM (TyClDecl GhcRn)

-- All flavours of top-level type family declarations ("type family", "newtype
-- family", and "data family")
rnTyClDecl (FamDecl { tcdFam = fam }) = FamDecl noExtField <$> rnFamDecl Nothing fam

rnTyClDecl (SynDecl { tcdLName = tycon, tcdTyVars = tyvars,
                      tcdFixity = fixity, tcdRhs = rhs })
  = WriterT
    do { tycon' <- wrapLocM lookupTopBndrRn tycon
       ; let kvs = extractHsTyRdrTyVarsKindVars rhs
             doc = TySynCtx tycon
       ; traceRn "rntycl-ty" (ppr tycon <+> ppr kvs)
       ; runWriterT $ bindHsQTyVars doc Nothing kvs tyvars $ \ tyvars' _ ->
  [ SynDecl
      { tcdLName = tycon', tcdTyVars = tyvars', tcdFixity = fixity
      , tcdRhs = rhs', tcdSExt = fvs }
  | (rhs', fvs) <- listen $ rnLHsType doc rhs ] }

-- "data", "newtype" declarations
rnTyClDecl (DataDecl
    { tcdLName = tycon, tcdTyVars = tyvars,
      tcdFixity = fixity,
      tcdDataDefn = defn@HsDataDefn{ dd_ND = new_or_data, dd_kindSig = kind_sig} })
  = do { tycon' <- lift $ wrapLocM lookupTopBndrRn tycon
       ; let kvs = extractDataDefnKindVars defn
             doc = TyDataCtx tycon
       ; traceRn "rntycl-data" (ppr tycon <+> ppr kvs)
       ; bindHsQTyVars doc Nothing kvs tyvars $ \ tyvars' no_rhs_kvs ->
  [ DataDecl
      { tcdLName    = tycon'
      , tcdTyVars   = tyvars'
      , tcdFixity   = fixity
      , tcdDataDefn = defn'
      , tcdDExt     = rn_info }
  | (defn', fvs) <- listen $ rnDataDefn doc defn
  , cusk <- data_decl_has_cusk tyvars' new_or_data no_rhs_kvs kind_sig
  , let rn_info = DataDeclRn { tcdDataCusk = cusk, tcdFVs = fvs }
  , () <- traceRn "rndata" (ppr tycon <+> ppr cusk <+> ppr no_rhs_kvs) ] }

rnTyClDecl ClassDecl
  { tcdCtxt = context, tcdLName = lcls, tcdTyVars = tyvars, tcdFixity = fixity
  , tcdFDs = fds, tcdSigs = sigs, tcdMeths = mbinds, tcdATs = ats, tcdATDefs = at_defs
  , tcdDocs = docs } = WriterT
  [ ( ClassDecl
      { tcdCtxt = context', tcdLName = lcls', tcdTyVars = tyvars', tcdFixity = fixity
      , tcdFDs = fds', tcdSigs = sigs', tcdMeths = mbinds', tcdATs = ats', tcdATDefs = at_defs'
      , tcdDocs = docs, tcdCExt = all_fvs }, all_fvs )
  | lcls' <- wrapLocM lookupTopBndrRn lcls
  , let cls' = unLoc lcls'
        kvs = []  -- No scoped kind vars except those in kind signatures on the tyvars

  , -- Tyvars scope over superclass context and method signatures
    ((tyvars', context', fds', ats'), stuff_fvs)
            <- runWriterT $ bindHsQTyVars cls_doc Nothing kvs tyvars $ \ tyvars' _ ->
                  -- Checks for distinct tyvars
          [ (tyvars', context', fds', ats')
          | context' <- rnContext cls_doc context
          , fds'  <- lift $ rnFds fds -- The fundeps have no free variables
          , ats' <- rnATDecls cls' ats ]

  , (at_defs', fv_at_defs) <- runWriterT $ (traverse . wrapLocM) (rnTyFamDefltDecl cls') at_defs

    -- No need to check for duplicate associated type decls
    -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn

  , -- Check the signatures
    -- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
    let sig_rdr_names_w_locs = [op | L _ (ClassOpSig _ False ops _) <- sigs, op <- ops]
  , () <- checkDupRdrNames sig_rdr_names_w_locs
    -- Typechecker is responsible for checking that we only give default-method bindings for
    -- things in this class. The renamer *could* check this for class decls, but can't for
    -- for instance decls.

  , -- The newLocals call is tiresome: given a generic class decl
    --      class C a where
    --        op :: a -> a
    --        op {| x+y |} (Inl a) = ...
    --        op {| x+y |} (Inr b) = ...
    --        op {| a*b |} (a*b)   = ...
    -- we want to name both "x" tyvars with the same unique, so that they are
    -- easy to group together in the typechecker.
    (mbinds', sigs', meth_fvs) <- rnMethodBinds True cls' (hsAllLTyVarNames tyvars') mbinds sigs
    -- No need to check for duplicate method signatures since that is done by
    -- GHC.Rename.Names.extendGlobalRdrEnvRn and the methods are already in scope

  , let all_fvs = meth_fvs `plusFV` stuff_fvs `plusFV` fv_at_defs ]
  where
    cls_doc  = ClassDeclCtx lcls

-- Does the data type declaration include a CUSK?
data_decl_has_cusk :: (HasDynFlags m, Functor m) => LHsQTyVars pass -> NewOrData -> Bool -> Maybe (LHsKind pass') -> m Bool
data_decl_has_cusk tyvars new_or_data no_rhs_kvs kind_sig =
    -- See Note [Unlifted Newtypes and CUSKs], and for a broader
    -- picture, see Note [Implementation of UnliftedNewtypes].
    xoptM LangExt.UnliftedNewtypes <???> \ unlifted_newtypes ->
    let non_cusk_newtype
          | NewType <- new_or_data = unlifted_newtypes && isNothing kind_sig
          | otherwise = False
    -- See Note [CUSKs: complete user-supplied kind signatures] in GHC.Hs.Decls
    in hsTvbAllKinded tyvars && no_rhs_kvs && not non_cusk_newtype

{- Note [Unlifted Newtypes and CUSKs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When unlifted newtypes are enabled, a newtype must have a kind signature
in order to be considered have a CUSK. This is because the flow of
kind inference works differently. Consider:

  newtype Foo = FooC Int

When UnliftedNewtypes is disabled, we decide that Foo has kind
`TYPE 'LiftedRep` without looking inside the data constructor. So, we
can say that Foo has a CUSK. However, when UnliftedNewtypes is enabled,
we fill in the kind of Foo as a metavar that gets solved by unification
with the kind of the field inside FooC (that is, Int, whose kind is
`TYPE 'LiftedRep`). But since we have to look inside the data constructors
to figure out the kind signature of Foo, it does not have a CUSK.

See Note [Implementation of UnliftedNewtypes] for where this fits in to
the broader picture of UnliftedNewtypes.
-}

rnDataDefn :: HsDocContext -> HsDataDefn GhcPs
           -> WriterT FreeVars RnM (HsDataDefn GhcRn)
rnDataDefn doc (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                           , dd_ctxt = context, dd_cons = condecls
                           , dd_kindSig = m_sig, dd_derivs = derivs }) =
  [ HsDataDefn
      { dd_ext = noExtField, dd_ND = new_or_data, dd_cType = cType, dd_ctxt = noLoc []
      , dd_kindSig = m_sig', dd_cons = condecls', dd_derivs = derivs' }
  | () <- lift $ checkTc (h98_style || null (unLoc context)) (badGadtStupidTheta doc)

  , m_sig' <- traverse (rnLHsKind doc) m_sig
  , derivs' <- rn_derivs derivs

    -- For the constructor declarations, drop the LocalRdrEnv
    -- in the GADT case, where the type variables in the declaration
    -- do not scope over the constructor signatures
    -- data T a where { T1 :: forall b. b-> b }
  , let { zap_lcl_env | h98_style = id
                      | otherwise = setLocalRdrEnv emptyLocalRdrEnv }
  , condecls' <- zap_lcl_env $ (traverse . wrapLocM) rnConDecl condecls
    -- No need to check for duplicate constructor decls
    -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn
  ]
  where
    h98_style = case condecls of  -- Note [Stupid theta]
                     (L _ (ConDeclGADT {}))                    : _ -> False
                     (L _ (XConDecl (ConDeclGADTPrefixPs {}))) : _ -> False
                     _                                             -> True

    rn_derivs lds@(L _ ds)
      = do { deriv_strats_ok <- xoptM LangExt.DerivingStrategies
           ; lift $ failIfTc (lengthExceeds ds 1 && not deriv_strats_ok)
               multipleDerivClausesErr
           ; (traverse . traverse) (rnLHsDerivingClause doc) lds }

warnNoDerivStrat :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => Maybe (LDerivStrategy GhcRn) -> SrcSpan -> m ()
warnNoDerivStrat mds loc
  = do { dyn_flags <- getDynFlags
       ; warnIfFlagAt loc Opt_WarnMissingDerivingStrategies (isNothing mds) $
         bool ($+$ deriv_strat_nenabled) id (xopt LangExt.DerivingStrategies dyn_flags)
         no_strat_warning
       }
  where
    no_strat_warning :: SDoc
    no_strat_warning = text "No deriving strategy specified. Did you want stock"
                       <> text ", newtype, or anyclass?"
    deriv_strat_nenabled :: SDoc
    deriv_strat_nenabled = text "Use DerivingStrategies to specify a strategy."

rnLHsDerivingClause :: HsDocContext -> LHsDerivingClause GhcPs
                    -> WriterT FreeVars RnM (LHsDerivingClause GhcRn)
rnLHsDerivingClause doc
                (L loc HsDerivingClause
                              { deriv_clause_ext = noExtField
                              , deriv_clause_strategy = dcs
                              , deriv_clause_tys = L loc' dct })
  = WriterT
  [ ( L loc HsDerivingClause
      { deriv_clause_ext = noExtField
      , deriv_clause_strategy = dcs'
      , deriv_clause_tys = L loc' dct' }
    , fvs )
  | (dcs', dct', fvs) <- rnLDerivStrategy doc dcs $ runWriterT $
    traverse (rnHsSigType doc TypeLevel inf_err) dct
  , () <- warnNoDerivStrat dcs' loc ]
  where
    inf_err = Just (text "Inferred type variables are not allowed")

rnLDerivStrategy :: forall a.
                    HsDocContext
                 -> Maybe (LDerivStrategy GhcPs)
                 -> RnM (a, FreeVars)
                 -> RnM (Maybe (LDerivStrategy GhcRn), a, FreeVars)
rnLDerivStrategy doc mds thing_inside = case mds of
      Nothing -> boring_case Nothing
      Just (L loc ds) -> setSrcSpan loc $ over _1 (Just . L loc) <$> rn_deriv_strat ds
  where
    rn_deriv_strat :: DerivStrategy GhcPs
                   -> RnM (DerivStrategy GhcRn, a, FreeVars)
    rn_deriv_strat ds = do
      let extNeeded :: LangExt.Extension
          extNeeded
            | ViaStrategy{} <- ds
            = LangExt.DerivingVia
            | otherwise
            = LangExt.DerivingStrategies

      unlessXOptM extNeeded $
        failWith $ illegalDerivStrategyErr ds

      case ds of
        StockStrategy    -> boring_case StockStrategy
        AnyclassStrategy -> boring_case AnyclassStrategy
        NewtypeStrategy  -> boring_case NewtypeStrategy
        ViaStrategy via_ty ->
          [ (ViaStrategy via_ty', thing, fvs1 `plusFV` fvs2)
          | (via_ty', fvs1) <- runWriterT $ rnHsSigType doc TypeLevel inf_err via_ty
          , let HsIB { hsib_ext  = via_imp_tvs, hsib_body = via_body } = via_ty'
                (via_exp_tv_bndrs, _, _) = splitLHsSigmaTyInvis via_body
                via_exp_tvs = hsLTyVarNames via_exp_tv_bndrs
                via_tvs = via_imp_tvs ++ via_exp_tvs
          , (thing, fvs2) <- bindLocalNamesFV via_tvs thing_inside ]

    inf_err = Just (text "Inferred type variables are not allowed")

    boring_case :: ds -> RnM (ds, a, FreeVars)
    boring_case ds = thing_inside <???> \ (thing, fvs) -> (ds, thing, fvs)

badGadtStupidTheta :: HsDocContext -> SDoc
badGadtStupidTheta _
  = vcat [text "No context is allowed on a GADT-style data declaration",
          text "(You can put a context on each constructor, though.)"]

illegalDerivStrategyErr :: DerivStrategy GhcPs -> SDoc
illegalDerivStrategyErr ds
  = vcat [ text "Illegal deriving strategy" <> colon <+> derivStrategyName ds
         , text enableStrategy ]

  where
    enableStrategy :: String
    enableStrategy
      | ViaStrategy{} <- ds
      = "Use DerivingVia to enable this extension"
      | otherwise
      = "Use DerivingStrategies to enable this extension"

multipleDerivClausesErr :: SDoc
multipleDerivClausesErr
  = vcat [ text "Illegal use of multiple, consecutive deriving clauses"
         , text "Use DerivingStrategies to allow this" ]

rnFamDecl :: Maybe Name -- Just cls => this FamilyDecl is nested
                        --             inside an *class decl* for cls
                        --             used for associated types
          -> FamilyDecl GhcPs
          -> WriterT FreeVars RnM (FamilyDecl GhcRn)
rnFamDecl mb_cls FamilyDecl
  { fdLName = tycon, fdTyVars = tyvars, fdFixity = fixity
  , fdInfo = info, fdResultSig = res_sig, fdInjectivityAnn = injectivity } =
  [ FamilyDecl
      { fdExt = noExtField, fdLName = tycon', fdTyVars = tyvars', fdFixity = fixity
      , fdInfo = info', fdResultSig = res_sig', fdInjectivityAnn = injectivity' }
  | tycon' <- lift $ wrapLocM lookupTopBndrRn tycon
  , (tyvars', res_sig', injectivity') <- bindHsQTyVars doc mb_cls kvs tyvars $ \ tyvars' _ ->
              [ (tyvars', res_sig', injectivity')
              | let rn_sig = rnFamResultSig doc
              , res_sig' <- wrapLocM rn_sig res_sig
              , injectivity' <- lift $ traverse (rnInjectivityAnn tyvars' res_sig') injectivity ]
  , info' <- rn_info tycon' info ]
  where
     doc = TyFamilyCtx tycon
     kvs = extractRdrKindSigVars res_sig

     ----------------------
     rn_info :: Located Name -> FamilyInfo GhcPs -> WriterT FreeVars RnM (FamilyInfo GhcRn)
     rn_info (L _ fam_name) (ClosedTypeFamily (Just eqns))
       = ClosedTypeFamily . Just <$> (traverse . wrapLocM) (rnTyFamInstEqn NonAssocTyFamEqn (ClosedTyFam tycon fam_name))
                                          -- no class context
                          eqns
     rn_info _ (ClosedTypeFamily Nothing) = pure (ClosedTypeFamily Nothing)
     rn_info _ OpenTypeFamily = pure OpenTypeFamily
     rn_info _ DataFamily     = pure DataFamily

rnFamResultSig :: HsDocContext
               -> FamilyResultSig GhcPs
               -> WriterT FreeVars RnM (FamilyResultSig GhcRn)
rnFamResultSig _ (NoSig _) = pure (NoSig noExtField)
rnFamResultSig doc (KindSig _ kind) = KindSig noExtField <$> rnLHsKind doc kind
rnFamResultSig doc (TyVarSig _ tvbndr)
   = do { -- `TyVarSig` tells us that user named the result of a type family by
          -- writing `= tyvar` or `= (tyvar :: kind)`. In such case we want to
          -- be sure that the supplied result name is not identical to an
          -- already in-scope type variable from an enclosing class.
          --
          --  Example of disallowed declaration:
          --         class C a b where
          --            type F b = a | a -> b
          rdr_env <- getLocalRdrEnv
       ;  let resName = hsLTyVarName tvbndr
       ;  lift $ when (resName `elemLocalRdrEnv` rdr_env) $
          addErrAt (getLoc tvbndr) $
                      hsep [ text "Type variable", quotes (ppr resName) <> comma
                           , text "naming a type family result,"
                           ] $$
                      text "shadows an already bound type variable"

       ; bindLHsTyVarBndr doc Nothing -- This might be a lie, but it's used for
                                      -- scoping checks that are irrelevant here
                          tvbndr $ \ tvbndr' -> writer
         (TyVarSig noExtField tvbndr', unitFV (hsLTyVarName tvbndr')) }

-- Note [Renaming injectivity annotation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- During renaming of injectivity annotation we have to make several checks to
-- make sure that it is well-formed.  At the moment injectivity annotation
-- consists of a single injectivity condition, so the terms "injectivity
-- annotation" and "injectivity condition" might be used interchangeably.  See
-- Note [Injectivity annotation] for a detailed discussion of currently allowed
-- injectivity annotations.
--
-- Checking LHS is simple because the only type variable allowed on the LHS of
-- injectivity condition is the variable naming the result in type family head.
-- Example of disallowed annotation:
--
--     type family Foo a b = r | b -> a
--
-- Verifying RHS of injectivity consists of checking that:
--
--  1. only variables defined in type family head appear on the RHS (kind
--     variables are also allowed).  Example of disallowed annotation:
--
--        type family Foo a = r | r -> b
--
--  2. for associated types the result variable does not shadow any of type
--     class variables. Example of disallowed annotation:
--
--        class Foo a b where
--           type F a = b | b -> a
--
-- Breaking any of these assumptions results in an error.

-- | Rename injectivity annotation. Note that injectivity annotation is just the
-- part after the "|".  Everything that appears before it is renamed in
-- rnFamDecl.
rnInjectivityAnn :: LHsQTyVars GhcRn           -- ^ Type variables declared in
                                               --   type family head
                 -> LFamilyResultSig GhcRn     -- ^ Result signature
                 -> LInjectivityAnn GhcPs      -- ^ Injectivity annotation
                 -> RnM (LInjectivityAnn GhcRn)
rnInjectivityAnn tvBndrs (L _ (TyVarSig _ resTv))
                 (L srcSpan inj@(InjectivityAnn injFrom _injTo))
 = do
   { (injDecl'@(L _ (InjectivityAnn injFrom' injTo')), noRnErrors)
          <- askNoErrs $
             bindLocalNames [hsLTyVarName resTv] $
             -- The return type variable scopes over the injectivity annotation
             -- e.g.   type family F a = (r::*) | r -> a
             L srcSpan <$> injectivityAnnVarsL lookupTypeOccRn inj

   ; let tvNames  = Set.fromList $ hsAllLTyVarNames tvBndrs
         resName  = hsLTyVarName resTv
         -- See Note [Renaming injectivity annotation]
         lhsValid = EQ == stableNameCmp resName (unLoc injFrom')
         rhsValid = Set.fromList (map unLoc injTo') `Set.difference` tvNames

   -- if renaming of type variables ended with errors (eg. there were
   -- not-in-scope variables) don't check the validity of injectivity
   -- annotation. This gives better error messages.
   ; when (noRnErrors && not lhsValid) $
        addErrAt (getLoc injFrom)
              ( vcat [ text $ "Incorrect type variable on the LHS of "
                           ++ "injectivity condition"
              , nest 5
              ( vcat [ text "Expected :" <+> ppr resName
                     , text "Actual   :" <+> ppr injFrom ])])

   ; when (noRnErrors && not (null rhsValid)) $
      do { let errorVars = toList rhsValid
         ; addErrAt srcSpan $ hsep
                        [ text "Unknown type variable" <> plural errorVars
                        , text "on the RHS of injectivity condition:"
                        , interpp'SP errorVars ] }

   ; return injDecl' }

-- We can only hit this case when the user writes injectivity annotation without
-- naming the result:
--
--   type family F a | result -> a
--   type family F a :: * | result -> a
--
-- So we rename injectivity annotation like we normally would except that
-- this time we expect "result" to be reported not in scope by rnTyVar.
rnInjectivityAnn _ _ (L srcSpan inj) =
   setSrcSpan srcSpan $ fst <$> askNoErrs (L srcSpan <$> injectivityAnnVarsL lookupTypeOccRn inj)

{-
Note [Stupid theta]
~~~~~~~~~~~~~~~~~~~
#3850 complains about a regression wrt 6.10 for
     data Show a => T a
There is no reason not to allow the stupid theta if there are no data
constructors.  It's still stupid, but does no harm, and I don't want
to cause programs to break unnecessarily (notably HList).  So if there
are no data constructors we allow h98_style = True
-}


{- *****************************************************
*                                                      *
     Support code for type/data declarations
*                                                      *
***************************************************** -}

---------------
wrongTyFamName :: Name -> Name -> SDoc
wrongTyFamName fam_tc_name eqn_tc_name
  = hang (text "Mismatched type name in type family instance.")
       2 (vcat [ text "Expected:" <+> ppr fam_tc_name
               , text "  Actual:" <+> ppr eqn_tc_name ])

-----------------
rnConDecl :: ConDecl GhcPs -> WriterT FreeVars RnM (ConDecl GhcRn)
rnConDecl decl@ConDeclH98
  { con_name = name, con_ex_tvs = ex_tvs, con_mb_cxt = mcxt, con_args = args, con_doc = mb_doc }
  = do  { ()       <- lift $ addLocM checkConName name
        ; new_name <- lift $ wrapLocM lookupTopBndrRn name

        -- We bind no implicit binders here; this is just like
        -- a nested HsForAllTy.  E.g. consider
        --         data T a = forall (b::k). MkT (...)
        -- The 'k' will already be in scope from the bindHsQTyVars
        -- for the data decl itself. So we'll get
        --         data T {k} a = ...
        -- And indeed we may later discover (a::k).  But that's the
        -- scoping we get.  So no implicit binders at the existential forall

        ; let ctxt = ConDeclCtx [new_name]
        ; bindLHsTyVarBndrs ctxt WarnUnusedForalls
                            Nothing ex_tvs $ \ new_ex_tvs ->
      [ decl
          { con_ext = noExtField, con_name = new_name, con_ex_tvs = new_ex_tvs
          , con_mb_cxt = new_context, con_args = new_args, con_doc = mb_doc }
      | new_context <- rnContext ctxt `traverse` mcxt
      , new_args    <- rnConDeclDetails (unLoc new_name) ctxt args
      , () <- traceRn "rnConDecl (ConDeclH98)" $ ppr name <+> vcat
          [ text "ex_tvs:" <+> ppr ex_tvs
          , text "new_ex_dqtvs':" <+> ppr new_ex_tvs ] ] }

rnConDecl decl@ConDeclGADT
  { con_names = names, con_forall = L _ explicit_forall, con_qvars = explicit_tkvs
  , con_mb_cxt = mcxt, con_args = args, con_res_ty = res_ty, con_doc = mb_doc }
  = do  { lift $ traverse_ (addLocM checkConName) names
        ; new_names <- lift $ traverse (wrapLocM lookupTopBndrRn) names

        ; let theta         = concatMap unLoc mcxt
              arg_tys       = hsConDeclArgTys args

          -- We must ensure that we extract the free tkvs in left-to-right
          -- order of their appearance in the constructor type.
          -- That order governs the order the implicitly-quantified type
          -- variable, and hence the order needed for visible type application
          -- See #14808.
        ; implicit_bndrs <- lift $ forAllOrNothing explicit_forall
            $ extractHsTvBndrs explicit_tkvs
            $ extractHsTysRdrTyVars (theta ++ arg_tys ++ [res_ty])

        ; let ctxt = ConDeclCtx new_names

        ; rnImplicitBndrs Nothing implicit_bndrs $ \ implicit_tkvs ->
          bindLHsTyVarBndrs ctxt WarnUnusedForalls Nothing explicit_tkvs $ \ explicit_tkvs ->
      [ decl
          { con_g_ext = implicit_tkvs, con_names = new_names
          , con_qvars = explicit_tkvs, con_mb_cxt = new_cxt
          , con_args = new_args, con_res_ty = new_res_ty
          , con_doc = mb_doc }
      | new_cxt    <- rnContext ctxt `traverse` mcxt
      , new_args   <- rnConDeclDetails (unLoc (head new_names)) ctxt args
      , new_res_ty <- rnLHsType ctxt res_ty
      , () <- traceRn "rnConDecl (ConDeclGADT)"
            (ppr names $$ ppr implicit_tkvs $$ ppr explicit_tkvs) ] }

-- This case is only used for prefix GADT constructors generated by GHC's
-- parser, where we do not know the argument types until type operator
-- precedence has been resolved. See Note [GADT abstract syntax] in
-- GHC.Hs.Decls for the full story.
rnConDecl (XConDecl ConDeclGADTPrefixPs
  { con_gp_names = names, con_gp_ty = ty, con_gp_doc = mb_doc }) =
  [ ConDeclGADT
      { con_g_ext = implicit_tkvs, con_names = new_names
      , con_forall = lhas_forall, con_qvars = explicit_tkvs
      , con_mb_cxt = mb_cxt, con_args = arg_details
      , con_res_ty = res_ty, con_doc = mb_doc }
  | () <- lift $ traverse_ (addLocM checkConName) names
  , new_names <- lift $ traverse (wrapLocM lookupTopBndrRn) names

  , let ctxt = ConDeclCtx new_names
  , ty' <- rnHsSigType ctxt TypeLevel Nothing ty

    -- Now that operator precedence has been resolved, we can split the
    -- GADT type into its individual components below.
  , let HsIB { hsib_ext = implicit_tkvs, hsib_body = body } = ty'
        (mb_explicit_tkvs, mb_cxt, tau) = splitLHsGADTPrefixTy body
        lhas_forall       = L (getLoc body) $ isJust mb_explicit_tkvs
        explicit_tkvs     = fromMaybe [] mb_explicit_tkvs
        (arg_tys, res_ty) = splitHsFunType tau
        arg_details       = PrefixCon arg_tys
        -- NB: The only possibility here is PrefixCon. RecCon is handled
        -- separately, through ConDeclGADT, from the parser onwards.

    -- Ensure that there are no nested `forall`s or contexts, per
    -- Note [GADT abstract syntax] (Wrinkle: No nested foralls or contexts) in GHC.Hs.Type.
  , () <- lift $ case res_ty of
           L l HsForAllTy { hst_fvf = fvf }
             |  ForallVis <- fvf
             -> setSrcSpan l $ addErr $ withHsDocContext ctxt $ vcat
                [ text "Illegal visible, dependent quantification" <+>
                  text "in the type of a term"
                , text "(GHC does not yet support this)" ]
             |  ForallInvis <- fvf
             -> nested_foralls_contexts_err l ctxt
           L l HsQualTy {}
             -> nested_foralls_contexts_err l ctxt
           _ -> pure ()

  , () <- traceRn "rnConDecl (ConDeclGADTPrefixPs)"
           (ppr names $$ ppr implicit_tkvs $$ ppr explicit_tkvs) ]
  where
    nested_foralls_contexts_err :: SrcSpan -> HsDocContext -> RnM ()
    nested_foralls_contexts_err l ctxt =
      setSrcSpan l $ addErr $ withHsDocContext ctxt $
      text "GADT constructor type signature cannot contain nested"
      <+> quotes forAllLit <> text "s or contexts"

rnConDeclDetails
   :: Name
   -> HsDocContext
   -> HsConDetails (LHsType GhcPs) (Located [LConDeclField GhcPs])
   -> WriterT FreeVars RnM (HsConDetails (LHsType GhcRn) (Located [LConDeclField GhcRn]))
rnConDeclDetails _ doc (PrefixCon tys) = PrefixCon <$> rnLHsType doc `traverse` tys
rnConDeclDetails _ doc (InfixCon ty1 ty2) = InfixCon <$> rnLHsType doc ty1 <*> rnLHsType doc ty2
rnConDeclDetails con doc (RecCon (L l fields))
  = do  { fls <- lift $ lookupConstructorFields con
        ; -- No need to check for duplicate fields
          -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn
          RecCon . L l <$> rnConDeclFields doc fls fields }

-------------------------------------------------

-- | Brings pattern synonym names and also pattern synonym selectors
-- from record pattern synonyms into scope.
extendPatSynEnv :: HsValBinds GhcPs -> MiniFixityEnv -> ([Name] -> RnM a) -> RnM a
extendPatSynEnv val_decls local_fix_env thing = do {
     names_with_fls <- new_ps val_decls
   ; let pat_syn_bndrs = concat [ name: map flSelector fields
                                | (name, fields) <- names_with_fls ]
   ; let avails = map avail pat_syn_bndrs
   ; (gbl_env, lcl_env) <- extendGlobalRdrEnvRn avails local_fix_env

   ; let field_env' = extendNameEnvList (tcg_field_env gbl_env) names_with_fls
         final_gbl_env = gbl_env { tcg_field_env = field_env' }
   ; setEnvs (final_gbl_env, lcl_env) (thing pat_syn_bndrs) }
  where
    new_ps :: HsValBinds GhcPs -> TcM [(Name, [FieldLabel])]
    new_ps (ValBinds _ binds _) = foldrM new_ps' [] binds
    new_ps _ = panic "new_ps"

    new_ps' :: LHsBindLR GhcPs GhcPs
            -> [(Name, [FieldLabel])]
            -> TcM [(Name, [FieldLabel])]
    new_ps' bind names
      | (L bind_loc (PatSynBind _ (PSB { psb_id = L _ n
                                       , psb_args = RecCon as }))) <- bind =
      [ ((bnd_name, flds): names)
      | bnd_name <- newTopSrcBinder (L bind_loc n)
      , let rnames = map recordPatSynSelectorId as
            mkFieldOcc :: Located RdrName -> LFieldOcc GhcPs
            mkFieldOcc (L l name) = L l (FieldOcc noExtField (L l name))
            field_occs =  map mkFieldOcc rnames
      , flds     <- traverse (newRecordSelector False (pure bnd_name)) field_occs ]
      | L bind_loc (PatSynBind _ (PSB { psb_id = L _ n})) <- bind
      = newTopSrcBinder (L bind_loc n) <???> \ bnd_name -> ((bnd_name, []): names)
      | otherwise
      = return names

{-
*********************************************************
*                                                      *
\subsection{Support code to rename types}
*                                                      *
*********************************************************
-}

rnFds
 :: (Traversable r, Traversable s1, Traversable s2, Traversable t1, Traversable t2)
 => r (Located (s1 (t1 RdrName), s2 (t2 RdrName))) -> RnM (r (Located (s1 (t1 Name), s2 (t2 Name))))
rnFds = (traverse . wrapLocM) \ (x, y) ->
    (,) <$> (traverse . traverse) lookupOccRn x <*> (traverse . traverse) lookupOccRn y

{-
*********************************************************
*                                                      *
        findSplice
*                                                      *
*********************************************************

This code marches down the declarations, looking for the first
Template Haskell splice.  As it does so it
        a) groups the declarations into a HsGroup
        b) runs any top-level quasi-quotes
-}

findSplice :: [LHsDecl GhcPs]
           -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
findSplice = addl emptyRdrGroup

addl :: HsGroup GhcPs -> [LHsDecl GhcPs]
     -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
-- This stuff reverses the declarations (again) but it doesn't matter
addl gp []           = return (gp, Nothing)
addl gp (L l d : ds) = add gp l d ds

add :: HsGroup GhcPs -> SrcSpan -> HsDecl GhcPs -> [LHsDecl GhcPs]
    -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
add = slipl \ l -> \ case

    -- #10047: Declaration QuasiQuoters are expanded immediately, causing np group split
    SpliceD _ (SpliceDecl _ (L _ qq@HsQuasiQuote{}) _) -> \ gp ds ->
        do { (ds', _) <- runWriterT $ rnTopSpliceDecls qq; addl gp (ds' ++ ds) }

    SpliceD _ splice@(SpliceDecl _ _ flag) -> \ gp ds ->
        (gp, Just (splice, ds)) <$
        -- We've found a top-level splice.  If it is an *implicit* one
        -- (i.e. a naked top level expression)
        case flag of
            ExplicitSplice -> return ()
            ImplicitSplice -> do { th_on <- xoptM LangExt.TemplateHaskell
                                 ; unless th_on $ setSrcSpan l $
                                   failWith badImplicitSplice }

    -- Class declarations: added to the TyClGroup
    TyClD _ d -> addl . (over hs_tycldsL . addToTyClGroup . over group_tycldsL) (L l d :)

    -- Signatures: fixity sigs go a different place than all others
    SigD _ (FixSig _ f) -> addl . over hs_fixdsL (L l f :)

    -- Standalone kind signatures: added to the TyClGroup
    KindSigD _ s -> addl . (over hs_tycldsL . addToTyClGroup . over group_kisigsL) (L l s :)

    SigD _ d -> addl . over hs_valdsL (add_sig $ L l d)

    -- Value declarations: use add_bind
    ValD _ d -> addl . over hs_valdsL (add_bind $ L l d)

    -- Role annotations: added to the TyClGroup
    RoleAnnotD _ d -> addl . (over hs_tycldsL . addToTyClGroup . over group_rolesL) (L l d :)

    -- NB instance declarations go into TyClGroups. We throw them into the first group, just as
    -- we do for the TyClD case. The renamer will go on to group and order them later.
    InstD _ d -> addl . (over hs_tycldsL . addToTyClGroup . over group_instdsL) (L l d :)

    -- The rest are routine
    DerivD _ d -> addl . over hs_derivdsL (L l d :)
    DefD _ d -> addl . over hs_defdsL (L l d :)
    ForD _ d -> addl . over hs_fordsL (L l d :)
    WarningD _ d -> addl . over hs_warndsL (L l d :)
    AnnD _ d -> addl . over hs_anndsL (L l d :)
    RuleD _ d -> addl . over hs_ruledsL (L l d :)
    DocD _ d -> addl . over hs_docsL (L l d :)
  where
    badImplicitSplice = text "Parse error: module header, import declaration"
                     $$ text "or top-level declaration expected."
                     -- The compiler should suggest the above, and not using
                     -- TemplateHaskell since the former suggestion is more
                     -- relevant to the larger base of users.
                     -- See #12146 for discussion.

addToTyClGroup
 :: (TyClGroup (GhcPass p) -> TyClGroup (GhcPass p))
 -> [TyClGroup (GhcPass p)] -> [TyClGroup (GhcPass p)]
addToTyClGroup f = \ case
  [] -> [f emptyTyClGroup]
  g:gs -> f g:gs

emptyTyClGroup :: TyClGroup (GhcPass p)
emptyTyClGroup = TyClGroup
  { group_ext    = noExtField
  , group_tyclds = []
  , group_kisigs = []
  , group_roles  = []
  , group_instds = []
  }

add_bind :: LHsBind a -> HsValBinds a -> HsValBinds a
add_bind b (ValBinds x bs sigs) = ValBinds x (bs `snocBag` b) sigs
add_bind _ (XValBindsLR {})     = panic "GHC.Rename.Module.add_bind"

add_sig :: LSig (GhcPass a) -> HsValBinds (GhcPass a) -> HsValBinds (GhcPass a)
add_sig s (ValBinds x bs sigs) = ValBinds x bs (s:sigs)
add_sig _ (XValBindsLR {})     = panic "GHC.Rename.Module.add_sig"
