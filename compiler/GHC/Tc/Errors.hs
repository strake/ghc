{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Tc.Errors(
       reportUnsolved, reportAllUnsolved, warnAllUnsolved,
       warnDefaulting,

       solverDepthErrorTcS
  ) where

import GHC.Prelude

import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.Unify( occCheckForErrors, MetaTyVarUpdateResult(..) )
import GHC.Tc.Utils.Env( tcInitTidyEnv )
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Origin
import GHC.Rename.Unbound ( unknownNameSuggestions )
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr  ( pprTyVars, pprWithExplicitKindsWhen, pprSourceTyCon, pprWithTYPE )
import GHC.Core.Unify     ( tcMatchTys )
import GHC.Unit.Module
import GHC.Tc.Instance.Family
import GHC.Core.FamInstEnv ( flattenTys )
import GHC.Tc.Utils.Instantiate
import GHC.Core.InstEnv
import GHC.Core.TyCon
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.EvTerm
import GHC.Hs.Binds ( PatSynBind(..) )
import GHC.Types.Name
import GHC.Types.Name.Reader ( lookupGRE_Name, GlobalRdrEnv, mkRdrUnqual )
import GHC.Builtin.Names ( typeableClassName )
import GHC.Types.Id
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Name.Set
import GHC.Data.Bag
import GHC.Utils.Error  ( ErrMsg, errDoc, pprLocErrMsg )
import GHC.Types.Basic
import GHC.Core.ConLike ( ConLike(..))
import GHC.Utils.Misc
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Types.SrcLoc
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Data.List.SetOps ( equivClasses )
import GHC.Data.Maybe
import qualified GHC.LanguageExtensions as LangExt
import GHC.Utils.FV ( fvVarList, unionFV )

import Control.Monad    ( when )
import Data.Foldable    ( toList )
import Data.List        ( partition, mapAccumL, nub, sortBy, unfoldr )

import {-# SOURCE #-} GHC.Tc.Errors.Hole ( findValidHoleFits )

-- import Data.Semigroup   ( Semigroup )
import qualified Data.Semigroup as Semigroup


{-
************************************************************************
*                                                                      *
\section{Errors and contexts}
*                                                                      *
************************************************************************

ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

Note [Deferring coercion errors to runtime]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While developing, sometimes it is desirable to allow compilation to succeed even
if there are type errors in the code. Consider the following case:

  module Main where

  a :: Int
  a = 'a'

  main = print "b"

Even though `a` is ill-typed, it is not used in the end, so if all that we're
interested in is `main` it is handy to be able to ignore the problems in `a`.

Since we treat type equalities as evidence, this is relatively simple. Whenever
we run into a type mismatch in GHC.Tc.Utils.Unify, we normally just emit an error. But it
is always safe to defer the mismatch to the main constraint solver. If we do
that, `a` will get transformed into

  co :: Int ~ Char
  co = ...

  a :: Int
  a = 'a' `cast` co

The constraint solver would realize that `co` is an insoluble constraint, and
emit an error with `reportUnsolved`. But we can also replace the right-hand side
of `co` with `error "Deferred type error: Int ~ Char"`. This allows the program
to compile, and it will run fine unless we evaluate `a`. This is what
`deferErrorsToRuntime` does.

It does this by keeping track of which errors correspond to which coercion
in GHC.Tc.Errors. GHC.Tc.Errors.reportTidyWanteds does not print the errors
and does not fail if -fdefer-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.
-}

-- | Report unsolved goals as errors or warnings. We may also turn some into
-- deferred run-time errors if `-fdefer-type-errors` is on.
reportUnsolved :: WantedConstraints -> TcM (Bag EvBind)
reportUnsolved wanted
  = do { binds_var <- newTcEvBinds
       ; defer_errors <- goptM Opt_DeferTypeErrors
       ; warn_errors <- woptM Opt_WarnDeferredTypeErrors -- implement #10283
       ; let type_errors | not defer_errors = TypeError
                         | warn_errors      = TypeWarn (Reason Opt_WarnDeferredTypeErrors)
                         | otherwise        = TypeDefer

       ; defer_holes <- goptM Opt_DeferTypedHoles
       ; warn_holes  <- woptM Opt_WarnTypedHoles
       ; let expr_holes | not defer_holes = HoleError
                        | warn_holes      = HoleWarn
                        | otherwise       = HoleDefer

       ; partial_sigs      <- xoptM LangExt.PartialTypeSignatures
       ; warn_partial_sigs <- woptM Opt_WarnPartialTypeSignatures
       ; let type_holes | not partial_sigs  = HoleError
                        | warn_partial_sigs = HoleWarn
                        | otherwise         = HoleDefer

       ; defer_out_of_scope <- goptM Opt_DeferOutOfScopeVariables
       ; warn_out_of_scope <- woptM Opt_WarnDeferredOutOfScopeVariables
       ; let out_of_scope_holes | not defer_out_of_scope = HoleError
                                | warn_out_of_scope      = HoleWarn
                                | otherwise              = HoleDefer

       ; report_unsolved type_errors expr_holes
                         type_holes out_of_scope_holes
                         binds_var wanted

       ; ev_binds <- getTcEvBindsMap binds_var
       ; return (evBindMapBinds ev_binds)}

-- | Report *all* unsolved goals as errors, even if -fdefer-type-errors is on
-- However, do not make any evidence bindings, because we don't
-- have any convenient place to put them.
-- NB: Type-level holes are OK, because there are no bindings.
-- See Note [Deferring coercion errors to runtime]
-- Used by solveEqualities for kind equalities
--      (see Note [Fail fast on kind errors] in "GHC.Tc.Solver")
-- and for simplifyDefault.
reportAllUnsolved :: WantedConstraints -> TcM ()
reportAllUnsolved wanted
  = do { ev_binds <- newNoTcEvBinds

       ; partial_sigs      <- xoptM LangExt.PartialTypeSignatures
       ; warn_partial_sigs <- woptM Opt_WarnPartialTypeSignatures
       ; let type_holes | not partial_sigs  = HoleError
                        | warn_partial_sigs = HoleWarn
                        | otherwise         = HoleDefer

       ; report_unsolved TypeError HoleError type_holes HoleError
                         ev_binds wanted }

-- | Report all unsolved goals as warnings (but without deferring any errors to
-- run-time). See Note [Safe Haskell Overlapping Instances Implementation] in
-- "GHC.Tc.Solver"
warnAllUnsolved :: WantedConstraints -> TcM ()
warnAllUnsolved wanted
  = do { ev_binds <- newTcEvBinds
       ; report_unsolved (TypeWarn NoReason) HoleWarn HoleWarn HoleWarn
                         ev_binds wanted }

-- | Report unsolved goals as errors or warnings.
report_unsolved :: TypeErrorChoice   -- Deferred type errors
                -> HoleChoice        -- Expression holes
                -> HoleChoice        -- Type holes
                -> HoleChoice        -- Out of scope holes
                -> EvBindsVar        -- cec_binds
                -> WantedConstraints -> TcM ()
report_unsolved type_errors expr_holes
    type_holes out_of_scope_holes binds_var wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = do { traceTc "reportUnsolved {" $
         vcat [ text "type errors:" <+> ppr type_errors
              , text "expr holes:" <+> ppr expr_holes
              , text "type holes:" <+> ppr type_holes
              , text "scope holes:" <+> ppr out_of_scope_holes ]
       ; traceTc "reportUnsolved (before zonking and tidying)" (ppr wanted)

       ; wanted <- zonkWC wanted   -- Zonk to reveal all information
            -- If we are deferring we are going to need /all/ evidence around,
            -- including the evidence produced by unflattening (zonkWC)
       ; let tidy_env = tidyFreeTyCoVars emptyTidyEnv free_tvs
             free_tvs = tyCoVarsOfWCList wanted

       ; traceTc "reportUnsolved (after zonking):" $
         vcat [ text "Free tyvars:" <+> pprTyVars free_tvs
              , text "Tidy env:" <+> ppr tidy_env
              , text "Wanted:" <+> ppr wanted ]

       ; warn_redundant <- woptM Opt_WarnRedundantConstraints
       ; let err_ctxt = CEC { cec_encl  = []
                            , cec_tidy  = tidy_env
                            , cec_defer_type_errors = type_errors
                            , cec_expr_holes = expr_holes
                            , cec_type_holes = type_holes
                            , cec_out_of_scope_holes = out_of_scope_holes
                            , cec_suppress = insolubleWC wanted
                                 -- See Note [Suppressing error messages]
                                 -- Suppress low-priority errors if there
                                 -- are insoluble errors anywhere;
                                 -- See #15539 and c.f. setting ic_status
                                 -- in GHC.Tc.Solver.setImplicationStatus
                            , cec_warn_redundant = warn_redundant
                            , cec_binds    = binds_var }

       ; tc_lvl <- getTcLevel
       ; reportWanteds err_ctxt tc_lvl wanted
       ; traceTc "reportUnsolved }" empty }

--------------------------------------------
--      Internal functions
--------------------------------------------

-- | An error Report collects messages categorised by their importance.
-- See Note [Error report] for details.
data Report
  = Report { report_important :: [SDoc]
           , report_relevant_bindings :: [SDoc]
           , report_valid_hole_fits :: [SDoc]
           }

instance Outputable Report where   -- Debugging only
  ppr (Report { report_important = imp
              , report_relevant_bindings = rel
              , report_valid_hole_fits = val })
    = vcat [ text "important:" <+> vcat imp
           , text "relevant:"  <+> vcat rel
           , text "valid:"  <+> vcat val ]

{- Note [Error report]
The idea is that error msgs are divided into three parts: the main msg, the
context block (\"In the second argument of ...\"), and the relevant bindings
block, which are displayed in that order, with a mark to divide them.  The
idea is that the main msg ('report_important') varies depending on the error
in question, but context and relevant bindings are always the same, which
should simplify visual parsing.

The context is added when the Report is passed off to 'mkErrorReport'.
Unfortunately, unlike the context, the relevant bindings are added in
multiple places so they have to be in the Report.
-}

instance Semigroup Report where
    Report a1 b1 c1 <> Report a2 b2 c2 = Report (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

instance Monoid Report where
    mempty = Report [] [] []
    mappend = (Semigroup.<>)

-- | Put a doc into the important msgs block.
important :: SDoc -> Report
important doc = mempty { report_important = [doc] }

-- | Put a doc into the relevant bindings block.
mk_relevant_bindings :: SDoc -> Report
mk_relevant_bindings doc = mempty { report_relevant_bindings = [doc] }

-- | Put a doc into the valid hole fits block.
valid_hole_fits :: SDoc -> Report
valid_hole_fits docs = mempty { report_valid_hole_fits = [docs] }

data TypeErrorChoice   -- What to do for type errors found by the type checker
  = TypeError     -- A type error aborts compilation with an error message
  | TypeWarn WarnReason
                  -- A type error is deferred to runtime, plus a compile-time warning
                  -- The WarnReason should usually be (Reason Opt_WarnDeferredTypeErrors)
                  -- but it isn't for the Safe Haskell Overlapping Instances warnings
                  -- see warnAllUnsolved
  | TypeDefer     -- A type error is deferred to runtime; no error or warning at compile time

data HoleChoice
  = HoleError     -- A hole is a compile-time error
  | HoleWarn      -- Defer to runtime, emit a compile-time warning
  | HoleDefer     -- Defer to runtime, no warning

instance Outputable HoleChoice where
  ppr HoleError = text "HoleError"
  ppr HoleWarn  = text "HoleWarn"
  ppr HoleDefer = text "HoleDefer"

instance Outputable TypeErrorChoice  where
  ppr TypeError         = text "TypeError"
  ppr (TypeWarn reason) = text "TypeWarn" <+> ppr reason
  ppr TypeDefer         = text "TypeDefer"

data ReportErrCtxt
    = CEC { cec_encl :: [Implication]  -- Enclosing implications
                                       --   (innermost first)
                                       -- ic_skols and givens are tidied, rest are not
          , cec_tidy  :: TidyEnv

          , cec_binds :: EvBindsVar    -- Make some errors (depending on cec_defer)
                                       -- into warnings, and emit evidence bindings
                                       -- into 'cec_binds' for unsolved constraints

          , cec_defer_type_errors :: TypeErrorChoice -- Defer type errors until runtime

          -- cec_expr_holes is a union of:
          --   cec_type_holes - a set of typed holes: '_', '_a', '_foo'
          --   cec_out_of_scope_holes - a set of variables which are
          --                            out of scope: 'x', 'y', 'bar'
          , cec_expr_holes :: HoleChoice           -- Holes in expressions
          , cec_type_holes :: HoleChoice           -- Holes in types
          , cec_out_of_scope_holes :: HoleChoice   -- Out of scope holes

          , cec_warn_redundant :: Bool    -- True <=> -Wredundant-constraints

          , cec_suppress :: Bool    -- True <=> More important errors have occurred,
                                    --          so create bindings if need be, but
                                    --          don't issue any more errors/warnings
                                    -- See Note [Suppressing error messages]
      }

instance Outputable ReportErrCtxt where
  ppr (CEC { cec_binds              = bvar
           , cec_defer_type_errors  = dte
           , cec_expr_holes         = eh
           , cec_type_holes         = th
           , cec_out_of_scope_holes = osh
           , cec_warn_redundant     = wr
           , cec_suppress           = sup })
    = text "CEC" <+> braces (vcat
         [ text "cec_binds"              <+> equals <+> ppr bvar
         , text "cec_defer_type_errors"  <+> equals <+> ppr dte
         , text "cec_expr_holes"         <+> equals <+> ppr eh
         , text "cec_type_holes"         <+> equals <+> ppr th
         , text "cec_out_of_scope_holes" <+> equals <+> ppr osh
         , text "cec_warn_redundant"     <+> equals <+> ppr wr
         , text "cec_suppress"           <+> equals <+> ppr sup ])

-- | Returns True <=> the ReportErrCtxt indicates that something is deferred
deferringAnyBindings :: ReportErrCtxt -> Bool
  -- Don't check cec_type_holes, as these don't cause bindings to be deferred
deferringAnyBindings (CEC { cec_defer_type_errors  = TypeError
                          , cec_expr_holes         = HoleError
                          , cec_out_of_scope_holes = HoleError }) = False
deferringAnyBindings _                                            = True

-- | Transforms a 'ReportErrCtxt' into one that does not defer any bindings
-- at all.
noDeferredBindings :: ReportErrCtxt -> ReportErrCtxt
noDeferredBindings ctxt = ctxt { cec_defer_type_errors  = TypeError
                               , cec_expr_holes         = HoleError
                               , cec_out_of_scope_holes = HoleError }

{- Note [Suppressing error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The cec_suppress flag says "don't report any errors".  Instead, just create
evidence bindings (as usual).  It's used when more important errors have occurred.

Specifically (see reportWanteds)
  * If there are insoluble Givens, then we are in unreachable code and all bets
    are off.  So don't report any further errors.
  * If there are any insolubles (eg Int~Bool), here or in a nested implication,
    then suppress errors from the simple constraints here.  Sometimes the
    simple-constraint errors are a knock-on effect of the insolubles.

This suppression behaviour is controlled by the Bool flag in
ReportErrorSpec, as used in reportWanteds.

But we need to take care: flags can turn errors into warnings, and we
don't want those warnings to suppress subsequent errors (including
suppressing the essential addTcEvBind for them: #15152). So in
tryReporter we use askNoErrs to see if any error messages were
/actually/ produced; if not, we don't switch on suppression.

A consequence is that warnings never suppress warnings, so turning an
error into a warning may allow subsequent warnings to appear that were
previously suppressed.   (e.g. partial-sigs/should_fail/T14584)
-}

reportImplic :: ReportErrCtxt -> Implication -> TcM ()
reportImplic ctxt implic@(Implic { ic_skols = tvs, ic_telescope = m_telescope
                                 , ic_given = given
                                 , ic_wanted = wanted, ic_binds = evb
                                 , ic_status = status, ic_info = info
                                 , ic_tclvl = tc_lvl })
  | BracketSkol <- info
  , not insoluble
  = return ()        -- For Template Haskell brackets report only
                     -- definite errors. The whole thing will be re-checked
                     -- later when we plug it in, and meanwhile there may
                     -- certainly be un-satisfied constraints

  | otherwise
  = do { traceTc "reportImplic" (ppr implic')
       ; reportWanteds ctxt' tc_lvl wanted
       ; when (cec_warn_redundant ctxt) $
         warnRedundantConstraints ctxt' tcl_env info' dead_givens
       ; when bad_telescope $ reportBadTelescope ctxt tcl_env m_telescope tvs }
  where
    tcl_env      = ic_env implic
    insoluble    = isInsolubleStatus status
    (env1, tvs') = mapAccumL tidyVarBndr (cec_tidy ctxt) tvs
    info'        = tidySkolemInfo env1 info
    implic' = implic { ic_skols = tvs'
                     , ic_given = map (tidyEvVar env1) given
                     , ic_info  = info' }
    ctxt1 | CoEvBindsVar{} <- evb    = noDeferredBindings ctxt
          | otherwise                = ctxt
          -- If we go inside an implication that has no term
          -- evidence (e.g. unifying under a forall), we can't defer
          -- type errors.  You could imagine using the /enclosing/
          -- bindings (in cec_binds), but that may not have enough stuff
          -- in scope for the bindings to be well typed.  So we just
          -- switch off deferred type errors altogether.  See #14605.

    ctxt' = ctxt1 { cec_tidy     = env1
                  , cec_encl     = implic' : cec_encl ctxt

                  , cec_suppress = insoluble || cec_suppress ctxt
                        -- Suppress inessential errors if there
                        -- are insolubles anywhere in the
                        -- tree rooted here, or we've come across
                        -- a suppress-worthy constraint higher up (#11541)

                  , cec_binds    = evb }

    dead_givens = case status of
                    IC_Solved { ics_dead = dead } -> dead
                    _                             -> []

    bad_telescope = case status of
              IC_BadTelescope -> True
              _               -> False

warnRedundantConstraints :: ReportErrCtxt -> TcLclEnv -> SkolemInfo -> [EvVar] -> TcM ()
-- See Note [Tracking redundant constraints] in GHC.Tc.Solver
warnRedundantConstraints ctxt env info ev_vars
 | null redundant_evs
 = return ()

 | SigSkol {} <- info
 = setLclEnv env $  -- We want to add "In the type signature for f"
                    -- to the error context, which is a bit tiresome
   addErrCtxt (text "In" <+> ppr info) $
   do { env <- getLclEnv
      ; msg <- mkErrorReport ctxt env (important doc)
      ; reportWarning (Reason Opt_WarnRedundantConstraints) msg }

 | otherwise  -- But for InstSkol there already *is* a surrounding
              -- "In the instance declaration for Eq [a]" context
              -- and we don't want to say it twice. Seems a bit ad-hoc
 = do { msg <- mkErrorReport ctxt env (important doc)
      ; reportWarning (Reason Opt_WarnRedundantConstraints) msg }
 where
   doc = text "Redundant constraint" <> plural redundant_evs <> colon
         <+> pprEvVarTheta redundant_evs

   redundant_evs =
       filterOut is_type_error $
       case info of -- See Note [Redundant constraints in instance decls]
         InstSkol -> filterOut (improving . idType) ev_vars
         _        -> ev_vars

   -- See #15232
   is_type_error = isJust . userTypeError_maybe . idType

   improving pred -- (transSuperClasses p) does not include p
     = any isImprovementPred (pred : transSuperClasses pred)

reportBadTelescope :: ReportErrCtxt -> TcLclEnv -> Maybe SDoc -> [TcTyVar] -> TcM ()
reportBadTelescope ctxt env (Just telescope) skols
  = do { msg <- mkErrorReport ctxt env (important doc)
       ; reportError msg }
  where
    doc = hang (text "These kind and type variables:" <+> telescope $$
                text "are out of dependency order. Perhaps try this ordering:")
             2 (pprTyVars sorted_tvs)

    sorted_tvs = scopedSort skols

reportBadTelescope _ _ Nothing skols
  = pprPanic "reportBadTelescope" (ppr skols)

{- Note [Redundant constraints in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For instance declarations, we don't report unused givens if
they can give rise to improvement.  Example (#10100):
    class Add a b ab | a b -> ab, a ab -> b
    instance Add Zero b b
    instance Add a b ab => Add (Succ a) b (Succ ab)
The context (Add a b ab) for the instance is clearly unused in terms
of evidence, since the dictionary has no fields.  But it is still
needed!  With the context, a wanted constraint
   Add (Succ Zero) beta (Succ Zero)
we will reduce to (Add Zero beta Zero), and thence we get beta := Zero.
But without the context we won't find beta := Zero.

This only matters in instance declarations..
-}

reportWanteds :: ReportErrCtxt -> TcLevel -> WantedConstraints -> TcM ()
reportWanteds ctxt tc_lvl (WC { wc_simple = simples, wc_impl = implics
                              , wc_holes = holes })
  = do { traceTc "reportWanteds" (vcat [ text "Simples =" <+> ppr simples
                                       , text "Suppress =" <+> ppr (cec_suppress ctxt)
                                       , text "tidy_cts =" <+> ppr tidy_cts
                                       , text "tidy_holes = " <+> ppr tidy_holes ])

         -- First, deal with any out-of-scope errors:
       ; let (out_of_scope, other_holes) = partition isOutOfScopeHole tidy_holes
               -- don't suppress out-of-scope errors
             ctxt_for_scope_errs = ctxt { cec_suppress = False }
       ; (_, no_out_of_scope) <- askNoErrs $
                                 reportHoles tidy_cts ctxt_for_scope_errs out_of_scope

         -- Next, deal with things that are utterly wrong
         -- Like Int ~ Bool (incl nullary TyCons)
         -- or  Int ~ t a   (AppTy on one side)
         -- These /ones/ are not suppressed by the incoming context
         -- (but will be by out-of-scope errors)
       ; let ctxt_for_insols = ctxt { cec_suppress = not no_out_of_scope }
       ; reportHoles tidy_cts ctxt_for_insols other_holes
          -- holes never suppress

       ; (ctxt1, cts1) <- tryReporters ctxt_for_insols report1 tidy_cts

         -- Now all the other constraints.  We suppress errors here if
         -- any of the first batch failed, or if the enclosing context
         -- says to suppress
       ; let ctxt2 = ctxt { cec_suppress = cec_suppress ctxt || cec_suppress ctxt1 }
       ; (_, leftovers) <- tryReporters ctxt2 report2 cts1
       ; massertPpr (null leftovers) (ppr leftovers)

            -- All the Derived ones have been filtered out of simples
            -- by the constraint solver. This is ok; we don't want
            -- to report unsolved Derived goals as errors
            -- See Note [Do not report derived but soluble errors]

     ; mapBagM_ (reportImplic ctxt2) implics }
            -- NB ctxt2: don't suppress inner insolubles if there's only a
            -- wanted insoluble here; but do suppress inner insolubles
            -- if there's a *given* insoluble here (= inaccessible code)
 where
    env = cec_tidy ctxt
    tidy_cts   = bagToList (mapBag (tidyCt env)   simples)
    tidy_holes = bagToList (mapBag (tidyHole env) holes)

    -- report1: ones that should *not* be suppressed by
    --          an insoluble somewhere else in the tree
    -- It's crucial that anything that is considered insoluble
    -- (see GHC.Tc.Utils.insolubleCt) is caught here, otherwise
    -- we might suppress its error message, and proceed on past
    -- type checking to get a Lint error later
    report1 = [ ("custom_error", unblocked is_user_type_error, True,  mkUserTypeErrorReporter)

              , given_eq_spec
              , ("insoluble2",   unblocked utterly_wrong,  True, mkGroupReporter mkEqErr)
              , ("skolem eq1",   unblocked very_wrong,     True, mkSkolReporter)
              , ("skolem eq2",   unblocked skolem_eq,      True, mkSkolReporter)
              , ("non-tv eq",    unblocked non_tv_eq,      True, mkSkolReporter)

                  -- The only remaining equalities are alpha ~ ty,
                  -- where alpha is untouchable; and representational equalities
                  -- Prefer homogeneous equalities over hetero, because the
                  -- former might be holding up the latter.
                  -- See Note [Equalities with incompatible kinds] in GHC.Tc.Solver.Canonical
              , ("Homo eqs",      unblocked is_homo_equality, True,  mkGroupReporter mkEqErr)
              , ("Other eqs",     unblocked is_equality,      True,  mkGroupReporter mkEqErr)
              , ("Blocked eqs",   is_equality,           False, mkSuppressReporter mkBlockedEqErr)]

    -- report2: we suppress these if there are insolubles elsewhere in the tree
    report2 = [ ("Implicit params", is_ip,           False, mkGroupReporter mkIPErr)
              , ("Irreds",          is_irred,        False, mkGroupReporter mkIrredErr)
              , ("Dicts",           is_dict,         False, mkGroupReporter mkDictErr) ]

    -- also checks to make sure the constraint isn't BlockedCIS
    -- See TcCanonical Note [Equalities with incompatible kinds], (4)
    unblocked :: (Ct -> Pred -> Bool) -> Ct -> Pred -> Bool
    unblocked _ (CIrredCan { cc_status = BlockedCIS }) _ = False
    unblocked checker ct pred = checker ct pred

    -- rigid_nom_eq, rigid_nom_tv_eq,
    is_dict, is_equality, is_ip, is_irred :: Ct -> Pred -> Bool

    is_given_eq ct pred
       | EqPred {} <- pred = arisesFromGivens ct
       | otherwise         = False
       -- I think all given residuals are equalities

    -- Things like (Int ~N Bool)
    utterly_wrong _ (EqPred NomEq ty1 ty2) = isRigidTy ty1 && isRigidTy ty2
    utterly_wrong _ _                      = False

    -- Things like (a ~N Int)
    very_wrong _ (EqPred NomEq ty1 ty2) = isSkolemTy tc_lvl ty1 && isRigidTy ty2
    very_wrong _ _                      = False

    -- Things like (a ~N b) or (a  ~N  F Bool)
    skolem_eq _ (EqPred NomEq ty1 _) = isSkolemTy tc_lvl ty1
    skolem_eq _ _                    = False

    -- Things like (F a  ~N  Int)
    non_tv_eq _ (EqPred NomEq ty1 _) = not (isTyVarTy ty1)
    non_tv_eq _ _                    = False

    is_user_type_error ct _ = isUserTypeErrorCt ct

    is_homo_equality _ (EqPred _ ty1 ty2) = tcTypeKind ty1 `tcEqType` tcTypeKind ty2
    is_homo_equality _ _                  = False

    is_equality _ (EqPred {}) = True
    is_equality _ _           = False

    is_dict _ (ClassPred {}) = True
    is_dict _ _              = False

    is_ip _ (ClassPred cls _) = isIPClass cls
    is_ip _ _                 = False

    is_irred _ (IrredPred {}) = True
    is_irred _ _              = False

    given_eq_spec  -- See Note [Given errors]
      | has_gadt_match (cec_encl ctxt)
      = ("insoluble1a", is_given_eq, True,  mkGivenErrorReporter)
      | otherwise
      = ("insoluble1b", is_given_eq, False, ignoreErrorReporter)
          -- False means don't suppress subsequent errors
          -- Reason: we don't report all given errors
          --         (see mkGivenErrorReporter), and we should only suppress
          --         subsequent errors if we actually report this one!
          --         #13446 is an example

    -- See Note [Given errors]
    has_gadt_match [] = False
    has_gadt_match (implic : implics)
      | PatSkol {} <- ic_info implic
      , not (ic_no_eqs implic)
      , ic_warn_inaccessible implic
          -- Don't bother doing this if -Winaccessible-code isn't enabled.
          -- See Note [Avoid -Winaccessible-code when deriving] in GHC.Tc.TyCl.Instance.
      = True
      | otherwise
      = has_gadt_match implics

---------------
isSkolemTy :: TcLevel -> Type -> Bool
-- The type is a skolem tyvar
isSkolemTy tc_lvl ty
  | Just tv <- getTyVar_maybe ty
  =  isSkolemTyVar tv
  || (isTyVarTyVar tv && isTouchableMetaTyVar tc_lvl tv)
     -- The last case is for touchable TyVarTvs
     -- we postpone untouchables to a latter test (too obscure)

  | otherwise
  = False

isTyFun_maybe :: Type -> Maybe TyCon
isTyFun_maybe ty = case tcSplitTyConApp_maybe ty of
                      Just (tc,_) | isTypeFamilyTyCon tc -> Just tc
                      _ -> Nothing

--------------------------------------------
--      Reporters
--------------------------------------------

type Reporter
  = ReportErrCtxt -> [Ct] -> TcM ()
type ReporterSpec
  = ( String                     -- Name
    , Ct -> Pred -> Bool         -- Pick these ones
    , Bool                       -- True <=> suppress subsequent reporters
    , Reporter)                  -- The reporter itself

mkSkolReporter :: Reporter
-- Suppress duplicates with either the same LHS, or same location
mkSkolReporter ctxt cts
  = mapM_ (reportGroup mkEqErr ctxt) (group cts)
  where
     group [] = []
     group (ct:cts) = (ct : yeses) : group noes
        where
          (yeses, noes) = partition (group_with ct) cts

     group_with ct1 ct2
       | EQ <- cmp_loc ct1 ct2 = True
       | eq_lhs_type   ct1 ct2 = True
       | otherwise             = False

reportHoles :: [Ct]  -- other (tidied) constraints
            -> ReportErrCtxt -> [Hole] -> TcM ()
reportHoles tidy_cts ctxt
  = mapM_ $ \hole -> do { err <- mkHoleError tidy_cts ctxt hole
                        ; maybeReportHoleError ctxt hole err
                        ; maybeAddDeferredHoleBinding ctxt err hole }

mkUserTypeErrorReporter :: Reporter
mkUserTypeErrorReporter ctxt
  = mapM_ $ \ct -> do { err <- mkUserTypeError ctxt ct
                      ; maybeReportError ctxt err
                      ; addDeferredBinding ctxt err ct }

mkUserTypeError :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkUserTypeError ctxt ct = mkErrorMsgFromCt ctxt ct
                        $ important
                        $ pprUserTypeErrorTy
                        $ case getUserTypeErrorMsg ct of
                            Just msg -> msg
                            Nothing  -> pprPanic "mkUserTypeError" (ppr ct)


mkGivenErrorReporter :: Reporter
-- See Note [Given errors]
mkGivenErrorReporter ctxt cts
  = do { (ctxt, binds_msg, ct) <- relevantBindings True ctxt ct
       ; dflags <- getDynFlags
       ; let (implic:_) = cec_encl ctxt
                 -- Always non-empty when mkGivenErrorReporter is called
             ct' = setCtLoc ct (setCtLocEnv (ctLoc ct) (ic_env implic))
                   -- For given constraints we overwrite the env (and hence src-loc)
                   -- with one from the immediately-enclosing implication.
                   -- See Note [Inaccessible code]

             inaccessible_msg = hang (text "Inaccessible code in")
                                   2 (ppr (ic_info implic))
             report = important inaccessible_msg `mappend`
                      mk_relevant_bindings binds_msg

       ; err <- mkEqErr_help dflags ctxt report ct'
                             Nothing ty1 ty2

       ; traceTc "mkGivenErrorReporter" (ppr ct)
       ; reportWarning (Reason Opt_WarnInaccessibleCode) err }
  where
    (ct : _ )  = cts    -- Never empty
    (ty1, ty2) = getEqPredTys (ctPred ct)

ignoreErrorReporter :: Reporter
-- Discard Given errors that don't come from
-- a pattern match; maybe we should warn instead?
ignoreErrorReporter ctxt cts
  = do { traceTc "mkGivenErrorReporter no" (ppr cts $$ ppr (cec_encl ctxt))
       ; return () }


{- Note [Given errors]
~~~~~~~~~~~~~~~~~~~~~~
Given constraints represent things for which we have (or will have)
evidence, so they aren't errors.  But if a Given constraint is
insoluble, this code is inaccessible, and we might want to at least
warn about that.  A classic case is

   data T a where
     T1 :: T Int
     T2 :: T a
     T3 :: T Bool

   f :: T Int -> Bool
   f T1 = ...
   f T2 = ...
   f T3 = ...  -- We want to report this case as inaccessible

We'd like to point out that the T3 match is inaccessible. It
will have a Given constraint [G] Int ~ Bool.

But we don't want to report ALL insoluble Given constraints.  See Trac
#12466 for a long discussion.  For example, if we aren't careful
we'll complain about
   f :: ((Int ~ Bool) => a -> a) -> Int
which arguably is OK.  It's more debatable for
   g :: (Int ~ Bool) => Int -> Int
but it's tricky to distinguish these cases so we don't report
either.

The bottom line is this: has_gadt_match looks for an enclosing
pattern match which binds some equality constraints.  If we
find one, we report the insoluble Given.
-}

mkGroupReporter :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg)
                             -- Make error message for a group
                -> Reporter  -- Deal with lots of constraints
-- Group together errors from same location,
-- and report only the first (to avoid a cascade)
mkGroupReporter mk_err ctxt cts
  = mapM_ (reportGroup mk_err ctxt . toList) (equivClasses cmp_loc cts)

-- Like mkGroupReporter, but doesn't actually print error messages
mkSuppressReporter :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg) -> Reporter
mkSuppressReporter mk_err ctxt cts
  = mapM_ (suppressGroup mk_err ctxt . toList) (equivClasses cmp_loc cts)

eq_lhs_type :: Ct -> Ct -> Bool
eq_lhs_type ct1 ct2
  = case (classifyPredType (ctPred ct1), classifyPredType (ctPred ct2)) of
       (EqPred eq_rel1 ty1 _, EqPred eq_rel2 ty2 _) ->
         (eq_rel1 == eq_rel2) && (ty1 `eqType` ty2)
       _ -> pprPanic "mkSkolReporter" (ppr ct1 $$ ppr ct2)

cmp_loc :: Ct -> Ct -> Ordering
cmp_loc ct1 ct2 = ctLocSpan (ctLoc ct1) `compare` ctLocSpan (ctLoc ct2)

reportGroup :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg) -> Reporter
reportGroup mk_err ctxt cts =
  assert (not (null cts))
  do { err <- mk_err ctxt cts
     ; traceTc "About to maybeReportErr" $
       vcat [ text "Constraint:"             <+> ppr cts
            , text "cec_suppress ="          <+> ppr (cec_suppress ctxt)
            , text "cec_defer_type_errors =" <+> ppr (cec_defer_type_errors ctxt) ]
     ; maybeReportError ctxt err
         -- But see Note [Always warn with -fdefer-type-errors]
     ; traceTc "reportGroup" (ppr cts)
     ; mapM_ (addDeferredBinding ctxt err) cts }
         -- Add deferred bindings for all
         -- Redundant if we are going to abort compilation,
         -- but that's hard to know for sure, and if we don't
         -- abort, we need bindings for all (e.g. #12156)

-- like reportGroup, but does not actually report messages. It still adds
-- -fdefer-type-errors bindings, though.
suppressGroup :: (ReportErrCtxt -> [Ct] -> TcM ErrMsg) -> Reporter
suppressGroup mk_err ctxt cts
 = do { err <- mk_err ctxt cts
      ; traceTc "Suppressing errors for" (ppr cts)
      ; mapM_ (addDeferredBinding ctxt err) cts }

maybeReportHoleError :: ReportErrCtxt -> Hole -> ErrMsg -> TcM ()
maybeReportHoleError ctxt hole err
  | isOutOfScopeHole hole
  -- Always report an error for out-of-scope variables
  -- Unless -fdefer-out-of-scope-variables is on,
  -- in which case the messages are discarded.
  -- See #12170, #12406
  = -- If deferring, report a warning only if -Wout-of-scope-variables is on
    case cec_out_of_scope_holes ctxt of
      HoleError -> reportError err
      HoleWarn  ->
        reportWarning (Reason Opt_WarnDeferredOutOfScopeVariables) err
      HoleDefer -> return ()

-- Unlike maybeReportError, these "hole" errors are
-- /not/ suppressed by cec_suppress.  We want to see them!
maybeReportHoleError ctxt (Hole { hole_sort = TypeHole }) err
  -- When -XPartialTypeSignatures is on, warnings (instead of errors) are
  -- generated for holes in partial type signatures.
  -- Unless -fwarn-partial-type-signatures is not on,
  -- in which case the messages are discarded.
  = -- For partial type signatures, generate warnings only, and do that
    -- only if -fwarn-partial-type-signatures is on
    case cec_type_holes ctxt of
       HoleError -> reportError err
       HoleWarn  -> reportWarning (Reason Opt_WarnPartialTypeSignatures) err
       HoleDefer -> return ()

maybeReportHoleError ctxt hole@(Hole { hole_sort = ExprHole _ }) err
  -- Otherwise this is a typed hole in an expression,
  -- but not for an out-of-scope variable (because that goes through a
  -- different function)
  = -- If deferring, report a warning only if -Wtyped-holes is on
    assert (not (isOutOfScopeHole hole))
    case cec_expr_holes ctxt of
       HoleError -> reportError err
       HoleWarn  -> reportWarning (Reason Opt_WarnTypedHoles) err
       HoleDefer -> return ()

maybeReportError :: ReportErrCtxt -> ErrMsg -> TcM ()
-- Report the error and/or make a deferred binding for it
maybeReportError ctxt err
  | cec_suppress ctxt    -- Some worse error has occurred;
  = return ()            -- so suppress this error/warning

  | otherwise
  = case cec_defer_type_errors ctxt of
      TypeDefer       -> return ()
      TypeWarn reason -> reportWarning reason err
      TypeError       -> reportError err

addDeferredBinding :: ReportErrCtxt -> ErrMsg -> Ct -> TcM ()
-- See Note [Deferring coercion errors to runtime]
addDeferredBinding ctxt err ct
  | deferringAnyBindings ctxt
  , CtWanted { ctev_pred = pred, ctev_dest = dest } <- ctEvidence ct
    -- Only add deferred bindings for Wanted constraints
  = do { dflags <- getDynFlags
       ; let err_tm       = mkErrorTerm dflags pred err
             ev_binds_var = cec_binds ctxt

       ; case dest of
           EvVarDest evar
             -> addTcEvBind ev_binds_var $ mkWantedEvBind evar err_tm
           HoleDest hole
             -> do { -- See Note [Deferred errors for coercion holes]
                     let co_var = coHoleCoVar hole
                   ; addTcEvBind ev_binds_var $ mkWantedEvBind co_var err_tm
                   ; fillCoercionHole hole (mkTcCoVarCo co_var) }}

  | otherwise   -- Do not set any evidence for Given/Derived
  = return ()

mkErrorTerm :: DynFlags -> Type  -- of the error term
            -> ErrMsg -> EvTerm
mkErrorTerm dflags ty err = evDelayedError ty err_fs
  where
    err_msg = pprLocErrMsg err
    err_fs  = mkFastString $ showSDoc dflags $
              err_msg $$ text "(deferred type error)"
maybeAddDeferredHoleBinding :: ReportErrCtxt -> ErrMsg -> Hole -> TcM ()
maybeAddDeferredHoleBinding ctxt err (Hole { hole_sort = ExprHole ev_id })
-- Only add bindings for holes in expressions
-- not for holes in partial type signatures
-- cf. addDeferredBinding
  | deferringAnyBindings ctxt
  = do { dflags <- getDynFlags
       ; let err_tm = mkErrorTerm dflags (idType ev_id) err
           -- NB: idType ev_id, not hole_ty. hole_ty might be rewritten.
           -- See Note [Holes] in GHC.Tc.Types.Constraint
       ; addTcEvBind (cec_binds ctxt) $ mkWantedEvBind ev_id err_tm }
  | otherwise
  = return ()
maybeAddDeferredHoleBinding _ _ (Hole { hole_sort = TypeHole })
  = return ()

tryReporters :: ReportErrCtxt -> [ReporterSpec] -> [Ct] -> TcM (ReportErrCtxt, [Ct])
-- Use the first reporter in the list whose predicate says True
tryReporters ctxt reporters cts
  = do { let (vis_cts, invis_cts) = partition (isVisibleOrigin . ctOrigin) cts
       ; traceTc "tryReporters {" (ppr vis_cts $$ ppr invis_cts)
       ; (ctxt', cts') <- go ctxt reporters vis_cts invis_cts
       ; traceTc "tryReporters }" (ppr cts')
       ; return (ctxt', cts') }
  where
    go ctxt [] vis_cts invis_cts
      = return (ctxt, vis_cts ++ invis_cts)

    go ctxt (r : rs) vis_cts invis_cts
       -- always look at *visible* Origins before invisible ones
       -- this is the whole point of isVisibleOrigin
      = do { (ctxt', vis_cts') <- tryReporter ctxt r vis_cts
           ; (ctxt'', invis_cts') <- tryReporter ctxt' r invis_cts
           ; go ctxt'' rs vis_cts' invis_cts' }
                -- Carry on with the rest, because we must make
                -- deferred bindings for them if we have -fdefer-type-errors
                -- But suppress their error messages

tryReporter :: ReportErrCtxt -> ReporterSpec -> [Ct] -> TcM (ReportErrCtxt, [Ct])
tryReporter ctxt (str, keep_me,  suppress_after, reporter) cts
  | null yeses
  = return (ctxt, cts)
  | otherwise
  = do { traceTc "tryReporter{ " (text str <+> ppr yeses)
       ; (_, no_errs) <- askNoErrs (reporter ctxt yeses)
       ; let suppress_now = not no_errs && suppress_after
                            -- See Note [Suppressing error messages]
             ctxt' = ctxt { cec_suppress = suppress_now || cec_suppress ctxt }
       ; traceTc "tryReporter end }" (text str <+> ppr (cec_suppress ctxt) <+> ppr suppress_after)
       ; return (ctxt', nos) }
  where
    (yeses, nos) = partition (\ct -> keep_me ct (classifyPredType (ctPred ct))) cts

pprArising :: CtOrigin -> SDoc
-- Used for the main, top-level error message
-- We've done special processing for TypeEq, KindEq, Given
pprArising (TypeEqOrigin {}) = empty
pprArising (KindEqOrigin {}) = empty
pprArising (GivenOrigin {})  = empty
pprArising orig              = pprCtOrigin orig

-- Add the "arising from..." part to a message about bunch of dicts
addArising :: CtOrigin -> SDoc -> SDoc
addArising orig msg = hang msg 2 (pprArising orig)

pprWithArising :: [Ct] -> (CtLoc, SDoc)
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising from a use of p at q
-- Also return a location for the error message
-- Works for Wanted/Derived only
pprWithArising []
  = panic "pprWithArising"
pprWithArising (ct:cts)
  | null cts
  = (loc, addArising (ctLocOrigin loc)
                     (pprTheta [ctPred ct]))
  | otherwise
  = (loc, vcat (map ppr_one (ct:cts)))
  where
    loc = ctLoc ct
    ppr_one ct' = hang (parens (pprType (ctPred ct')))
                     2 (pprCtLoc (ctLoc ct'))

mkErrorMsgFromCt :: ReportErrCtxt -> Ct -> Report -> TcM ErrMsg
mkErrorMsgFromCt ctxt ct report
  = mkErrorReport ctxt (ctLocEnv (ctLoc ct)) report

mkErrorReport :: ReportErrCtxt -> TcLclEnv -> Report -> TcM ErrMsg
mkErrorReport ctxt tcl_env (Report important relevant_bindings valid_subs)
  = do { context <- mkErrInfo (cec_tidy ctxt) (tcl_ctxt tcl_env)
       ; mkErrDocAt (RealSrcSpan (tcl_loc tcl_env) Nothing)
            (errDoc important [context] (relevant_bindings ++ valid_subs))
       }

type UserGiven = Implication

getUserGivens :: ReportErrCtxt -> [UserGiven]
-- One item for each enclosing implication
getUserGivens (CEC {cec_encl = implics}) = getUserGivensFromImplics implics

getUserGivensFromImplics :: [Implication] -> [UserGiven]
getUserGivensFromImplics implics
  = reverse (filterOut (null . ic_given) implics)

{- Note [Always warn with -fdefer-type-errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -fdefer-type-errors is on we warn about *all* type errors, even
if cec_suppress is on.  This can lead to a lot more warnings than you
would get errors without -fdefer-type-errors, but if we suppress any of
them you might get a runtime error that wasn't warned about at compile
time.

This is an easy design choice to change; just flip the order of the
first two equations for maybeReportError

To be consistent, we should also report multiple warnings from a single
location in mkGroupReporter, when -fdefer-type-errors is on.  But that
is perhaps a bit *over*-consistent! Again, an easy choice to change.

With #10283, you can now opt out of deferred type error warnings.

Note [Deferred errors for coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we need to defer a type error where the destination for the evidence
is a coercion hole. We can't just put the error in the hole, because we can't
make an erroneous coercion. (Remember that coercions are erased for runtime.)
Instead, we invent a new EvVar, bind it to an error and then make a coercion
from that EvVar, filling the hole with that coercion. Because coercions'
types are unlifted, the error is guaranteed to be hit before we get to the
coercion.

Note [Do not report derived but soluble errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wc_simples include Derived constraints that have not been solved,
but are not insoluble (in that case they'd be reported by 'report1').
We do not want to report these as errors:

* Superclass constraints. If we have an unsolved [W] Ord a, we'll also have
  an unsolved [D] Eq a, and we do not want to report that; it's just noise.

* Functional dependencies.  For givens, consider
      class C a b | a -> b
      data T a where
         MkT :: C a d => [d] -> T a
      f :: C a b => T a -> F Int
      f (MkT xs) = length xs
  Then we get a [D] b~d.  But there *is* a legitimate call to
  f, namely   f (MkT [True]) :: T Bool, in which b=d.  So we should
  not reject the program.

  For wanteds, something similar
      data T a where
        MkT :: C Int b => a -> b -> T a
      g :: C Int c => c -> ()
      f :: T a -> ()
      f (MkT x y) = g x
  Here we get [G] C Int b, [W] C Int a, hence [D] a~b.
  But again f (MkT True True) is a legitimate call.

(We leave the Deriveds in wc_simple until reportErrors, so that we don't lose
derived superclasses between iterations of the solver.)

For functional dependencies, here is a real example,
stripped off from libraries/utf8-string/Codec/Binary/UTF8/Generic.hs

  class C a b | a -> b
  g :: C a b => a -> b -> ()
  f :: C a b => a -> b -> ()
  f xa xb =
      let loop = g xa
      in loop xb

We will first try to infer a type for loop, and we will succeed:
    C a b' => b' -> ()
Subsequently, we will type check (loop xb) and all is good. But,
recall that we have to solve a final implication constraint:
    C a b => (C a b' => .... cts from body of loop .... ))
And now we have a problem as we will generate an equality b ~ b' and fail to
solve it.


************************************************************************
*                                                                      *
                Irreducible predicate errors
*                                                                      *
************************************************************************
-}

mkIrredErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIrredErr ctxt cts
  = do { (ctxt, binds_msg, ct1) <- relevantBindings True ctxt ct1
       ; let orig = ctOrigin ct1
             msg  = couldNotDeduce (getUserGivens ctxt) (map ctPred cts, orig)
       ; mkErrorMsgFromCt ctxt ct1 $
            important msg `mappend` mk_relevant_bindings binds_msg }
  where
    (ct1:_) = cts

----------------
mkHoleError :: [Ct] -> ReportErrCtxt -> Hole -> TcM ErrMsg
mkHoleError _tidy_simples _ctxt hole@(Hole { hole_occ = occ
                                           , hole_ty = hole_ty
                                           , hole_loc = ct_loc })
  | isOutOfScopeHole hole
  = do { dflags  <- getDynFlags
       ; rdr_env <- getGlobalRdrEnv
       ; imp_info <- getImports
       ; curr_mod <- getModule
       ; hpt <- getHpt
       ; mkErrDocAt (RealSrcSpan (tcl_loc lcl_env) Nothing) $
                    errDoc [out_of_scope_msg] []
                           [unknownNameSuggestions dflags hpt curr_mod rdr_env
                            (tcl_rdr lcl_env) imp_info (mkRdrUnqual occ)] }
  where
    herald | isDataOcc occ = text "Data constructor not in scope:"
           | otherwise     = text "Variable not in scope:"

    out_of_scope_msg -- Print v :: ty only if the type has structure
      | boring_type = hang herald 2 (ppr occ)
      | otherwise   = hang herald 2 (pp_occ_with_type occ hole_ty)

    lcl_env     = ctLocEnv ct_loc
    boring_type = isTyVarTy hole_ty

 -- general case: not an out-of-scope error
mkHoleError tidy_simples ctxt hole@(Hole { hole_occ = occ
                                         , hole_ty = hole_ty
                                         , hole_sort = sort
                                         , hole_loc = ct_loc })
  = do { (ctxt, binds_msg)
           <- relevant_bindings False ctxt lcl_env (tyCoVarsOfType hole_ty)
               -- The 'False' means "don't filter the bindings"; see Trac #8191

       ; show_hole_constraints <- goptM Opt_ShowHoleConstraints
       ; let constraints_msg
               | ExprHole _ <- sort, show_hole_constraints
               = givenConstraintsMsg ctxt
               | otherwise
               = empty

       ; show_valid_hole_fits <- goptM Opt_ShowValidHoleFits
       ; (ctxt, sub_msg) <- if show_valid_hole_fits
                            then validHoleFits ctxt tidy_simples hole
                            else return (ctxt, empty)

       ; mkErrorReport ctxt lcl_env $
            important hole_msg `mappend`
            mk_relevant_bindings (binds_msg $$ constraints_msg) `mappend`
            valid_hole_fits sub_msg }

  where
    lcl_env     = ctLocEnv ct_loc
    hole_kind   = tcTypeKind hole_ty
    tyvars      = tyCoVarsOfTypeList hole_ty

    hole_msg = case sort of
      ExprHole _ -> vcat [ hang (text "Found hole:")
                            2 (pp_occ_with_type occ hole_ty)
                         , tyvars_msg, expr_hole_hint ]
      TypeHole -> vcat [ hang (text "Found type wildcard" <+> quotes (ppr occ))
                            2 (text "standing for" <+> quotes pp_hole_type_with_kind)
                       , tyvars_msg, type_hole_hint ]

    pp_hole_type_with_kind
      | isLiftedTypeKind hole_kind
        || isCoVarType hole_ty -- Don't print the kind of unlifted
                               -- equalities (#15039)
      = pprType hole_ty
      | otherwise
      = pprType hole_ty <+> dcolon <+> pprKind hole_kind

    tyvars_msg = ppUnless (null tyvars) $
                 text "Where:" <+> (vcat (map loc_msg other_tvs)
                                    $$ pprSkols ctxt skol_tvs)
       where
         (skol_tvs, other_tvs) = partition is_skol tyvars
         is_skol tv = isTcTyVar tv && isSkolemTyVar tv
                      -- Coercion variables can be free in the
                      -- hole, via kind casts

    type_hole_hint
         | HoleError <- cec_type_holes ctxt
         = text "To use the inferred type, enable PartialTypeSignatures"
         | otherwise
         = empty

    expr_hole_hint                       -- Give hint for, say,   f x = _x
         | lengthFS (occNameFS occ) > 1  -- Don't give this hint for plain "_"
         = text "Or perhaps" <+> quotes (ppr occ)
           <+> text "is mis-spelled, or not in scope"
         | otherwise
         = empty

    loc_msg tv
       | isTyVar tv
       = case tcTyVarDetails tv of
           MetaTv {} -> quotes (ppr tv) <+> text "is an ambiguous type variable"
           _         -> empty  -- Skolems dealt with already
       | otherwise  -- A coercion variable can be free in the hole type
       = ppWhenOption sdocPrintExplicitCoercions $
           quotes (ppr tv) <+> text "is a coercion variable"

pp_occ_with_type :: OccName -> Type -> SDoc
pp_occ_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

-- We unwrap the ReportErrCtxt here, to avoid introducing a loop in module
-- imports
validHoleFits :: ReportErrCtxt -- The context we're in, i.e. the
                                        -- implications and the tidy environment
                       -> [Ct]          -- Unsolved simple constraints
                       -> Hole          -- The hole
                       -> TcM (ReportErrCtxt, SDoc) -- We return the new context
                                                    -- with a possibly updated
                                                    -- tidy environment, and
                                                    -- the message.
validHoleFits ctxt@(CEC {cec_encl = implics
                             , cec_tidy = lcl_env}) simps hole
  = do { (tidy_env, msg) <- findValidHoleFits lcl_env implics simps hole
       ; return (ctxt {cec_tidy = tidy_env}, msg) }

-- See Note [Constraints include ...]
givenConstraintsMsg :: ReportErrCtxt -> SDoc
givenConstraintsMsg ctxt =
    let constraints :: [(Type, RealSrcSpan)]
        constraints =
          do { implic@Implic{ ic_given = given } <- cec_encl ctxt
             ; constraint <- given
             ; return (varType constraint, tcl_loc (ic_env implic)) }

        pprConstraint (constraint, loc) =
          ppr constraint <+> nest 2 (parens (text "from" <+> ppr loc))

    in ppUnless (null constraints) $
         hang (text "Constraints include")
            2 (vcat $ map pprConstraint constraints)

----------------
mkIPErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkIPErr ctxt cts
  = do { (ctxt, binds_msg, ct1) <- relevantBindings True ctxt ct1
       ; let orig    = ctOrigin ct1
             preds   = map ctPred cts
             givens  = getUserGivens ctxt
             msg | null givens
                 = addArising orig $
                   sep [ text "Unbound implicit parameter" <> plural cts
                       , nest 2 (pprParendTheta preds) ]
                 | otherwise
                 = couldNotDeduce givens (preds, orig)

       ; mkErrorMsgFromCt ctxt ct1 $
            important msg `mappend` mk_relevant_bindings binds_msg }
  where
    (ct1:_) = cts

{-
Note [Constraints include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'givenConstraintsMsg' returns the "Constraints include ..." message enabled by
-fshow-hole-constraints. For example, the following hole:

    foo :: (Eq a, Show a) => a -> String
    foo x = _

would generate the message:

    Constraints include
      Eq a (from foo.hs:1:1-36)
      Show a (from foo.hs:1:1-36)

Constraints are displayed in order from innermost (closest to the hole) to
outermost. There's currently no filtering or elimination of duplicates.

************************************************************************
*                                                                      *
                Equality errors
*                                                                      *
************************************************************************

Note [Inaccessible code]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a where
     T1 :: T a
     T2 :: T Bool

   f :: (a ~ Int) => T a -> Int
   f T1 = 3
   f T2 = 4   -- Unreachable code

Here the second equation is unreachable. The original constraint
(a~Int) from the signature gets rewritten by the pattern-match to
(Bool~Int), so the danger is that we report the error as coming from
the *signature* (#7293).  So, for Given errors we replace the
env (and hence src-loc) on its CtLoc with that from the immediately
enclosing implication.

Note [Error messages for untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#9109)
  data G a where { GBool :: G Bool }
  foo x = case x of GBool -> True

Here we can't solve (t ~ Bool), where t is the untouchable result
meta-var 't', because of the (a ~ Bool) from the pattern match.
So we infer the type
   f :: forall a t. G a -> t
making the meta-var 't' into a skolem.  So when we come to report
the unsolved (t ~ Bool), t won't look like an untouchable meta-var
any more.  So we don't assert that it is.
-}

-- Don't have multiple equality errors from the same location
-- E.g.   (Int,Bool) ~ (Bool,Int)   one error will do!
mkEqErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkEqErr ctxt (ct:_) = mkEqErr1 ctxt ct
mkEqErr _ [] = panic "mkEqErr"

mkEqErr1 :: ReportErrCtxt -> Ct -> TcM ErrMsg
mkEqErr1 ctxt ct   -- Wanted or derived;
                   -- givens handled in mkGivenErrorReporter
  = do { (ctxt, binds_msg, ct) <- relevantBindings True ctxt ct
       ; rdr_env <- getGlobalRdrEnv
       ; fam_envs <- tcGetFamInstEnvs
       ; exp_syns <- goptM Opt_PrintExpandedSynonyms
       ; let (keep_going, is_oriented, wanted_msg)
                           = mk_wanted_extra (ctLoc ct) exp_syns
             coercible_msg = case ctEqRel ct of
               NomEq  -> empty
               ReprEq -> mkCoercibleExplanation rdr_env fam_envs ty1 ty2
       ; dflags <- getDynFlags
       ; traceTc "mkEqErr1" (ppr ct $$ pprCtOrigin (ctOrigin ct) $$ ppr keep_going)
       ; let report = mconcat [important wanted_msg, important coercible_msg,
                               mk_relevant_bindings binds_msg]
       ; if keep_going
         then mkEqErr_help dflags ctxt report ct is_oriented ty1 ty2
         else mkErrorMsgFromCt ctxt ct report }
  where
    (ty1, ty2) = getEqPredTys (ctPred ct)

       -- If the types in the error message are the same as the types
       -- we are unifying, don't add the extra expected/actual message
    mk_wanted_extra :: CtLoc -> Bool -> (Bool, Maybe SwapFlag, SDoc)
    mk_wanted_extra loc expandSyns
      = case ctLocOrigin loc of
          orig@TypeEqOrigin {} -> mkExpectedActualMsg ty1 ty2 orig
                                                      t_or_k expandSyns
            where
              t_or_k = ctLocTypeOrKind_maybe loc

          KindEqOrigin cty1 mb_cty2 sub_o sub_t_or_k
            -> (True, Nothing, msg1 $$ msg2)
            where
              sub_what = case sub_t_or_k of Just KindLevel -> text "kinds"
                                            _              -> text "types"
              msg1 = sdocOption sdocPrintExplicitCoercions $ \printExplicitCoercions ->
                     case mb_cty2 of
                       Just cty2
                         |  printExplicitCoercions
                         || not (cty1 `pickyEqType` cty2)
                         -> hang (text "When matching" <+> sub_what)
                               2 (vcat [ ppr cty1 <+> dcolon <+>
                                         ppr (tcTypeKind cty1)
                                       , ppr cty2 <+> dcolon <+>
                                         ppr (tcTypeKind cty2) ])
                       _ -> text "When matching the kind of" <+> quotes (ppr cty1)
              msg2 = case sub_o of
                       TypeEqOrigin {}
                         | Just cty2 <- mb_cty2 ->
                         thdOf3 (mkExpectedActualMsg cty1 cty2 sub_o sub_t_or_k
                                                     expandSyns)
                       _ -> empty
          _ -> (True, Nothing, empty)

-- | This function tries to reconstruct why a "Coercible ty1 ty2" constraint
-- is left over.
mkCoercibleExplanation :: GlobalRdrEnv -> FamInstEnvs
                       -> TcType -> TcType -> SDoc
mkCoercibleExplanation rdr_env fam_envs ty1 ty2
  | Just (tc, tys) <- tcSplitTyConApp_maybe ty1
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = msg
  | Just (tc, tys) <- splitTyConApp_maybe ty2
  , (rep_tc, _, _) <- tcLookupDataFamInst fam_envs tc tys
  , Just msg <- coercible_msg_for_tycon rep_tc
  = msg
  | Just (s1, _) <- tcSplitAppTy_maybe ty1
  , Just (s2, _) <- tcSplitAppTy_maybe ty2
  , s1 `eqType` s2
  , has_unknown_roles s1
  = hang (text "NB: We cannot know what roles the parameters to" <+>
          quotes (ppr s1) <+> text "have;")
       2 (text "we must assume that the role is nominal")
  | otherwise
  = empty
  where
    coercible_msg_for_tycon tc
        | isAbstractTyCon tc
        = Just $ hsep [ text "NB: The type constructor"
                      , quotes (pprSourceTyCon tc)
                      , text "is abstract" ]
        | isNewTyCon tc
        , [data_con] <- tyConDataCons tc
        , let dc_name = dataConName data_con
        , isNothing (lookupGRE_Name rdr_env dc_name)
        = Just $ hang (text "The data constructor" <+> quotes (ppr dc_name))
                    2 (sep [ text "of newtype" <+> quotes (pprSourceTyCon tc)
                           , text "is not in scope" ])
        | otherwise = Nothing

    has_unknown_roles ty
      | Just (tc, tys) <- tcSplitTyConApp_maybe ty
      = tys `lengthAtLeast` tyConArity tc  -- oversaturated tycon
      | Just (s, _) <- tcSplitAppTy_maybe ty
      = has_unknown_roles s
      | isTyVarTy ty
      = True
      | otherwise
      = False

{-
-- | Make a listing of role signatures for all the parameterised tycons
-- used in the provided types


-- SLPJ Jun 15: I could not convince myself that these hints were really
-- useful.  Maybe they are, but I think we need more work to make them
-- actually helpful.
mkRoleSigs :: Type -> Type -> SDoc
mkRoleSigs ty1 ty2
  = ppUnless (null role_sigs) $
    hang (text "Relevant role signatures:")
       2 (vcat role_sigs)
  where
    tcs = nameEnvElts $ tyConsOfType ty1 `plusNameEnv` tyConsOfType ty2
    role_sigs = mapMaybe ppr_role_sig tcs

    ppr_role_sig tc
      | null roles  -- if there are no parameters, don't bother printing
      = Nothing
      | isBuiltInSyntax (tyConName tc)  -- don't print roles for (->), etc.
      = Nothing
      | otherwise
      = Just $ hsep $ [text "type role", ppr tc] ++ map ppr roles
      where
        roles = tyConRoles tc
-}

mkEqErr_help :: DynFlags -> ReportErrCtxt -> Report
             -> Ct
             -> Maybe SwapFlag   -- Nothing <=> not sure
             -> TcType -> TcType -> TcM ErrMsg
mkEqErr_help dflags ctxt report ct oriented ty1 ty2
  | Just (tv1, _) <- tcGetCastedTyVar_maybe ty1
  = mkTyVarEqErr dflags ctxt report ct oriented tv1 ty2
  | Just (tv2, _) <- tcGetCastedTyVar_maybe ty2
  = mkTyVarEqErr dflags ctxt report ct swapped  tv2 ty1
  | otherwise
  = reportEqErr ctxt report ct oriented ty1 ty2
  where
    swapped = fmap flipSwap oriented

reportEqErr :: ReportErrCtxt -> Report
            -> Ct
            -> Maybe SwapFlag   -- Nothing <=> not sure
            -> TcType -> TcType -> TcM ErrMsg
reportEqErr ctxt report ct oriented ty1 ty2
  = mkErrorMsgFromCt ctxt ct (mconcat [misMatch, report, eqInfo])
  where misMatch = important $ misMatchOrCND ctxt ct oriented ty1 ty2
        eqInfo = important $ mkEqInfoMsg ct ty1 ty2

mkTyVarEqErr, mkTyVarEqErr'
  :: DynFlags -> ReportErrCtxt -> Report -> Ct
             -> Maybe SwapFlag -> TcTyVar -> TcType -> TcM ErrMsg
-- tv1 and ty2 are already tidied
mkTyVarEqErr dflags ctxt report ct oriented tv1 ty2
  = do { traceTc "mkTyVarEqErr" (ppr ct $$ ppr tv1 $$ ppr ty2)
       ; mkTyVarEqErr' dflags ctxt report ct oriented tv1 ty2 }

mkTyVarEqErr' dflags ctxt report ct oriented tv1 ty2
  | not insoluble_occurs_check   -- See Note [Occurs check wins]
  , isUserSkolem ctxt tv1   -- ty2 won't be a meta-tyvar, or else the thing would
                            -- be oriented the other way round;
                            -- see GHC.Tc.Solver.Canonical.canEqTyVarTyVar
    || isTyVarTyVar tv1 && not (isTyVarTy ty2)
    || ctEqRel ct == ReprEq
     -- the cases below don't really apply to ReprEq (except occurs check)
  = mkErrorMsgFromCt ctxt ct $ mconcat
        [ important $ misMatchOrCND ctxt ct oriented ty1 ty2
        , important $ extraTyVarEqInfo ctxt tv1 ty2
        , report
        ]

  | MTVU_Occurs <- occ_check_expand
    -- We report an "occurs check" even for  a ~ F t a, where F is a type
    -- function; it's not insoluble (because in principle F could reduce)
    -- but we have certainly been unable to solve it
    -- See Note [Occurs check error] in GHC.Tc.Solver.Canonical
  = do { let main_msg = addArising (ctOrigin ct) $
                        hang (text "Occurs check: cannot construct the infinite" <+> what <> colon)
                              2 (sep [ppr ty1, char '~', ppr ty2])

             extra2 = important $ mkEqInfoMsg ct ty1 ty2

             interesting_tyvars = filter (not . noFreeVarsOfType . tyVarKind) $
                                  filter isTyVar $
                                  fvVarList $
                                  tyCoFVsOfType ty1 `unionFV` tyCoFVsOfType ty2
             extra3 = mk_relevant_bindings $
                      ppWhen (not (null interesting_tyvars)) $
                      hang (text "Type variable kinds:") 2 $
                      vcat (map (tyvar_binding . tidyTyCoVarOcc (cec_tidy ctxt))
                                interesting_tyvars)

             tyvar_binding tv = ppr tv <+> dcolon <+> ppr (tyVarKind tv)
       ; mkErrorMsgFromCt ctxt ct $
         mconcat [important main_msg, extra2, extra3, report] }

  | MTVU_Bad <- occ_check_expand
  = do { let msg = vcat [ text "Cannot instantiate unification variable"
                          <+> quotes (ppr tv1)
                        , hang (text "with a" <+> what <+> text "involving polytypes:") 2 (ppr ty2)
                        , nest 2 (text "GHC doesn't yet support impredicative polymorphism") ]
       -- Unlike the other reports, this discards the old 'report_important'
       -- instead of augmenting it.  This is because the details are not likely
       -- to be helpful since this is just an unimplemented feature.
       ; mkErrorMsgFromCt ctxt ct $ report { report_important = [msg] } }

  -- If the immediately-enclosing implication has 'tv' a skolem, and
  -- we know by now its an InferSkol kind of skolem, then presumably
  -- it started life as a TyVarTv, else it'd have been unified, given
  -- that there's no occurs-check or forall problem
  | (implic:_) <- cec_encl ctxt
  , Implic { ic_skols = skols } <- implic
  , tv1 `elem` skols
  = mkErrorMsgFromCt ctxt ct $ mconcat
        [ important $ misMatchMsg ct oriented ty1 ty2
        , important $ extraTyVarEqInfo ctxt tv1 ty2
        , report
        ]

  -- Check for skolem escape
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_skols = skols, ic_info = skol_info } <- implic
  , let esc_skols = filter (`elemVarSet` (tyCoVarsOfType ty2)) skols
  , not (null esc_skols)
  = do { let msg = important $ misMatchMsg ct oriented ty1 ty2
             esc_doc = sep [ text "because" <+> what <+> text "variable" <> plural esc_skols
                             <+> pprQuotedList esc_skols
                           , text "would escape" <+>
                             if isSingleton esc_skols then text "its scope"
                                                      else text "their scope" ]
             tv_extra = important $
                        vcat [ nest 2 $ esc_doc
                             , sep [ (if isSingleton esc_skols
                                      then text "This (rigid, skolem)" <+>
                                           what <+> text "variable is"
                                      else text "These (rigid, skolem)" <+>
                                           what <+> text "variables are")
                               <+> text "bound by"
                             , nest 2 $ ppr skol_info
                             , nest 2 $ text "at" <+>
                               ppr (tcl_loc (ic_env implic)) ] ]
       ; mkErrorMsgFromCt ctxt ct (mconcat [msg, tv_extra, report]) }

  -- Nastiest case: attempt to unify an untouchable variable
  -- So tv is a meta tyvar (or started that way before we
  -- generalised it).  So presumably it is an *untouchable*
  -- meta tyvar or a TyVarTv, else it'd have been unified
  -- See Note [Error messages for untouchables]
  | (implic:_) <- cec_encl ctxt   -- Get the innermost context
  , Implic { ic_given = given, ic_tclvl = lvl, ic_info = skol_info } <- implic
  = assertPpr (not (isTouchableMetaTyVar lvl tv1))
              (ppr tv1 $$ ppr lvl) $  -- See Note [Error messages for untouchables]
    do { let msg = important $ misMatchMsg ct oriented ty1 ty2
             tclvl_extra = important $
                  nest 2 $
                  sep [ quotes (ppr tv1) <+> text "is untouchable"
                      , nest 2 $ text "inside the constraints:" <+> pprEvVarTheta given
                      , nest 2 $ text "bound by" <+> ppr skol_info
                      , nest 2 $ text "at" <+>
                        ppr (tcl_loc (ic_env implic)) ]
             tv_extra = important $ extraTyVarEqInfo ctxt tv1 ty2
             add_sig  = important $ suggestAddSig ctxt ty1 ty2
       ; mkErrorMsgFromCt ctxt ct $ mconcat
            [msg, tclvl_extra, tv_extra, add_sig, report] }

  | otherwise
  = reportEqErr ctxt report ct oriented (mkTyVarTy tv1) ty2
        -- This *can* happen (#6123, and test T2627b)
        -- Consider an ambiguous top-level constraint (a ~ F a)
        -- Not an occurs check, because F is a type function.
  where
    ty1 = mkTyVarTy tv1
    occ_check_expand       = occCheckForErrors dflags tv1 ty2
    insoluble_occurs_check = isInsolubleOccursCheck (ctEqRel ct) tv1 ty2

    what = case ctLocTypeOrKind_maybe (ctLoc ct) of
      Just KindLevel -> text "kind"
      _              -> text "type"

mkEqInfoMsg :: Ct -> TcType -> TcType -> SDoc
-- Report (a) ambiguity if either side is a type function application
--            e.g. F a0 ~ Int
--        (b) warning about injectivity if both sides are the same
--            type function application   F a ~ F b
--            See Note [Non-injective type functions]
mkEqInfoMsg ct ty1 ty2
  = tyfun_msg $$ ambig_msg
  where
    mb_fun1 = isTyFun_maybe ty1
    mb_fun2 = isTyFun_maybe ty2

    ambig_msg | isJust mb_fun1 || isJust mb_fun2
              = snd (mkAmbigMsg False ct)
              | otherwise = empty

    tyfun_msg | Just tc1 <- mb_fun1
              , Just tc2 <- mb_fun2
              , tc1 == tc2
              , not (isInjectiveTyCon tc1 Nominal)
              = text "NB:" <+> quotes (ppr tc1)
                <+> text "is a non-injective type family"
              | otherwise = empty

isUserSkolem :: ReportErrCtxt -> TcTyVar -> Bool
-- See Note [Reporting occurs-check errors]
isUserSkolem ctxt tv
  = isSkolemTyVar tv && any is_user_skol_tv (cec_encl ctxt)
  where
    is_user_skol_tv (Implic { ic_skols = sks, ic_info = skol_info })
      = tv `elem` sks && is_user_skol_info skol_info

    is_user_skol_info (InferSkol {}) = False
    is_user_skol_info _ = True

misMatchOrCND :: ReportErrCtxt -> Ct
              -> Maybe SwapFlag -> TcType -> TcType -> SDoc
-- If oriented then ty1 is actual, ty2 is expected
misMatchOrCND ctxt ct oriented ty1 ty2
  | null givens ||
    (isRigidTy ty1 && isRigidTy ty2) ||
    isGivenCt ct
       -- If the equality is unconditionally insoluble
       -- or there is no context, don't report the context
  = misMatchMsg ct oriented ty1 ty2
  | otherwise
  = couldNotDeduce givens ([eq_pred], orig)
  where
    ev      = ctEvidence ct
    eq_pred = ctEvPred ev
    orig    = ctEvOrigin ev
    givens  = [ given | given <- getUserGivens ctxt, not (ic_no_eqs given)]
              -- Keep only UserGivens that have some equalities.
              -- See Note [Suppress redundant givens during error reporting]

couldNotDeduce :: [UserGiven] -> (ThetaType, CtOrigin) -> SDoc
couldNotDeduce givens (wanteds, orig)
  = vcat [ addArising orig (text "Could not deduce:" <+> pprTheta wanteds)
         , vcat (pp_givens givens)]

pp_givens :: [UserGiven] -> [SDoc]
pp_givens givens
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (text "from the context:") g
                 : map (ppr_given (text "or from:")) gs
    where
       ppr_given herald implic@(Implic { ic_given = gs, ic_info = skol_info })
           = hang (herald <+> pprEvVarTheta (mkMinimalBySCs evVarPred gs))
             -- See Note [Suppress redundant givens during error reporting]
             -- for why we use mkMinimalBySCs above.
                2 (sep [ text "bound by" <+> ppr skol_info
                       , text "at" <+> ppr (tcl_loc (ic_env implic)) ])

-- These are for the "blocked" equalities, as described in TcCanonical
-- Note [Equalities with incompatible kinds], wrinkle (2). There should
-- always be another unsolved wanted around, which will ordinarily suppress
-- this message. But this can still be printed out with -fdefer-type-errors
-- (sigh), so we must produce a message.
mkBlockedEqErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkBlockedEqErr ctxt (ct:_) = mkErrorMsgFromCt ctxt ct report
  where
    report = important msg
    msg = vcat [ hang (text "Cannot use equality for substitution:")
                   2 (ppr (ctPred ct))
               , text "Doing so would be ill-kinded." ]
          -- This is a terrible message. Perhaps worse, if the user
          -- has -fprint-explicit-kinds on, they will see that the two
          -- sides have the same kind, as there is an invisible cast.
          -- I really don't know how to do better.
mkBlockedEqErr _ [] = panic "mkBlockedEqErr no constraints"

{-
Note [Suppress redundant givens during error reporting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When GHC is unable to solve a constraint and prints out an error message, it
will print out what given constraints are in scope to provide some context to
the programmer. But we shouldn't print out /every/ given, since some of them
are not terribly helpful to diagnose type errors. Consider this example:

  foo :: Int :~: Int -> a :~: b -> a :~: c
  foo Refl Refl = Refl

When reporting that GHC can't solve (a ~ c), there are two givens in scope:
(Int ~ Int) and (a ~ b). But (Int ~ Int) is trivially soluble (i.e.,
redundant), so it's not terribly useful to report it in an error message.
To accomplish this, we discard any Implications that do not bind any
equalities by filtering the `givens` selected in `misMatchOrCND` (based on
the `ic_no_eqs` field of the Implication).

But this is not enough to avoid all redundant givens! Consider this example,
from #15361:

  goo :: forall (a :: Type) (b :: Type) (c :: Type).
         a :~~: b -> a :~~: c
  goo HRefl = HRefl

Matching on HRefl brings the /single/ given (* ~ *, a ~ b) into scope.
The (* ~ *) part arises due the kinds of (:~~:) being unified. More
importantly, (* ~ *) is redundant, so we'd like not to report it. However,
the Implication (* ~ *, a ~ b) /does/ bind an equality (as reported by its
ic_no_eqs field), so the test above will keep it wholesale.

To refine this given, we apply mkMinimalBySCs on it to extract just the (a ~ b)
part. This works because mkMinimalBySCs eliminates reflexive equalities in
addition to superclasses (see Note [Remove redundant provided dicts]
in GHC.Tc.TyCl.PatSyn).
-}

extraTyVarEqInfo :: ReportErrCtxt -> TcTyVar -> TcType -> SDoc
-- Add on extra info about skolem constants
-- NB: The types themselves are already tidied
extraTyVarEqInfo ctxt tv1 ty2
  = extraTyVarInfo ctxt tv1 $$ ty_extra ty2
  where
    ty_extra ty = case tcGetCastedTyVar_maybe ty of
                    Just (tv, _) -> extraTyVarInfo ctxt tv
                    Nothing      -> empty

extraTyVarInfo :: ReportErrCtxt -> TcTyVar -> SDoc
extraTyVarInfo ctxt tv
  = assertPpr (isTyVar tv) (ppr tv) $
    case tcTyVarDetails tv of
          SkolemTv {}   -> pprSkols ctxt [tv]
          RuntimeUnk {} -> quotes (ppr tv) <+> text "is an interactive-debugger skolem"
          MetaTv {}     -> empty

suggestAddSig :: ReportErrCtxt -> TcType -> TcType -> SDoc
-- See Note [Suggest adding a type signature]
suggestAddSig ctxt ty1 ty2
  | null inferred_bndrs
  = empty
  | [bndr] <- inferred_bndrs
  = text "Possible fix: add a type signature for" <+> quotes (ppr bndr)
  | otherwise
  = text "Possible fix: add type signatures for some or all of" <+> (ppr inferred_bndrs)
  where
    inferred_bndrs = nub (get_inf ty1 ++ get_inf ty2)
    get_inf ty | Just tv <- tcGetTyVar_maybe ty
               , isSkolemTyVar tv
               , ((InferSkol prs, _) : _) <- getSkolemInfo (cec_encl ctxt) [tv]
               = map fst prs
               | otherwise
               = []

--------------------
misMatchMsg :: Ct -> Maybe SwapFlag -> TcType -> TcType -> SDoc
-- Types are already tidy
-- If oriented then ty1 is actual, ty2 is expected
misMatchMsg ct oriented ty1 ty2
  | Just NotSwapped <- oriented
  = misMatchMsg ct (Just IsSwapped) ty2 ty1

  -- These next two cases are when we're about to report, e.g., that
  -- 'LiftedRep doesn't match 'VoidRep. Much better just to say
  -- lifted vs. unlifted
  | isLiftedRuntimeRep ty1
  = lifted_vs_unlifted

  | isLiftedRuntimeRep ty2
  = lifted_vs_unlifted

  | otherwise  -- So now we have Nothing or (Just IsSwapped)
               -- For some reason we treat Nothing like IsSwapped
  = addArising orig $
    pprWithExplicitKindsWhenMismatch ty1 ty2 (ctOrigin ct) $
    sep [ text herald1 <+> quotes (ppr ty1)
        , nest padding $
          text herald2 <+> quotes (ppr ty2)
        , sameOccExtra ty2 ty1 ]
  where
    herald1 = conc [ "Couldn't match"
                   , if is_repr     then "representation of" else ""
                   , if is_oriented then "expected"          else ""
                   , what ]
    herald2 = conc [ "with"
                   , if is_repr     then "that of"           else ""
                   , if is_oriented then ("actual " ++ what) else "" ]
    padding = length herald1 - length herald2

    is_repr = case ctEqRel ct of { ReprEq -> True; NomEq -> False }
    is_oriented = isJust oriented

    orig = ctOrigin ct
    what = case ctLocTypeOrKind_maybe (ctLoc ct) of
      Just KindLevel -> "kind"
      _              -> "type"

    conc :: [String] -> String
    conc = foldr1 add_space

    add_space :: String -> String -> String
    add_space s1 s2 | null s1   = s2
                    | null s2   = s1
                    | otherwise = s1 ++ (' ' : s2)

    lifted_vs_unlifted
      = addArising orig $
        text "Couldn't match a lifted type with an unlifted type"

-- | Prints explicit kinds (with @-fprint-explicit-kinds@) in an 'SDoc' when a
-- type mismatch occurs to due invisible kind arguments.
--
-- This function first checks to see if the 'CtOrigin' argument is a
-- 'TypeEqOrigin', and if so, uses the expected/actual types from that to
-- check for a kind mismatch (as these types typically have more surrounding
-- types and are likelier to be able to glean information about whether a
-- mismatch occurred in an invisible argument position or not). If the
-- 'CtOrigin' is not a 'TypeEqOrigin', fall back on the actual mismatched types
-- themselves.
pprWithExplicitKindsWhenMismatch :: Type -> Type -> CtOrigin
                                 -> SDoc -> SDoc
pprWithExplicitKindsWhenMismatch ty1 ty2 ct
  = pprWithExplicitKindsWhen show_kinds
  where
    (act_ty, exp_ty) = case ct of
      TypeEqOrigin { uo_actual = act
                   , uo_expected = exp } -> (act, exp)
      _                                  -> (ty1, ty2)
    show_kinds = tcEqTypeVis act_ty exp_ty
                 -- True when the visible bit of the types look the same,
                 -- so we want to show the kinds in the displayed type

mkExpectedActualMsg :: Type -> Type -> CtOrigin -> Maybe TypeOrKind -> Bool
                    -> (Bool, Maybe SwapFlag, SDoc)
-- NotSwapped means (actual, expected), IsSwapped is the reverse
-- First return val is whether or not to print a herald above this msg
mkExpectedActualMsg ty1 ty2 ct@(TypeEqOrigin { uo_actual = act
                                             , uo_expected = exp
                                             , uo_thing = maybe_thing })
                    m_level printExpanded
  | KindLevel <- level, occurs_check_error       = (True, Nothing, empty)
  | isUnliftedTypeKind act, isLiftedTypeKind exp = (False, Nothing, msg2)
  | isLiftedTypeKind act, isUnliftedTypeKind exp = (False, Nothing, msg3)
  | tcIsLiftedTypeKind exp                       = (False, Nothing, msg4)
  | Just msg <- num_args_msg                     = (False, Nothing, msg $$ msg1)
  | KindLevel <- level, Just th <- maybe_thing   = (False, Nothing, msg5 th)
  | act `pickyEqType` ty1, exp `pickyEqType` ty2 = (True, Just NotSwapped, empty)
  | exp `pickyEqType` ty1, act `pickyEqType` ty2 = (True, Just IsSwapped, empty)
  | otherwise                                    = (True, Nothing, msg1)
  where
    level = m_level `orElse` TypeLevel

    occurs_check_error
      | Just tv <- tcGetTyVar_maybe ty1
      , tv `elemVarSet` tyCoVarsOfType ty2
      = True
      | Just tv <- tcGetTyVar_maybe ty2
      , tv `elemVarSet` tyCoVarsOfType ty1
      = True
      | otherwise
      = False

    sort = case level of
      TypeLevel -> text "type"
      KindLevel -> text "kind"

    msg1 = case level of
      KindLevel
        | Just th <- maybe_thing
        -> msg5 th

      _ | not (act `pickyEqType` exp)
        -> pprWithExplicitKindsWhenMismatch ty1 ty2 ct $
           vcat [ text "Expected" <+> sort <> colon <+> ppr exp
                , text "  Actual" <+> sort <> colon <+> ppr act
                , if printExpanded then expandedTys else empty ]

        | otherwise
        -> empty

    thing_msg = case maybe_thing of
                  Just thing -> \_ levity ->
                    quotes thing <+> text "is" <+> levity
                  Nothing    -> \vowel levity ->
                    text "got a" <>
                    (if vowel then char 'n' else empty) <+>
                    levity <+>
                    text "type"
    msg2 = sep [ text "Expecting a lifted type, but"
               , thing_msg True (text "unlifted") ]
    msg3 = sep [ text "Expecting an unlifted type, but"
               , thing_msg False (text "lifted") ]
    msg4 = maybe_num_args_msg $$
           sep [ text "Expected a type, but"
               , maybe (text "found something with kind")
                       (\thing -> quotes thing <+> text "has kind")
                       maybe_thing
               , quotes (pprWithTYPE act) ]

    msg5 th = pprWithExplicitKindsWhenMismatch ty1 ty2 ct $
              hang (text "Expected" <+> kind_desc <> comma)
                 2 (text "but" <+> quotes th <+> text "has kind" <+>
                    quotes (ppr act))
      where
        kind_desc | tcIsConstraintKind exp = text "a constraint"

                    -- TYPE t0
                  | Just arg <- kindRep_maybe exp
                  , tcIsTyVarTy arg = sdocOption sdocPrintExplicitRuntimeReps $ \case
                                       True  -> text "kind" <+> quotes (ppr exp)
                                       False -> text "a type"

                  | otherwise       = text "kind" <+> quotes (ppr exp)

    num_args_msg = case level of
      KindLevel
        | not (isMetaTyVarTy exp) && not (isMetaTyVarTy act)
           -- if one is a meta-tyvar, then it's possible that the user
           -- has asked for something impredicative, and we couldn't unify.
           -- Don't bother with counting arguments.
        -> let n_act = count_args act
               n_exp = count_args exp in
           case n_act - n_exp of
             n | n > 0   -- we don't know how many args there are, so don't
                         -- recommend removing args that aren't
               , Just thing <- maybe_thing
               -> Just $ text "Expecting" <+> speakN (abs n) <+>
                         more <+> quotes thing
               where
                 more
                  | n == 1    = text "more argument to"
                  | otherwise = text "more arguments to"  -- n > 1
             _ -> Nothing

      _ -> Nothing

    maybe_num_args_msg = case num_args_msg of
      Nothing -> empty
      Just m  -> m

    count_args ty = count isVisibleBinder $ fst $ splitPiTys ty

    expandedTys =
      ppUnless (expTy1 `pickyEqType` exp && expTy2 `pickyEqType` act) $ vcat
        [ text "Type synonyms expanded:"
        , text "Expected type:" <+> ppr expTy1
        , text "  Actual type:" <+> ppr expTy2
        ]

    (expTy1, expTy2) = expandSynonymsToMatch exp act

mkExpectedActualMsg _ _ _ _ _ = panic "mkExpectedAcutalMsg"

{- Note [Insoluble occurs check wins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] a ~ [a],  [W] a ~ [a] (#13674).  The Given is insoluble
so we don't use it for rewriting.  The Wanted is also insoluble, and
we don't solve it from the Given.  It's very confusing to say
    Cannot solve a ~ [a] from given constraints a ~ [a]

And indeed even thinking about the Givens is silly; [W] a ~ [a] is
just as insoluble as Int ~ Bool.

Conclusion: if there's an insoluble occurs check (isInsolubleOccursCheck)
then report it first.

(NB: there are potentially-soluble ones, like (a ~ F a b), and we don't
want to be as draconian with them.)

Note [Expanding type synonyms to make types similar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In type error messages, if -fprint-expanded-types is used, we want to expand
type synonyms to make expected and found types as similar as possible, but we
shouldn't expand types too much to make type messages even more verbose and
harder to understand. The whole point here is to make the difference in expected
and found types clearer.

`expandSynonymsToMatch` does this, it takes two types, and expands type synonyms
only as much as necessary. Given two types t1 and t2:

  * If they're already same, it just returns the types.

  * If they're in form `C1 t1_1 .. t1_n` and `C2 t2_1 .. t2_m` (C1 and C2 are
    type constructors), it expands C1 and C2 if they're different type synonyms.
    Then it recursively does the same thing on expanded types. If C1 and C2 are
    same, then it applies the same procedure to arguments of C1 and arguments of
    C2 to make them as similar as possible.

    Most important thing here is to keep number of synonym expansions at
    minimum. For example, if t1 is `T (T3, T5, Int)` and t2 is `T (T5, T3,
    Bool)` where T5 = T4, T4 = T3, ..., T1 = X, it returns `T (T3, T3, Int)` and
    `T (T3, T3, Bool)`.

  * Otherwise types don't have same shapes and so the difference is clearly
    visible. It doesn't do any expansions and show these types.

Note that we only expand top-layer type synonyms. Only when top-layer
constructors are the same we start expanding inner type synonyms.

Suppose top-layer type synonyms of t1 and t2 can expand N and M times,
respectively. If their type-synonym-expanded forms will meet at some point (i.e.
will have same shapes according to `sameShapes` function), it's possible to find
where they meet in O(N+M) top-layer type synonym expansions and O(min(N,M))
comparisons. We first collect all the top-layer expansions of t1 and t2 in two
lists, then drop the prefix of the longer list so that they have same lengths.
Then we search through both lists in parallel, and return the first pair of
types that have same shapes. Inner types of these two types with same shapes
are then expanded using the same algorithm.

In case they don't meet, we return the last pair of types in the lists, which
has top-layer type synonyms completely expanded. (in this case the inner types
are not expanded at all, as the current form already shows the type error)
-}

-- | Expand type synonyms in given types only enough to make them as similar as
-- possible. Returned types are the same in terms of used type synonyms.
--
-- To expand all synonyms, see 'Type.expandTypeSynonyms'.
--
-- See `ExpandSynsFail` tests in tests testsuite/tests/typecheck/should_fail for
-- some examples of how this should work.
expandSynonymsToMatch :: Type -> Type -> (Type, Type)
expandSynonymsToMatch ty1 ty2 = (ty1_ret, ty2_ret)
  where
    (ty1_ret, ty2_ret) = go ty1 ty2

    -- | Returns (type synonym expanded version of first type,
    --            type synonym expanded version of second type)
    go :: Type -> Type -> (Type, Type)
    go t1 t2
      | t1 `pickyEqType` t2 =
        -- Types are same, nothing to do
        (t1, t2)

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , tys1 `equalLength` tys2 =
        -- Type constructors are same. They may be synonyms, but we don't
        -- expand further. The lengths of tys1 and tys2 must be equal;
        -- for example, with type S a = a, we don't want
        -- to zip (S Monad Int) and (S Bool).
        let (tys1', tys2') =
              unzip (zipWithEqual "expandSynonymsToMatch" go tys1 tys2)
         in (TyConApp tc1 tys1', TyConApp tc2 tys2')

    go (AppTy t1_1 t1_2) (AppTy t2_1 t2_2) =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in (mkAppTy t1_1' t1_2', mkAppTy t2_1' t2_2')

    go ty1@(FunTy _ t1_1 t1_2) ty2@(FunTy _ t2_1 t2_2) =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in ( ty1 { ft_arg = t1_1', ft_res = t1_2' }
          , ty2 { ft_arg = t2_1', ft_res = t2_2' })

    go (ForAllTy b1 t1) (ForAllTy b2 t2) =
      -- NOTE: We may have a bug here, but we just can't reproduce it easily.
      -- See D1016 comments for details and our attempts at producing a test
      -- case. Short version: We probably need RnEnv2 to really get this right.
      let (t1', t2') = go t1 t2
       in (ForAllTy b1 t1', ForAllTy b2 t2')

    go (CastTy ty1 _) ty2 = go ty1 ty2
    go ty1 (CastTy ty2 _) = go ty1 ty2

    go t1 t2 =
      -- See Note [Expanding type synonyms to make types similar] for how this
      -- works
      let
        t1_exp_tys = t1 : tyExpansions t1
        t2_exp_tys = t2 : tyExpansions t2
        t1_exps    = length t1_exp_tys
        t2_exps    = length t2_exp_tys
        dif        = abs (t1_exps - t2_exps)
      in
        followExpansions $
          zipEqual "expandSynonymsToMatch.go"
            (if t1_exps > t2_exps then drop dif t1_exp_tys else t1_exp_tys)
            (if t2_exps > t1_exps then drop dif t2_exp_tys else t2_exp_tys)

    -- | Expand the top layer type synonyms repeatedly, collect expansions in a
    -- list. The list does not include the original type.
    --
    -- Example, if you have:
    --
    --   type T10 = T9
    --   type T9  = T8
    --   ...
    --   type T0  = Int
    --
    -- `tyExpansions T10` returns [T9, T8, T7, ... Int]
    --
    -- This only expands the top layer, so if you have:
    --
    --   type M a = Maybe a
    --
    -- `tyExpansions (M T10)` returns [Maybe T10] (T10 is not expanded)
    tyExpansions :: Type -> [Type]
    tyExpansions = unfoldr (\t -> (\x -> (x, x)) `fmap` tcView t)

    -- | Drop the type pairs until types in a pair look alike (i.e. the outer
    -- constructors are the same).
    followExpansions :: [(Type, Type)] -> (Type, Type)
    followExpansions [] = pprPanic "followExpansions" empty
    followExpansions [(t1, t2)]
      | sameShapes t1 t2 = go t1 t2 -- expand subtrees
      | otherwise        = (t1, t2) -- the difference is already visible
    followExpansions ((t1, t2) : tss)
      -- Traverse subtrees when the outer shapes are the same
      | sameShapes t1 t2 = go t1 t2
      -- Otherwise follow the expansions until they look alike
      | otherwise = followExpansions tss

    sameShapes :: Type -> Type -> Bool
    sameShapes AppTy{}          AppTy{}          = True
    sameShapes (TyConApp tc1 _) (TyConApp tc2 _) = tc1 == tc2
    sameShapes (FunTy {})       (FunTy {})       = True
    sameShapes (ForAllTy {})    (ForAllTy {})    = True
    sameShapes (CastTy ty1 _)   ty2              = sameShapes ty1 ty2
    sameShapes ty1              (CastTy ty2 _)   = sameShapes ty1 ty2
    sameShapes _                _                = False

sameOccExtra :: TcType -> TcType -> SDoc
-- See Note [Disambiguating (X ~ X) errors]
sameOccExtra ty1 ty2
  | Just (tc1, _) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, _) <- tcSplitTyConApp_maybe ty2
  , let n1 = tyConName tc1
        n2 = tyConName tc2
        same_occ = nameOccName n1                   == nameOccName n2
        same_pkg = moduleUnit (nameModule n1) == moduleUnit (nameModule n2)
  , n1 /= n2   -- Different Names
  , same_occ   -- but same OccName
  = text "NB:" <+> (ppr_from same_pkg n1 $$ ppr_from same_pkg n2)
  | otherwise
  = empty
  where
    ppr_from same_pkg nm
      | isGoodSrcSpan loc
      = hang (quotes (ppr nm) <+> text "is defined at")
           2 (ppr loc)
      | otherwise  -- Imported things have an UnhelpfulSrcSpan
      = hang (quotes (ppr nm))
           2 (sep [ text "is defined in" <+> quotes (ppr (moduleName mod))
                  , ppUnless (same_pkg || pkg == mainUnitId) $
                    nest 4 $ text "in package" <+> quotes (ppr pkg) ])
       where
         pkg = moduleUnit mod
         mod = nameModule nm
         loc = nameSrcSpan nm

{-
Note [Suggest adding a type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The OutsideIn algorithm rejects GADT programs that don't have a principal
type, and indeed some that do.  Example:
   data T a where
     MkT :: Int -> T Int

   f (MkT n) = n

Does this have type f :: T a -> a, or f :: T a -> Int?
The error that shows up tends to be an attempt to unify an
untouchable type variable.  So suggestAddSig sees if the offending
type variable is bound by an *inferred* signature, and suggests
adding a declared signature instead.

This initially came up in #8968, concerning pattern synonyms.

Note [Disambiguating (X ~ X) errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #8278

Note [Reporting occurs-check errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (a ~ [a]), if 'a' is a rigid type variable bound by a user-supplied
type signature, then the best thing is to report that we can't unify
a with [a], because a is a skolem variable.  That avoids the confusing
"occur-check" error message.

But nowadays when inferring the type of a function with no type signature,
even if there are errors inside, we still generalise its signature and
carry on. For example
   f x = x:x
Here we will infer something like
   f :: forall a. a -> [a]
with a deferred error of (a ~ [a]).  So in the deferred unsolved constraint
'a' is now a skolem, but not one bound by the programmer in the context!
Here we really should report an occurs check.

So isUserSkolem distinguishes the two.

Note [Non-injective type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very confusing to get a message like
     Couldn't match expected type `Depend s'
            against inferred type `Depend s1'
so mkTyFunInfoMsg adds:
       NB: `Depend' is type function, and hence may not be injective

Warn of loopy local equalities that were dropped.


************************************************************************
*                                                                      *
                 Type-class errors
*                                                                      *
************************************************************************
-}

mkDictErr :: ReportErrCtxt -> [Ct] -> TcM ErrMsg
mkDictErr ctxt cts
  = assert (not (null cts)) $
    do { inst_envs <- tcGetInstEnvs
       ; let (ct1:_) = cts  -- ct1 just for its location
             min_cts = elim_superclasses cts
             lookups = map (lookup_cls_inst inst_envs) min_cts
             (no_inst_cts, overlap_cts) = partition is_no_inst lookups

       -- Report definite no-instance errors,
       -- or (iff there are none) overlap errors
       -- But we report only one of them (hence 'head') because they all
       -- have the same source-location origin, to try avoid a cascade
       -- of error from one location
       ; (ctxt, err) <- mk_dict_err ctxt (head (no_inst_cts ++ overlap_cts))
       ; mkErrorMsgFromCt ctxt ct1 (important err) }
  where
    no_givens = null (getUserGivens ctxt)

    is_no_inst (ct, (matches, unifiers, _))
      =  no_givens
      && null matches
      && (null unifiers || all (not . isAmbiguousTyVar) (tyCoVarsOfCtList ct))

    lookup_cls_inst inst_envs ct
                -- Note [Flattening in error message generation]
      = (ct, lookupInstEnv True inst_envs clas (flattenTys emptyInScopeSet tys))
      where
        (clas, tys) = getClassPredTys (ctPred ct)


    -- When simplifying [W] Ord (Set a), we need
    --    [W] Eq a, [W] Ord a
    -- but we really only want to report the latter
    elim_superclasses cts = mkMinimalBySCs ctPred cts

mk_dict_err :: ReportErrCtxt -> (Ct, ClsInstLookupResult)
            -> TcM (ReportErrCtxt, SDoc)
-- Report an overlap error if this class constraint results
-- from an overlap (returning Left clas), otherwise return (Right pred)
mk_dict_err ctxt@(CEC {cec_encl = implics}) (ct, (matches, unifiers, unsafe_overlapped))
  | null matches  -- No matches but perhaps several unifiers
  = do { (ctxt, binds_msg, ct) <- relevantBindings True ctxt ct
       ; candidate_insts <- get_candidate_instances
       ; return (ctxt, cannot_resolve_msg ct candidate_insts binds_msg) }

  | null unsafe_overlapped   -- Some matches => overlap errors
  = return (ctxt, overlap_msg)

  | otherwise
  = return (ctxt, safe_haskell_msg)
  where
    orig          = ctOrigin ct
    pred          = ctPred ct
    (clas, tys)   = getClassPredTys pred
    ispecs        = [ispec | (ispec, _) <- matches]
    unsafe_ispecs = [ispec | (ispec, _) <- unsafe_overlapped]
    useful_givens = discardProvCtxtGivens orig (getUserGivensFromImplics implics)
         -- useful_givens are the enclosing implications with non-empty givens,
         -- modulo the horrid discardProvCtxtGivens

    get_candidate_instances :: TcM [ClsInst]
    -- See Note [Report candidate instances]
    get_candidate_instances
      | [ty] <- tys   -- Only try for single-parameter classes
      = do { instEnvs <- tcGetInstEnvs
           ; return (filter (is_candidate_inst ty)
                            (classInstances instEnvs clas)) }
      | otherwise = return []

    is_candidate_inst ty inst -- See Note [Report candidate instances]
      | [other_ty] <- is_tys inst
      , Just (tc1, _) <- tcSplitTyConApp_maybe ty
      , Just (tc2, _) <- tcSplitTyConApp_maybe other_ty
      = let n1 = tyConName tc1
            n2 = tyConName tc2
            different_names = n1 /= n2
            same_occ_names = nameOccName n1 == nameOccName n2
        in different_names && same_occ_names
      | otherwise = False

    cannot_resolve_msg :: Ct -> [ClsInst] -> SDoc -> SDoc
    cannot_resolve_msg ct candidate_insts binds_msg
      = vcat [ no_inst_msg
             , nest 2 extra_note
             , vcat (pp_givens useful_givens)
             , mb_patsyn_prov `orElse` empty
             , ppWhen (has_ambig_tvs && not (null unifiers && null useful_givens))
               (vcat [ ppUnless lead_with_ambig ambig_msg, binds_msg, potential_msg ])

             , ppWhen (isNothing mb_patsyn_prov) $
                   -- Don't suggest fixes for the provided context of a pattern
                   -- synonym; the right fix is to bind more in the pattern
               show_fixes (ctxtFixes has_ambig_tvs pred implics
                           ++ drv_fixes)
             , ppWhen (not (null candidate_insts))
               (hang (text "There are instances for similar types:")
                   2 (vcat (map ppr candidate_insts))) ]
                   -- See Note [Report candidate instances]
      where
        orig = ctOrigin ct
        -- See Note [Highlighting ambiguous type variables]
        lead_with_ambig = has_ambig_tvs && not (any isRuntimeUnkSkol ambig_tvs)
                        && not (null unifiers) && null useful_givens

        (has_ambig_tvs, ambig_msg) = mkAmbigMsg lead_with_ambig ct
        ambig_tvs = uncurry (++) (getAmbigTkvs ct)

        no_inst_msg
          | lead_with_ambig
          = ambig_msg <+> pprArising orig
              $$ text "prevents the constraint" <+>  quotes (pprParendType pred)
              <+> text "from being solved."

          | null useful_givens
          = addArising orig $ text "No instance for"
            <+> pprParendType pred

          | otherwise
          = addArising orig $ text "Could not deduce"
            <+> pprParendType pred

        potential_msg
          = ppWhen (not (null unifiers) && want_potential orig) $
            sdocOption sdocPrintPotentialInstances $ \print_insts ->
            getPprStyle $ \sty ->
            pprPotentials (PrintPotentialInstances print_insts) sty potential_hdr unifiers

        potential_hdr
          = vcat [ ppWhen lead_with_ambig $
                     text "Probable fix: use a type annotation to specify what"
                     <+> pprQuotedList ambig_tvs <+> text "should be."
                 , text "These potential instance" <> plural unifiers
                   <+> text "exist:"]

        mb_patsyn_prov :: Maybe SDoc
        mb_patsyn_prov
          | not lead_with_ambig
          , ProvCtxtOrigin PSB{ psb_def = L _ pat } <- orig
          = Just (vcat [ text "In other words, a successful match on the pattern"
                       , nest 2 $ ppr pat
                       , text "does not provide the constraint" <+> pprParendType pred ])
          | otherwise = Nothing

    -- Report "potential instances" only when the constraint arises
    -- directly from the user's use of an overloaded function
    want_potential (TypeEqOrigin {}) = False
    want_potential _                 = True

    extra_note | any isFunTy (filterOutInvisibleTypes (classTyCon clas) tys)
               = text "(maybe you haven't applied a function to enough arguments?)"
               | className clas == typeableClassName  -- Avoid mysterious "No instance for (Typeable T)
               , [_,ty] <- tys                        -- Look for (Typeable (k->*) (T k))
               , Just (tc,_) <- tcSplitTyConApp_maybe ty
               , not (isTypeFamilyTyCon tc)
               = hang (text "GHC can't yet do polykinded")
                    2 (text "Typeable" <+>
                       parens (ppr ty <+> dcolon <+> ppr (tcTypeKind ty)))
               | otherwise
               = empty

    drv_fixes = case orig of
                   DerivClauseOrigin                  -> [drv_fix False]
                   StandAloneDerivOrigin              -> [drv_fix True]
                   DerivOriginDC _ _       standalone -> [drv_fix standalone]
                   DerivOriginCoerce _ _ _ standalone -> [drv_fix standalone]
                   _                -> []

    drv_fix standalone_wildcard
      | standalone_wildcard
      = text "fill in the wildcard constraint yourself"
      | otherwise
      = hang (text "use a standalone 'deriving instance' declaration,")
           2 (text "so you can specify the instance context yourself")

    -- Normal overlap error
    overlap_msg
      = assert (not (null matches)) $
        vcat [  addArising orig (text "Overlapping instances for"
                                <+> pprType (mkClassPred clas tys))

             ,  ppUnless (null matching_givens) $
                  sep [text "Matching givens (or their superclasses):"
                      , nest 2 (vcat matching_givens)]

             ,  sdocOption sdocPrintPotentialInstances $ \print_insts ->
                getPprStyle $ \sty ->
                pprPotentials (PrintPotentialInstances print_insts) sty (text "Matching instances:") $
                ispecs ++ unifiers

             ,  ppWhen (null matching_givens && isSingleton matches && null unifiers) $
                -- Intuitively, some given matched the wanted in their
                -- flattened or rewritten (from given equalities) form
                -- but the matcher can't figure that out because the
                -- constraints are non-flat and non-rewritten so we
                -- simply report back the whole given
                -- context. Accelerate Smart.hs showed this problem.
                  sep [ text "There exists a (perhaps superclass) match:"
                      , nest 2 (vcat (pp_givens useful_givens))]

             ,  ppWhen (isSingleton matches) $
                parens (vcat [ text "The choice depends on the instantiation of" <+>
                                  quotes (pprWithCommas ppr (tyCoVarsOfTypesList tys))
                             , ppWhen (null (matching_givens)) $
                               vcat [ text "To pick the first instance above, use IncoherentInstances"
                                    , text "when compiling the other instance declarations"]
                        ])]

    matching_givens = mapMaybe matchable useful_givens

    matchable implic@(Implic { ic_given = evvars, ic_info = skol_info })
      = case ev_vars_matching of
             [] -> Nothing
             _  -> Just $ hang (pprTheta ev_vars_matching)
                            2 (sep [ text "bound by" <+> ppr skol_info
                                   , text "at" <+>
                                     ppr (tcl_loc (ic_env implic)) ])
        where ev_vars_matching = [ pred
                                 | ev_var <- evvars
                                 , let pred = evVarPred ev_var
                                 , any can_match (pred : transSuperClasses pred) ]
              can_match pred
                 = case getClassPredTys_maybe pred of
                     Just (clas', tys') -> clas' == clas
                                          && isJust (tcMatchTys tys tys')
                     Nothing -> False

    -- Overlap error because of Safe Haskell (first
    -- match should be the most specific match)
    safe_haskell_msg
     = assert (matches `lengthIs` 1 && not (null unsafe_ispecs)) $
       vcat [ addArising orig (text "Unsafe overlapping instances for"
                       <+> pprType (mkClassPred clas tys))
            , sep [text "The matching instance is:",
                   nest 2 (pprInstance $ head ispecs)]
            , vcat [ text "It is compiled in a Safe module and as such can only"
                   , text "overlap instances from the same module, however it"
                   , text "overlaps the following instances from different" <+>
                     text "modules:"
                   , nest 2 (vcat [pprInstances $ unsafe_ispecs])
                   ]
            ]


ctxtFixes :: Bool -> PredType -> [Implication] -> [SDoc]
ctxtFixes has_ambig_tvs pred implics
  | not has_ambig_tvs
  , isTyVarClassPred pred
  , (skol:skols) <- usefulContext implics pred
  , let what | null skols
             , SigSkol (PatSynCtxt {}) _ _ <- skol
             = text "\"required\""
             | otherwise
             = empty
  = [sep [ text "add" <+> pprParendType pred
           <+> text "to the" <+> what <+> text "context of"
         , nest 2 $ ppr_skol skol $$
                    vcat [ text "or" <+> ppr_skol skol
                         | skol <- skols ] ] ]
  | otherwise = []
  where
    ppr_skol (PatSkol (RealDataCon dc) _) = text "the data constructor" <+> quotes (ppr dc)
    ppr_skol (PatSkol (PatSynCon ps)   _) = text "the pattern synonym"  <+> quotes (ppr ps)
    ppr_skol skol_info = ppr skol_info

discardProvCtxtGivens :: CtOrigin -> [UserGiven] -> [UserGiven]
discardProvCtxtGivens orig givens  -- See Note [discardProvCtxtGivens]
  | ProvCtxtOrigin (PSB {psb_id = L _ name}) <- orig
  = filterOut (discard name) givens
  | otherwise
  = givens
  where
    discard n (Implic { ic_info = SigSkol (PatSynCtxt n') _ _ }) = n == n'
    discard _ _                                                  = False

usefulContext :: [Implication] -> PredType -> [SkolemInfo]
-- usefulContext picks out the implications whose context
-- the programmer might plausibly augment to solve 'pred'
usefulContext implics pred
  = go implics
  where
    pred_tvs = tyCoVarsOfType pred
    go [] = []
    go (ic : ics)
       | implausible ic = rest
       | otherwise      = ic_info ic : rest
       where
          -- Stop when the context binds a variable free in the predicate
          rest | any (`elemVarSet` pred_tvs) (ic_skols ic) = []
               | otherwise                                 = go ics

    implausible ic
      | null (ic_skols ic)            = True
      | implausible_info (ic_info ic) = True
      | otherwise                     = False

    implausible_info (SigSkol (InfSigCtxt {}) _ _) = True
    implausible_info _                             = False
    -- Do not suggest adding constraints to an *inferred* type signature

{- Note [Report candidate instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved (Num Int), where `Int` is not the Prelude Int,
but comes from some other module, then it may be helpful to point out
that there are some similarly named instances elsewhere.  So we get
something like
    No instance for (Num Int) arising from the literal ‘3’
    There are instances for similar types:
      instance Num GHC.Types.Int -- Defined in ‘GHC.Num’
Discussion in #9611.

Note [Highlighting ambiguous type variables]
~-------------------------------------------
When we encounter ambiguous type variables (i.e. type variables
that remain metavariables after type inference), we need a few more
conditions before we can reason that *ambiguity* prevents constraints
from being solved:
  - We can't have any givens, as encountering a typeclass error
    with given constraints just means we couldn't deduce
    a solution satisfying those constraints and as such couldn't
    bind the type variable to a known type.
  - If we don't have any unifiers, we don't even have potential
    instances from which an ambiguity could arise.
  - Lastly, I don't want to mess with error reporting for
    unknown runtime types so we just fall back to the old message there.
Once these conditions are satisfied, we can safely say that ambiguity prevents
the constraint from being solved.

Note [discardProvCtxtGivens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In most situations we call all enclosing implications "useful". There is one
exception, and that is when the constraint that causes the error is from the
"provided" context of a pattern synonym declaration:

  pattern Pat :: (Num a, Eq a) => Show a   => a -> Maybe a
             --  required      => provided => type
  pattern Pat x <- (Just x, 4)

When checking the pattern RHS we must check that it does actually bind all
the claimed "provided" constraints; in this case, does the pattern (Just x, 4)
bind the (Show a) constraint.  Answer: no!

But the implication we generate for this will look like
   forall a. (Num a, Eq a) => [W] Show a
because when checking the pattern we must make the required
constraints available, since they are needed to match the pattern (in
this case the literal '4' needs (Num a, Eq a)).

BUT we don't want to suggest adding (Show a) to the "required" constraints
of the pattern synonym, thus:
  pattern Pat :: (Num a, Eq a, Show a) => Show a => a -> Maybe a
It would then typecheck but it's silly.  We want the /pattern/ to bind
the alleged "provided" constraints, Show a.

So we suppress that Implication in discardProvCtxtGivens.  It's
painfully ad-hoc but the truth is that adding it to the "required"
constraints would work.  Suppressing it solves two problems.  First,
we never tell the user that we could not deduce a "provided"
constraint from the "required" context. Second, we never give a
possible fix that suggests to add a "provided" constraint to the
"required" context.

For example, without this distinction the above code gives a bad error
message (showing both problems):

  error: Could not deduce (Show a) ... from the context: (Eq a)
         ... Possible fix: add (Show a) to the context of
         the signature for pattern synonym `Pat' ...

-}

show_fixes :: [SDoc] -> SDoc
show_fixes []     = empty
show_fixes (f:fs) = sep [ text "Possible fix:"
                        , nest 2 (vcat (f : map (text "or" <+>) fs))]


-- Avoid boolean blindness
newtype PrintPotentialInstances = PrintPotentialInstances Bool

pprPotentials :: PrintPotentialInstances -> PprStyle -> SDoc -> [ClsInst] -> SDoc
-- See Note [Displaying potential instances]
pprPotentials (PrintPotentialInstances show_potentials) sty herald insts
  | null insts
  = empty

  | null show_these
  = hang herald
       2 (vcat [ not_in_scope_msg empty
               , flag_hint ])

  | otherwise
  = hang herald
       2 (vcat [ pprInstances show_these
               , ppWhen (n_in_scope_hidden > 0) $
                 text "...plus"
                   <+> speakNOf n_in_scope_hidden (text "other")
               , not_in_scope_msg (text "...plus")
               , flag_hint ])
  where
    n_show = 3 :: Int

    (in_scope, not_in_scope) = partition inst_in_scope insts
    sorted = sortBy fuzzyClsInstCmp in_scope
    show_these | show_potentials = sorted
               | otherwise       = take n_show sorted
    n_in_scope_hidden = length sorted - length show_these

       -- "in scope" means that all the type constructors
       -- are lexically in scope; these instances are likely
       -- to be more useful
    inst_in_scope :: ClsInst -> Bool
    inst_in_scope cls_inst = nameSetAll name_in_scope $
                             orphNamesOfTypes (is_tys cls_inst)

    name_in_scope name
      | isBuiltInSyntax name
      = True -- E.g. (->)
      | Just mod <- nameModule_maybe name
      = qual_in_scope (qualName sty mod (nameOccName name))
      | otherwise
      = True

    qual_in_scope :: QualifyName -> Bool
    qual_in_scope NameUnqual    = True
    qual_in_scope (NameQual {}) = True
    qual_in_scope _             = False

    not_in_scope_msg herald
      | null not_in_scope
      = empty
      | otherwise
      = hang (herald <+> speakNOf (length not_in_scope) (text "instance")
                     <+> text "involving out-of-scope types")
           2 (ppWhen show_potentials (pprInstances not_in_scope))

    flag_hint = ppUnless (show_potentials || equalLength show_these insts) $
                text "(use -fprint-potential-instances to see them all)"

{- Note [Displaying potential instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When showing a list of instances for
  - overlapping instances (show ones that match)
  - no such instance (show ones that could match)
we want to give it a bit of structure.  Here's the plan

* Say that an instance is "in scope" if all of the
  type constructors it mentions are lexically in scope.
  These are the ones most likely to be useful to the programmer.

* Show at most n_show in-scope instances,
  and summarise the rest ("plus 3 others")

* Summarise the not-in-scope instances ("plus 4 not in scope")

* Add the flag -fshow-potential-instances which replaces the
  summary with the full list
-}

{-
Note [Flattening in error message generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (C (Maybe (F x))), where F is a type function, and we have
instances
                C (Maybe Int) and C (Maybe a)
Since (F x) might turn into Int, this is an overlap situation, and
indeed (because of flattening) the main solver will have refrained
from solving.  But by the time we get to error message generation, we've
un-flattened the constraint.  So we must *re*-flatten it before looking
up in the instance environment, lest we only report one matching
instance when in fact there are two.

Re-flattening is pretty easy, because we don't need to keep track of
evidence.  We don't re-use the code in GHC.Tc.Solver.Canonical because that's in
the TcS monad, and we are in TcM here.

Note [Kind arguments in error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be terribly confusing to get an error message like (#9171)

    Couldn't match expected type ‘GetParam Base (GetParam Base Int)’
                with actual type ‘GetParam Base (GetParam Base Int)’

The reason may be that the kinds don't match up.  Typically you'll get
more useful information, but not when it's as a result of ambiguity.

To mitigate this, GHC attempts to enable the -fprint-explicit-kinds flag
whenever any error message arises due to a kind mismatch. This means that
the above error message would instead be displayed as:

    Couldn't match expected type
                  ‘GetParam @* @k2 @* Base (GetParam @* @* @k2 Base Int)’
                with actual type
                  ‘GetParam @* @k20 @* Base (GetParam @* @* @k20 Base Int)’

Which makes it clearer that the culprit is the mismatch between `k2` and `k20`.
-}

mkAmbigMsg :: Bool -- True when message has to be at beginning of sentence
           -> Ct -> (Bool, SDoc)
mkAmbigMsg prepend_msg ct
  | null ambig_kvs && null ambig_tvs = (False, empty)
  | otherwise                        = (True,  msg)
  where
    (ambig_kvs, ambig_tvs) = getAmbigTkvs ct

    msg |  any isRuntimeUnkSkol ambig_kvs  -- See Note [Runtime skolems]
        || any isRuntimeUnkSkol ambig_tvs
        = vcat [ text "Cannot resolve unknown runtime type"
                 <> plural ambig_tvs <+> pprQuotedList ambig_tvs
               , text "Use :print or :force to determine these types"]

        | not (null ambig_tvs)
        = pp_ambig (text "type") ambig_tvs

        | otherwise
        = pp_ambig (text "kind") ambig_kvs

    pp_ambig what tkvs
      | prepend_msg -- "Ambiguous type variable 't0'"
      = text "Ambiguous" <+> what <+> text "variable"
        <> plural tkvs <+> pprQuotedList tkvs

      | otherwise -- "The type variable 't0' is ambiguous"
      = text "The" <+> what <+> text "variable" <> plural tkvs
        <+> pprQuotedList tkvs <+> isOrAre tkvs <+> text "ambiguous"

pprSkols :: ReportErrCtxt -> [TcTyVar] -> SDoc
pprSkols ctxt tvs
  = vcat (map pp_one (getSkolemInfo (cec_encl ctxt) tvs))
  where
    pp_one (UnkSkol, tvs)
      = hang (pprQuotedList tvs)
           2 (is_or_are tvs "an" "unknown")
    pp_one (RuntimeUnkSkol, tvs)
      = hang (pprQuotedList tvs)
           2 (is_or_are tvs "an" "unknown runtime")
    pp_one (skol_info, tvs)
      = vcat [ hang (pprQuotedList tvs)
                  2 (is_or_are tvs "a"  "rigid" <+> text "bound by")
             , nest 2 (pprSkolInfo skol_info)
             , nest 2 (text "at" <+> ppr (foldr1 combineSrcSpans (map getSrcSpan tvs))) ]

    is_or_are [_] article adjective = text "is" <+> text article <+> text adjective
                                      <+> text "type variable"
    is_or_are _   _       adjective = text "are" <+> text adjective
                                      <+> text "type variables"

getAmbigTkvs :: Ct -> ([Var],[Var])
getAmbigTkvs ct
  = partition (`elemVarSet` dep_tkv_set) ambig_tkvs
  where
    tkvs       = tyCoVarsOfCtList ct
    ambig_tkvs = filter isAmbiguousTyVar tkvs
    dep_tkv_set = tyCoVarsOfTypes (map tyVarKind tkvs)

getSkolemInfo :: [Implication] -> [TcTyVar]
              -> [(SkolemInfo, [TcTyVar])]                    -- #14628
-- Get the skolem info for some type variables
-- from the implication constraints that bind them.
--
-- In the returned (skolem, tvs) pairs, the 'tvs' part is non-empty
getSkolemInfo _ []
  = []

getSkolemInfo [] tvs
  | all isRuntimeUnkSkol tvs = [(RuntimeUnkSkol, tvs)]        -- #14628
  | otherwise = pprPanic "No skolem info:" (ppr tvs)

getSkolemInfo (implic:implics) tvs
  | null tvs_here =                            getSkolemInfo implics tvs
  | otherwise   = (ic_info implic, tvs_here) : getSkolemInfo implics tvs_other
  where
    (tvs_here, tvs_other) = partition (`elem` ic_skols implic) tvs

-----------------------
-- relevantBindings looks at the value environment and finds values whose
-- types mention any of the offending type variables.  It has to be
-- careful to zonk the Id's type first, so it has to be in the monad.
-- We must be careful to pass it a zonked type variable, too.
--
-- We always remove closed top-level bindings, though,
-- since they are never relevant (cf #8233)

relevantBindings :: Bool  -- True <=> filter by tyvar; False <=> no filtering
                          -- See #8191
                 -> ReportErrCtxt -> Ct
                 -> TcM (ReportErrCtxt, SDoc, Ct)
-- Also returns the zonked and tidied CtOrigin of the constraint
relevantBindings want_filtering ctxt ct
  = do { traceTc "relevantBindings" (ppr ct)
       ; (env1, tidy_orig) <- zonkTidyOrigin (cec_tidy ctxt) (ctLocOrigin loc)

             -- For *kind* errors, report the relevant bindings of the
             -- enclosing *type* equality, because that's more useful for the programmer
       ; let extra_tvs = case tidy_orig of
                             KindEqOrigin t1 m_t2 _ _ -> tyCoVarsOfTypes $
                                                         t1 : maybeToList m_t2
                             _                        -> emptyVarSet
             ct_fvs = tyCoVarsOfCt ct `unionVarSet` extra_tvs

             -- Put a zonked, tidied CtOrigin into the Ct
             loc'   = setCtLocOrigin loc tidy_orig
             ct'    = setCtLoc ct loc'
             ctxt1  = ctxt { cec_tidy = env1 }

       ; (ctxt2, doc) <- relevant_bindings want_filtering ctxt1 lcl_env ct_fvs
       ; return (ctxt2, doc, ct') }
  where
    loc     = ctLoc ct
    lcl_env = ctLocEnv loc

-- slightly more general version, to work also with holes
relevant_bindings :: Bool
                  -> ReportErrCtxt
                  -> TcLclEnv
                  -> TyCoVarSet
                  -> TcM (ReportErrCtxt, SDoc)
relevant_bindings want_filtering ctxt lcl_env ct_tvs
  = do { dflags <- getDynFlags
       ; traceTc "relevant_bindings" $
           vcat [ ppr ct_tvs
                , pprWithCommas id [ ppr id <+> dcolon <+> ppr (idType id)
                                   | TcIdBndr id _ <- tcl_bndrs lcl_env ]
                , pprWithCommas id
                    [ ppr id | TcIdBndr_ExpType id _ _ <- tcl_bndrs lcl_env ] ]

       ; (tidy_env', docs, discards)
              <- go dflags (cec_tidy ctxt) (maxRelevantBinds dflags)
                    emptyVarSet [] False
                    (removeBindingShadowing $ tcl_bndrs lcl_env)
         -- tcl_bndrs has the innermost bindings first,
         -- which are probably the most relevant ones

       ; let doc = ppUnless (null docs) $
                   hang (text "Relevant bindings include")
                      2 (vcat docs $$ ppWhen discards discardMsg)

             ctxt' = ctxt { cec_tidy = tidy_env' }

       ; return (ctxt', doc) }
  where
    run_out :: Maybe Int -> Bool
    run_out Nothing = False
    run_out (Just n) = n <= 0

    dec_max :: Maybe Int -> Maybe Int
    dec_max = fmap (\n -> n - 1)


    go :: DynFlags -> TidyEnv -> Maybe Int -> TcTyVarSet -> [SDoc]
       -> Bool                          -- True <=> some filtered out due to lack of fuel
       -> [TcBinder]
       -> TcM (TidyEnv, [SDoc], Bool)   -- The bool says if we filtered any out
                                        -- because of lack of fuel
    go _ tidy_env _ _ docs discards []
      = return (tidy_env, reverse docs, discards)
    go dflags tidy_env n_left tvs_seen docs discards (tc_bndr : tc_bndrs)
      = case tc_bndr of
          TcTvBndr {} -> discard_it
          TcIdBndr id top_lvl -> go2 (idName id) (idType id) top_lvl
          TcIdBndr_ExpType name et top_lvl ->
            do { mb_ty <- readExpType_maybe et
                   -- et really should be filled in by now. But there's a chance
                   -- it hasn't, if, say, we're reporting a kind error en route to
                   -- checking a term. See test indexed-types/should_fail/T8129
                   -- Or we are reporting errors from the ambiguity check on
                   -- a local type signature
               ; case mb_ty of
                   Just ty -> go2 name ty top_lvl
                   Nothing -> discard_it  -- No info; discard
               }
      where
        discard_it = go dflags tidy_env n_left tvs_seen docs
                        discards tc_bndrs
        go2 id_name id_type top_lvl
          = do { (tidy_env', tidy_ty) <- zonkTidyTcType tidy_env id_type
               ; traceTc "relevantBindings 1" (ppr id_name <+> dcolon <+> ppr tidy_ty)
               ; let id_tvs = tyCoVarsOfType tidy_ty
                     doc = sep [ pprPrefixOcc id_name <+> dcolon <+> ppr tidy_ty
                               , nest 2 (parens (text "bound at"
                                    <+> ppr (getSrcLoc id_name)))]
                     new_seen = tvs_seen `unionVarSet` id_tvs

               ; if (want_filtering && not (hasPprDebug dflags)
                                    && id_tvs `disjointVarSet` ct_tvs)
                          -- We want to filter out this binding anyway
                          -- so discard it silently
                 then discard_it

                 else if isTopLevel top_lvl && not (isNothing n_left)
                          -- It's a top-level binding and we have not specified
                          -- -fno-max-relevant-bindings, so discard it silently
                 then discard_it

                 else if run_out n_left && id_tvs `subVarSet` tvs_seen
                          -- We've run out of n_left fuel and this binding only
                          -- mentions already-seen type variables, so discard it
                 then go dflags tidy_env n_left tvs_seen docs
                         True      -- Record that we have now discarded something
                         tc_bndrs

                          -- Keep this binding, decrement fuel
                 else go dflags tidy_env' (dec_max n_left) new_seen
                         (doc:docs) discards tc_bndrs }


discardMsg :: SDoc
discardMsg = text "(Some bindings suppressed;" <+>
             text "use -fmax-relevant-binds=N or -fno-max-relevant-binds)"

-----------------------
warnDefaulting :: [Ct] -> Type -> TcM ()
warnDefaulting wanteds default_ty
  = do { warn_default <- woptM Opt_WarnTypeDefaults
       ; env0 <- tcInitTidyEnv
       ; let tidy_env = tidyFreeTyCoVars env0 $
                        tyCoVarsOfCtsList (listToBag wanteds)
             tidy_wanteds = map (tidyCt tidy_env) wanteds
             (loc, ppr_wanteds) = pprWithArising tidy_wanteds
             warn_msg =
                hang (hsep [ text "Defaulting the following"
                           , text "constraint" <> plural tidy_wanteds
                           , text "to type"
                           , quotes (ppr default_ty) ])
                     2
                     ppr_wanteds
       ; setCtLocM loc $ warnTc (Reason Opt_WarnTypeDefaults) warn_default warn_msg }

{-
Note [Runtime skolems]
~~~~~~~~~~~~~~~~~~~~~~
We want to give a reasonably helpful error message for ambiguity
arising from *runtime* skolems in the debugger.  These
are created by in GHC.Runtime.Heap.Inspect.zonkRTTIType.

************************************************************************
*                                                                      *
                 Error from the canonicaliser
         These ones are called *during* constraint simplification
*                                                                      *
************************************************************************
-}

solverDepthErrorTcS :: CtLoc -> TcType -> TcM a
solverDepthErrorTcS loc ty
  = setCtLocM loc $
    do { ty <- zonkTcType ty
       ; env0 <- tcInitTidyEnv
       ; let tidy_env     = tidyFreeTyCoVars env0 (tyCoVarsOfTypeList ty)
             tidy_ty      = tidyType tidy_env ty
             msg
               = vcat [ text "Reduction stack overflow; size =" <+> ppr depth
                      , hang (text "When simplifying the following type:")
                           2 (ppr tidy_ty)
                      , note ]
       ; failWithTcM (tidy_env, msg) }
  where
    depth = ctLocDepth loc
    note = vcat
      [ text "Use -freduction-depth=0 to disable this check"
      , text "(any upper bound you could choose might fail unpredictably with"
      , text " minor updates to GHC, so disabling the check is recommended if"
      , text " you're sure that type checking should terminate)" ]
