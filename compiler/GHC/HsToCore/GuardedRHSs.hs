{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Matching guarded right-hand-sides (GRHSs)
-}

module GHC.HsToCore.GuardedRHSs ( dsGuarded, dsGRHSs, isTrueLHsExpr ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.HsToCore.Expr  ( dsLExpr, dsLocalBinds )
import {-# SOURCE #-} GHC.HsToCore.Match ( matchSinglePatVar )

import GHC.Hs
import GHC.Core.Make
import GHC.Core
import GHC.Core.Utils (bindNonRec)

import GHC.HsToCore.Monad
import GHC.HsToCore.Utils
import GHC.HsToCore.PmCheck.Types ( Deltas, initDeltas )
import GHC.HsToCore.Types
import GHC.Core.Type ( Type )
import GHC.Tc.Types
import GHC.Types.SrcLoc
import GHC.Utils.Lens.Monad
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import Control.Monad ( zipWithM )
import Data.List.NonEmpty ( NonEmpty, toList )

{-
@dsGuarded@ is used for pattern bindings.
It desugars:
\begin{verbatim}
        | g1 -> e1
        ...
        | gn -> en
        where binds
\end{verbatim}
producing an expression with a runtime error in the corner if
necessary.  The type argument gives the type of the @ei@.
-}

dsGuarded :: GRHSs GhcTc (LHsExpr GhcTc) -> Type -> Maybe (NonEmpty Deltas) -> DsM CoreExpr
dsGuarded grhss rhs_ty mb_rhss_deltas = do
    match_result <- dsGRHSs PatBindRhs grhss rhs_ty mb_rhss_deltas
    error_expr <- mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID rhs_ty mempty
    extractMatchResult match_result error_expr

-- In contrast, @dsGRHSs@ produces a @MatchResult CoreExpr@.

dsGRHSs :: HsMatchContext GhcRn
        -> GRHSs GhcTc (LHsExpr GhcTc) -- ^ Guarded RHSs
        -> Type                        -- ^ Type of RHS
        -> Maybe (NonEmpty Deltas)     -- ^ Refined pattern match checking
                                       --   models, one for each GRHS. Defaults
                                       --   to 'initDeltas' if 'Nothing'.
        -> DsM (MatchResult CoreExpr)
dsGRHSs hs_ctx (GRHSs _ grhss binds) rhs_ty mb_rhss_deltas
  = assert (notNull grhss) $
    -- NB: nested dsLet inside matchResult
    adjustMatchResultDs (dsLocalBinds binds) . foldr1 combineMatchResults <$> case toList <$> mb_rhss_deltas of
           Nothing          -> traverse (dsGRHS hs_ctx rhs_ty initDeltas) grhss
           Just rhss_deltas -> assert (length grhss == length rhss_deltas)
                               zipWithM (dsGRHS hs_ctx rhs_ty) rhss_deltas grhss

dsGRHS :: HsMatchContext GhcRn -> Type -> Deltas -> LGRHS GhcTc (LHsExpr GhcTc)
       -> DsM (MatchResult CoreExpr)
dsGRHS hs_ctx rhs_ty rhs_deltas (L _ (GRHS _ guards rhs))
  = locally (env_lclL . dsl_deltasL) (pure rhs_deltas) (matchGuards (unLoc <$> guards) (PatGuard hs_ctx) rhs rhs_ty)

{-
************************************************************************
*                                                                      *
*  matchGuard : make a MatchResult CoreExpr CoreExpr from a guarded RHS                  *
*                                                                      *
************************************************************************
-}

matchGuards :: [GuardStmt GhcTc]     -- Guard
            -> HsStmtContext GhcRn   -- Context
            -> LHsExpr GhcTc         -- RHS
            -> Type                  -- Type of RHS of guard
            -> DsM (MatchResult CoreExpr)

-- See comments with HsExpr.Stmt re what a BodyStmt means
-- Here we must be in a guard context (not do-expression, nor list-comp)

matchGuards [] _ rhs _
  = do  { core_rhs <- dsLExpr rhs
        ; return (cantFailMatchResult core_rhs) }

        -- BodyStmts must be guards
        -- Turn an "otherwise" guard is a no-op.  This ensures that
        -- you don't get a "non-exhaustive eqns" message when the guards
        -- finish in "otherwise".
        -- NB:  The success of this clause depends on the typechecker not
        --      wrapping the 'otherwise' in empty HsTyApp or HsWrap constructors
        --      If it does, you'll get bogus overlap warnings
matchGuards (BodyStmt _ e _ _ : stmts) ctx rhs rhs_ty
  | Just addTicks <- isTrueLHsExpr e = do
    adjustMatchResultDs addTicks <$> matchGuards stmts ctx rhs rhs_ty
matchGuards (BodyStmt _ expr _ _ : stmts) ctx rhs rhs_ty =
    flip mkGuardedMatchResult <$> matchGuards stmts ctx rhs rhs_ty <*> dsLExpr expr

matchGuards (LetStmt _ binds : stmts) ctx rhs rhs_ty =
    adjustMatchResultDs (dsLocalBinds binds) <$> matchGuards stmts ctx rhs rhs_ty
        -- NB the dsLet occurs inside the match_result
        -- Reason: dsLet takes the body expression as its argument
        --         so we can't desugar the bindings without the
        --         body expression in hand

matchGuards (BindStmt _ pat bind_rhs : stmts) ctx rhs rhs_ty = do
    let upat = unLoc pat
    match_var <- selectMatchVar upat

    match_result <- matchGuards stmts ctx rhs rhs_ty
    core_rhs <- dsLExpr bind_rhs
    fmap (bindNonRec match_var core_rhs) <$>
        matchSinglePatVar match_var (StmtCtxt ctx) pat rhs_ty match_result

matchGuards (LastStmt  {} : _) _ _ _ = panic "matchGuards LastStmt"
matchGuards (ParStmt   {} : _) _ _ _ = panic "matchGuards ParStmt"
matchGuards (TransStmt {} : _) _ _ _ = panic "matchGuards TransStmt"
matchGuards (RecStmt   {} : _) _ _ _ = panic "matchGuards RecStmt"
matchGuards (ApplicativeStmt {} : _) _ _ _ =
  panic "matchGuards ApplicativeLastStmt"

{-
Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise          = r2
\end{verbatim}
-}
