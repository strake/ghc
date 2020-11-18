module GHC.Tc.Validity.CoAxiom (checkValidCoAxiom, checkValidCoAxBranch) where

import GHC.Prelude

-- friends:
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.TyCon
import GHC.Tc.Validity

-- others:
import GHC.Tc.Utils.Monad
import GHC.Core.FamInstEnv
   ( isDominatedBy )
import GHC.Core.FamInstEnv.Injectivity
   ( injectiveBranches, InjectivityCheckResult(..) )
import GHC.Tc.Instance.Family
import GHC.Driver.Session
import GHC.Utils.Outputable as Outputable

import Control.Monad
import Data.Foldable (toList)
import qualified Data.IntSet as IntSet

{-
************************************************************************
*                                                                      *
        Checking type instance well-formedness and termination
*                                                                      *
************************************************************************
-}

checkValidCoAxiom :: CoAxiom Branched -> TcM ()
checkValidCoAxiom ax@(CoAxiom { co_ax_tc = fam_tc, co_ax_branches = branches })
  = do { traceTc "checkValidCoAxiom" $
         vcat [ppr ax, ppr branch_list, ppr $ fmap IntSet.toList . toList . unInjectivity1 <$> inj']
       ; mapM_ (checkValidCoAxBranch fam_tc) branch_list
       ; foldlM_ check_branch_compat [] branch_list }
  where
    branch_list = fromBranches branches
    inj@(Injectivity inj') = tyConInjectivityInfo fam_tc

    check_branch_compat :: [CoAxBranch]    -- previous branches in reverse order
                        -> CoAxBranch      -- current branch
                        -> TcM [CoAxBranch]-- current branch : previous branches
    -- Check for
    --   (a) this branch is dominated by previous ones
    --   (b) failure of injectivity
    check_branch_compat prev_branches cur_branch
      | cur_branch `isDominatedBy` prev_branches
      = do { addWarnAt NoReason (coAxBranchSpan cur_branch) $
             inaccessibleCoAxBranch fam_tc cur_branch
           ; return prev_branches }
      | otherwise
      = do { check_injectivity prev_branches cur_branch
           ; return (cur_branch : prev_branches) }

     -- Injectivity check: check whether a new (CoAxBranch) can extend
     -- already checked equations without violating injectivity
     -- annotation supplied by the user.
     -- See Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv
    check_injectivity prev_branches cur_branch = when (any (/= NotInjective1) inj') $ do
        dflags <- getDynFlags
        let (conflicts, _) =
                foldl' (gather_conflicts inj prev_branches cur_branch)
                ([], 0) prev_branches
        traceTc "checkValidCoAxiom.check_injectivity" (ppr (prev_branches, cur_branch, conflicts))
        reportConflictingInjectivityErrs fam_tc conflicts cur_branch
        reportInjectivityErrors dflags ax cur_branch inj

    gather_conflicts inj prev_branches cur_branch (acc, n) branch
               -- n is 0-based index of branch in prev_branches
      = case injectiveBranches inj cur_branch branch of
           -- Case 1B2 in Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv
          InjectivityUnified ax1 ax2
            | ax1 `isDominatedBy` (replace_br prev_branches n ax2)
                -> (acc, n + 1)
            | otherwise
                -> (branch : acc, n + 1)
          InjectivityAccepted -> (acc, n + 1)

    -- Replace n-th element in the list. Assumes 0-based indexing.
    replace_br :: [CoAxBranch] -> Int -> CoAxBranch -> [CoAxBranch]
    replace_br brs n br = take n brs ++ [br] ++ drop (n+1) brs


-- Check that a "type instance" is well-formed (which includes decidability
-- unless -XUndecidableInstances is given).
--
checkValidCoAxBranch :: TyCon -> CoAxBranch -> TcM ()
checkValidCoAxBranch fam_tc
                    (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                                , cab_lhs = typats
                                , cab_rhs = rhs, cab_loc = loc })
  = setSrcSpan loc $
    checkValidTyFamEqn fam_tc (tvs++cvs) typats rhs

-- Error messages

inaccessibleCoAxBranch :: TyCon -> CoAxBranch -> SDoc
inaccessibleCoAxBranch fam_tc cur_branch
  = text "Type family instance equation is overlapped:" $$
    nest 2 (pprCoAxBranchUser fam_tc cur_branch)
