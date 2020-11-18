module GHC.Core.FamInstEnv.Injectivity (
    InjectivityCheckResult (..),
    injectiveBranches,
    lookupFamInstEnvInjectivityConflicts,
) where

import GHC.Prelude

import GHC.Core.Unify
import GHC.Core.Type as Type
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Types.Unique.DFM
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Core.FamInstEnv
import Control.Monad
import Data.Foldable( toList )
import Data.Function( on )
import Data.List.NonEmpty( NonEmpty (..) )
import qualified Data.IntSet as IntSet

-- | Result of testing two type family equations for injectiviy.
data InjectivityCheckResult
   = InjectivityAccepted
    -- ^ Either RHSs are distinct or unification of RHSs leads to unification of
    -- LHSs
   | InjectivityUnified CoAxBranch CoAxBranch
    -- ^ RHSs unify but LHSs don't unify under that substitution.  Relevant for
    -- closed type families where equation after unification might be
    -- overlpapped (in which case it is OK if they don't unify).  Constructor
    -- stores axioms after unification.

-- | Check whether two type family axioms don't violate injectivity annotation.
injectiveBranches :: Injectivity -> CoAxBranch -> CoAxBranch -> InjectivityCheckResult
injectiveBranches inj
    ax1@CoAxBranch { cab_lhs = lhs1, cab_rhs = rhs1 }
    ax2@CoAxBranch { cab_lhs = lhs2, cab_rhs = rhs2 }
  -- See Note [Verifying injectivity annotation], case 1.
  = case foldr combine Nothing $
    uncurry injectiveBranches' <$> rearrangeBranch inj (zip lhs1 lhs2, (rhs1, rhs2)) of
        Nothing -> InjectivityAccepted
        -- payload of InjectivityUnified used only for check 1B2, only for closed type families
        Just subst -> InjectivityUnified
            ax1 { cab_lhs = Type.substTys subst lhs1, cab_rhs = Type.substTy subst rhs1 }
            ax2 { cab_lhs = Type.substTys subst lhs2, cab_rhs = Type.substTy subst rhs2 }
  where
    combine Nothing Nothing = Nothing
    combine a b = Just $ (unionTCvSubst `on` fromMaybe emptyTCvSubst) a b

injectiveBranches'
 :: (Foldable f, Foldable g) => f (Type, Type) -> g (Type, Type) -> Maybe TCvSubst
injectiveBranches' lhs rhs = do
    subst <- tcUnifyTysWithTFs True `uncurry` unzip (toList rhs) -- True = two-way pre-unification
    -- RHS are different, so equations are injective.
    -- This is case 1A from Note [Verifying injectivity annotation]
    -- RHS unify under a substitution

    -- If LHSs are equal under the substitution used for RHSs then this pair
    -- of equations does not violate injectivity annotation. If LHSs are not
    -- equal under that substitution then this pair of equations violates
    -- injectivity annotation, but for closed type families it still might
    -- be the case that one LHS after substitution is unreachable.
    subst <$ guard (not $ (eqTypes `on` Type.substTys subst) `uncurry` unzip (toList lhs))

rearrangeBranch :: Injectivity -> ([a], a) -> [(NonEmpty a, NonEmpty a)]
rearrangeBranch (Injectivity inj) (lhs, rhs) =
  [ (injRhs, rhs :| (lhs !!?) `mapMaybe` IntSet.toList injLhs)
  | (injLhs, injRhs) <- gather
    [(injLhs, injRhs) | (Injectivity1 inj1, injRhs) <- zip inj lhs, injLhs <- toList inj1]
  ]

--------------------------------------------------------------------------------
--                 Type family injectivity checking bits                      --
--------------------------------------------------------------------------------

{- Note [Verifying injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Injectivity means that the RHS of a type family uniquely determines the LHS (see
Note [Type inference for type families with injectivity]).  The user informs us about
injectivity using an injectivity annotation and it is GHC's task to verify that
this annotation is correct w.r.t. type family equations. Whenever we see a new
equation of a type family we need to make sure that adding this equation to the
already known equations of a type family does not violate the injectivity annotation
supplied by the user (see Note [Injectivity annotation]).  Of course if the type
family has no injectivity annotation then no check is required.  But if a type
family has injectivity annotation we need to make sure that the following
conditions hold:

1. For each pair of *different* equations of a type family, one of the following
   conditions holds:

   A:  RHSs are different. (Check done in GHC.Core.FamInstEnv.injectiveBranches)

   B1: OPEN TYPE FAMILIES: If the RHSs can be unified under some substitution
       then it must be possible to unify the LHSs under the same substitution.
       Example:

          type family FunnyId a = r | r -> a
          type instance FunnyId Int = Int
          type instance FunnyId a = a

       RHSs of these two equations unify under [ a |-> Int ] substitution.
       Under this substitution LHSs are equal therefore these equations don't
       violate injectivity annotation. (Check done in GHC.Core.FamInstEnv.injectiveBranches)

   B2: CLOSED TYPE FAMILIES: If the RHSs can be unified under some
       substitution then either the LHSs unify under the same substitution or
       the LHS of the latter equation is overlapped by earlier equations.
       Example 1:

          type family SwapIntChar a = r | r -> a where
              SwapIntChar Int  = Char
              SwapIntChar Char = Int
              SwapIntChar a    = a

       Say we are checking the last two equations. RHSs unify under [ a |->
       Int ] substitution but LHSs don't. So we apply the substitution to LHS
       of last equation and check whether it is overlapped by any of previous
       equations. Since it is overlapped by the first equation we conclude
       that pair of last two equations does not violate injectivity
       annotation. (Check done in GHC.Tc.Validity.checkValidCoAxiom#gather_conflicts)

   A special case of B is when RHSs unify with an empty substitution ie. they
   are identical.

   If any of the above two conditions holds we conclude that the pair of
   equations does not violate injectivity annotation. But if we find a pair
   of equations where neither of the above holds we report that this pair
   violates injectivity annotation because for a given RHS we don't have a
   unique LHS. (Note that (B) actually implies (A).)

   Note that we only take into account these LHS patterns that were declared
   as injective.

2. If an RHS of a type family equation is a bare type variable then
   all LHS variables (including implicit kind variables) also have to be bare.
   In other words, this has to be a sole equation of that type family and it has
   to cover all possible patterns.  So for example this definition will be
   rejected:

      type family W1 a = r | r -> a
      type instance W1 [a] = a

   If it were accepted we could call `W1 [W1 Int]`, which would reduce to
   `W1 Int` and then by injectivity we could conclude that `[W1 Int] ~ Int`,
   which is bogus. Checked FamInst.bareTvInRHSViolated.

3. If the RHS of a type family equation is a type family application then the type
   family is rejected as not injective. This is checked by FamInst.isTFHeaded.

4. If a LHS type variable that is declared as injective is not mentioned in an
   injective position in the RHS then the type family is rejected as not
   injective.  "Injective position" means either an argument to a type
   constructor or argument to a type family on injective position.
   There are subtleties here. See Note [Coverage condition for injective type families]
   in GHC.Tc.Instance.Family.

Check (1) must be done for all family instances (transitively) imported. Other
checks (2-4) should be done just for locally written equations, as they are checks
involving just a single equation, not about interactions. Doing the other checks for
imported equations led to #17405, as the behavior of check (4) depends on
-XUndecidableInstances (see Note [Coverage condition for injective type families] in
FamInst), which may vary between modules.

See also Note [Injective type families] in GHC.Core.TyCon
-}


-- | Check whether an open type family equation can be added to already existing
-- instance environment without causing conflicts with supplied injectivity
-- annotations.  Returns list of conflicting axioms (type instance
-- declarations).
lookupFamInstEnvInjectivityConflicts
    :: Injectivity    -- injectivity annotation for this type family instance
                      -- INVARIANT: list contains at least one non-empty value
    ->  FamInstEnvs   -- all type instances seens so far
    ->  FamInst       -- new type instance that we're checking
    -> [CoAxBranch]   -- conflicting instance declarations
lookupFamInstEnvInjectivityConflicts injList (pkg_ie, home_ie)
                             fam_inst@(FamInst { fi_axiom = new_axiom })
  -- See Note [Verifying injectivity annotation]. This function implements
  -- check (1.B1) for open type families described there.
  = lookup_inj_fam_conflicts home_ie ++ lookup_inj_fam_conflicts pkg_ie
    where
      fam        = famInstTyCon fam_inst
      new_branch = coAxiomSingleBranch new_axiom

      -- filtering function used by `lookup_inj_fam_conflicts` to check whether
      -- a pair of equations conflicts with the injectivity annotation.
      isInjConflict (FamInst { fi_axiom = old_axiom })
          | InjectivityAccepted <-
            injectiveBranches injList (coAxiomSingleBranch old_axiom) new_branch
          = False -- no conflict
          | otherwise = True

      lookup_inj_fam_conflicts ie
          | isOpenFamilyTyCon fam, Just (FamIE insts) <- lookupUDFM ie fam
          = coAxiomSingleBranch . fi_axiom <$> filter isInjConflict insts
          | otherwise = []
