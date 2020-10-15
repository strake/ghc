-- | COMPLETE signature
module GHC.Types.CompleteMatch
   ( CompleteMatch (..)
   , CompleteMatchMap
   , mkCompleteMatchMap
   , extendCompleteMatchMap
   )
where

import Data.Foldable (foldl')
import Data.List ((++))
import GHC.Types.Name
import GHC.Types.Unique.FM
import GHC.Utils.Outputable

-- | A list of conlikes which represents a complete pattern match.
-- These arise from @COMPLETE@ signatures.

-- See Note [Implementation of COMPLETE signatures]
data CompleteMatch = CompleteMatch {
                            completeMatchConLikes :: [Name]
                            -- ^ The ConLikes that form a covering family
                            -- (e.g. Nothing, Just)
                          , completeMatchTyCon :: Name
                            -- ^ The TyCon that they cover (e.g. Maybe)
                          }

instance Outputable CompleteMatch where
  ppr (CompleteMatch cl ty) = text "CompleteMatch:" <+> ppr cl
                                                    <+> dcolon <+> ppr ty

-- | A map keyed by the 'completeMatchTyCon' which has type Name.

-- See Note [Implementation of COMPLETE signatures]
type CompleteMatchMap = UniqFM Name [CompleteMatch]

mkCompleteMatchMap :: [CompleteMatch] -> CompleteMatchMap
mkCompleteMatchMap = extendCompleteMatchMap emptyUFM

extendCompleteMatchMap :: CompleteMatchMap -> [CompleteMatch]
                       -> CompleteMatchMap
extendCompleteMatchMap = foldl' insertMatch
  where
    insertMatch :: CompleteMatchMap -> CompleteMatch -> CompleteMatchMap
    insertMatch ufm c@(CompleteMatch _ t) = addToUFM_C (++) ufm t [c]

{-
Note [Implementation of COMPLETE signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A COMPLETE signature represents a set of conlikes (i.e., constructors or
pattern synonyms) such that if they are all pattern-matched against in a
function, it gives rise to a total function. An example is:

  newtype Boolean = Boolean Int
  pattern F, T :: Boolean
  pattern F = Boolean 0
  pattern T = Boolean 1
  {-# COMPLETE F, T #-}

  -- This is a total function
  booleanToInt :: Boolean -> Int
  booleanToInt F = 0
  booleanToInt T = 1

COMPLETE sets are represented internally in GHC with the CompleteMatch data
type. For example, {-# COMPLETE F, T #-} would be represented as:

  CompleteMatch { complateMatchConLikes = [F, T]
                , completeMatchTyCon    = Boolean }

Note that GHC was able to infer the completeMatchTyCon (Boolean), but for the
cases in which it's ambiguous, you can also explicitly specify it in the source
language by writing this:

  {-# COMPLETE F, T :: Boolean #-}

For efficiency purposes, GHC collects all of the CompleteMatches that it knows
about into a CompleteMatchMap, which is a map that is keyed by the
completeMatchTyCon. In other words, you could have a multiple COMPLETE sets
for the same TyCon:

  {-# COMPLETE F, T1 :: Boolean #-}
  {-# COMPLETE F, T2 :: Boolean #-}

And looking up the values in the CompleteMatchMap associated with Boolean
would give you [CompleteMatch [F, T1] Boolean, CompleteMatch [F, T2] Boolean].
dsGetCompleteMatches in GHC.HsToCore.Quote accomplishes this lookup.

Also see Note [Typechecking Complete Matches] in GHC.Tc.Gen.Bind for a more detailed
explanation for how GHC ensures that all the conlikes in a COMPLETE set are
consistent.
-}
