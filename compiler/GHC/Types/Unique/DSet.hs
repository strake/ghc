-- (c) Bartosz Nitka, Facebook, 2015

-- |
-- Specialised deterministic sets, for things with @Uniques@
--
-- Based on 'UniqDFM's (as you would expect).
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" for explanation why we need it.
--
-- Basically, the things need to be in class 'Uniquable'.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Types.Unique.DSet (
        -- * Unique set type
        UniqDSet,    -- type synonym for UniqFM a
        getUniqDSet,
        pprUniqDSet,

        -- ** Manipulating these sets
        delOneFromUniqDSet, delListFromUniqDSet,
        emptyUniqDSet,
        unitUniqDSet,
        mkUniqDSet,
        addOneToUniqDSet, addListToUniqDSet,
        unionUniqDSets, unionManyUniqDSets,
        minusUniqDSet, uniqDSetMinusUniqSet,
        intersectUniqDSets, uniqDSetIntersectUniqSet,
        nonDetStrictFoldUniqDSet,
        elementOfUniqDSet,
        filterUniqDSet,
        sizeUniqDSet,
        isEmptyUniqDSet,
        lookupUniqDSet,
        uniqDSetToList,
        partitionUniqDSet,
        mapUniqDSet
    ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Types.Unique.DFM
import GHC.Types.Unique.Set
import GHC.Types.Unique
import GHC.Data.Collections

import Control.Monad
import Data.Coerce
import Data.Data
import Data.Foldable (Foldable (..))

-- See Note [UniqSet invariant] in GHC.Types.Unique.Set for why we want a newtype here.
-- Beyond preserving invariants, we may also want to 'override' typeclass
-- instances.

newtype UniqDSet a = UniqDSet {getUniqDSet' :: UniqDFM a a}
                   deriving (Data)

instance Foldable UniqDSet where
    foldMap f (UniqDSet as) = foldMap f as
    foldr f z (UniqDSet as) = foldr f z as
    null (UniqDSet as) = null as
    length (UniqDSet as) = length as

instance Uniquable a => IsSet (UniqDSet a) where
    type ElemOf (UniqDSet a) = a
    setNull (UniqDSet as) = null as
    setSize (UniqDSet as) = length as
    setMember a (UniqDSet as) = mapMember a as
    setEmpty = UniqDSet mapEmpty
    setSingleton = UniqDSet . join mapSingleton
    setInsert a (UniqDSet as) = UniqDSet (mapInsert a a as)
    setDelete a (UniqDSet as) = UniqDSet (mapDelete a as)
    setUnion (UniqDSet as) (UniqDSet bs) = UniqDSet (mapUnion as bs)
    setDifference (UniqDSet as) (UniqDSet bs) = UniqDSet (mapDifference as bs)
    setIntersection (UniqDSet as) (UniqDSet bs) = UniqDSet (mapIntersection as bs)
    setIsSubsetOf (UniqDSet as) (UniqDSet bs) = mapIsSubmapOfBy (\ _ _ -> True) as bs
    setFilter = filterUniqDSet
    setFoldl f z (UniqDSet as) = foldl f z as
    setFoldr f z (UniqDSet as) = foldr f z as
    setElems (UniqDSet as) = toList as
    setFromList = mkUniqDSet

emptyUniqDSet :: UniqDSet a
emptyUniqDSet = UniqDSet emptyUDFM

unitUniqDSet :: Uniquable a => a -> UniqDSet a
unitUniqDSet x = UniqDSet (unitUDFM x x)

mkUniqDSet :: Uniquable a => [a] -> UniqDSet a
mkUniqDSet = foldl' addOneToUniqDSet emptyUniqDSet

-- The new element always goes to the right of existing ones.
addOneToUniqDSet :: Uniquable a => UniqDSet a -> a -> UniqDSet a
addOneToUniqDSet (UniqDSet set) x = UniqDSet (addToUDFM set x x)

addListToUniqDSet :: Uniquable a => UniqDSet a -> [a] -> UniqDSet a
addListToUniqDSet = foldl' addOneToUniqDSet

delOneFromUniqDSet :: Uniquable a => UniqDSet a -> a -> UniqDSet a
delOneFromUniqDSet (UniqDSet s) = UniqDSet . delFromUDFM s

delListFromUniqDSet :: Uniquable a => UniqDSet a -> [a] -> UniqDSet a
delListFromUniqDSet (UniqDSet s) = UniqDSet . delListFromUDFM s

unionUniqDSets :: UniqDSet a -> UniqDSet a -> UniqDSet a
unionUniqDSets (UniqDSet s) (UniqDSet t) = UniqDSet (plusUDFM s t)

unionManyUniqDSets :: [UniqDSet a] -> UniqDSet a
unionManyUniqDSets []     = emptyUniqDSet
unionManyUniqDSets (x:xs) = foldl' unionUniqDSets x xs

minusUniqDSet :: UniqDSet a -> UniqDSet a -> UniqDSet a
minusUniqDSet (UniqDSet s) (UniqDSet t) = UniqDSet (minusUDFM s t)

uniqDSetMinusUniqSet :: UniqDSet a -> UniqSet a -> UniqDSet a
uniqDSetMinusUniqSet xs ys
  = UniqDSet (udfmMinusUFM (getUniqDSet xs) (getUniqSet ys))

intersectUniqDSets :: UniqDSet a -> UniqDSet a -> UniqDSet a
intersectUniqDSets (UniqDSet s) (UniqDSet t) = UniqDSet (intersectUDFM s t)

uniqDSetIntersectUniqSet :: UniqDSet a -> UniqSet a -> UniqDSet a
uniqDSetIntersectUniqSet xs ys
  = UniqDSet (udfmIntersectUFM (getUniqDSet xs) (getUniqSet ys))

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldUniqDSet :: (a -> b -> b) -> b -> UniqDSet a -> b
nonDetStrictFoldUniqDSet f acc (UniqDSet s) = nonDetStrictFoldUDFM f acc s

elementOfUniqDSet :: Uniquable a => a -> UniqDSet a -> Bool
elementOfUniqDSet k = elemUDFM k . getUniqDSet

filterUniqDSet :: (a -> Bool) -> UniqDSet a -> UniqDSet a
filterUniqDSet p (UniqDSet s) = UniqDSet (filter p s)

sizeUniqDSet :: UniqDSet a -> Int
sizeUniqDSet = length . getUniqDSet

isEmptyUniqDSet :: UniqDSet a -> Bool
isEmptyUniqDSet = null . getUniqDSet

lookupUniqDSet :: Uniquable a => UniqDSet a -> a -> Maybe a
lookupUniqDSet = lookupUDFM . getUniqDSet

uniqDSetToList :: UniqDSet a -> [a]
uniqDSetToList = toList . getUniqDSet

partitionUniqDSet :: (a -> Bool) -> UniqDSet a -> (UniqDSet a, UniqDSet a)
partitionUniqDSet p = coerce . partition p . getUniqDSet

-- See Note [UniqSet invariant] in GHC.Types.Unique.Set
mapUniqDSet :: Uniquable b => (a -> b) -> UniqDSet a -> UniqDSet b
mapUniqDSet f = mkUniqDSet . map f . uniqDSetToList

-- Two 'UniqDSet's are considered equal if they contain the same
-- uniques.
instance Eq (UniqDSet a) where
  UniqDSet a == UniqDSet b = equalKeysUDFM a b

getUniqDSet :: UniqDSet a -> UniqDFM a a
getUniqDSet = getUniqDSet'

instance Outputable a => Outputable (UniqDSet a) where
  ppr = pprUniqDSet ppr

pprUniqDSet :: (a -> SDoc) -> UniqDSet a -> SDoc
pprUniqDSet f = braces . pprWithCommas f . uniqDSetToList
