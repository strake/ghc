-- (c) Bartosz Nitka, Facebook, 2015

-- |
-- Specialised deterministic sets, for things with @Uniques@
--
-- Based on @UniqDFMs@ (as you would expect).
-- See Note [Deterministic UniqFM] in UniqDFM for explanation why we need it.
--
-- Basically, the things need to be in class @Uniquable@.

module UniqDSet (
        -- * Unique set type
        UniqDSet,    -- type synonym for UniqFM a

        -- ** Manipulating these sets
        delOneFromUniqDSet,
        emptyUniqDSet,
        unitUniqDSet,
        mkUniqDSet,
        addOneToUniqDSet, addListToUniqDSet,
        unionUniqDSets, unionManyUniqDSets,
        minusUniqDSet,
        intersectUniqDSets,
        foldUniqDSet,
        elementOfUniqDSet,
        filterUniqDSet,
        sizeUniqDSet,
        isEmptyUniqDSet,
        lookupUniqDSet,
        uniqDSetToList,
    ) where

import UniqDFM
import Unique

type UniqDSet a = UniqDFM a

emptyUniqDSet :: UniqDSet a
emptyUniqDSet = emptyUDFM

unitUniqDSet :: Uniquable a => a -> UniqDSet a
unitUniqDSet x = unitUDFM x x

mkUniqDSet :: Uniquable a => [a]  -> UniqDSet a
mkUniqDSet = foldl addOneToUniqDSet emptyUniqDSet

addOneToUniqDSet :: Uniquable a => UniqDSet a -> a -> UniqDSet a
addOneToUniqDSet set x = addToUDFM set x x

addListToUniqDSet :: Uniquable a => UniqDSet a -> [a] -> UniqDSet a
addListToUniqDSet = foldl addOneToUniqDSet

delOneFromUniqDSet :: Uniquable a => UniqDSet a -> a -> UniqDSet a
delOneFromUniqDSet = delFromUDFM

unionUniqDSets :: UniqDSet a -> UniqDSet a -> UniqDSet a
unionUniqDSets = plusUDFM

unionManyUniqDSets :: [UniqDSet a] -> UniqDSet a
unionManyUniqDSets [] = emptyUniqDSet
unionManyUniqDSets sets = foldr1 unionUniqDSets sets

minusUniqDSet  :: UniqDSet a -> UniqDSet a -> UniqDSet a
minusUniqDSet = minusUDFM

intersectUniqDSets :: UniqDSet a -> UniqDSet a -> UniqDSet a
intersectUniqDSets = intersectUDFM

foldUniqDSet :: (a -> b -> b) -> b -> UniqDSet a -> b
foldUniqDSet = foldUDFM

elementOfUniqDSet :: Uniquable a => a -> UniqDSet a -> Bool
elementOfUniqDSet = elemUDFM

filterUniqDSet :: (a -> Bool) -> UniqDSet a -> UniqDSet a
filterUniqDSet = filterUDFM

sizeUniqDSet :: UniqDSet a -> Int
sizeUniqDSet = sizeUDFM

isEmptyUniqDSet :: UniqDSet a -> Bool
isEmptyUniqDSet = isNullUDFM

lookupUniqDSet :: Uniquable a => UniqDSet a -> a -> Maybe a
lookupUniqDSet = lookupUDFM

uniqDSetToList :: UniqDSet a -> [a]
uniqDSetToList = eltsUDFM
