{-
(c) Bartosz Nitka, Facebook, 2015

UniqDFM: Specialised deterministic finite maps, for things with @Uniques@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

This is very similar to @UniqFM@, the major difference being that the order of
folding is not dependent on @Unique@ ordering, giving determinism.
Currently the ordering is determined by insertion order.

See Note [Unique Determinism] in GHC.Types.Unique for explanation why @Unique@ ordering
is not deterministic.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module GHC.Types.Unique.DFM (
        -- * Unique-keyed deterministic mappings
        UniqDFM,       -- abstract type

        -- ** Manipulating those mappings
        emptyUDFM,
        unitUDFM,
        addToUDFM,
        addToUDFM_C,
        addToUDFM_C_Directly,
        addToUDFM_Directly,
        addListToUDFM,
        delFromUDFM,
        delListFromUDFM,
        adjustUDFM,
        adjustUDFM_Directly,
        alterUDFM,
        alterFUDFM,
        plusUDFM,
        plusUDFM_C,
        lookupUDFM, lookupUDFM_Directly,
        elemUDFM,
        filterUDFM_Directly,
        intersectUDFM, udfmIntersectUFM,
        disjointUDFM, disjointUdfmUfm,
        equalKeysUDFM,
        minusUDFM,
        listToUDFM, listToUDFM_Directly,
        udfmMinusUFM, ufmMinusUDFM,
        pprUniqDFM, pprUDFM,

        udfmToList,
        udfmToUfm,
        nonDetStrictFoldUDFM,
        unsafeCastUDFMKey,
        alwaysUnsafeUfmToUdfm,
    ) where

import GHC.Prelude

import GHC.Data.Collections
import GHC.Types.Unique ( Uniquable(..), Unique, getKey )
import GHC.Utils.Outputable

import qualified Data.IntMap as M
import Data.Data
import Data.Functor.Classes (Eq1 (..))
import Data.List (sortBy)
import Data.Foldable (toList)
import GHC.Types.Unique.FM (UniqFM, nonDetUFMToList, ufmToIntMap, unsafeIntMapToUFM)
import Unsafe.Coerce
import Data.Bifunctor (bimap)
import Util (compose2)

-- Note [Deterministic UniqFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A @UniqDFM@ is just like @UniqFM@ with the following additional
-- property: the function `udfmToList` returns the elements in some
-- deterministic order not depending on the Unique key for those elements.
--
-- If the client of the map performs operations on the map in deterministic
-- order then `udfmToList` returns them in deterministic order.
--
-- There is an implementation cost: each element is given a serial number as it is added, and
-- `udfmToList` sorts its result by this serial number. So you should only use `UniqDFM` if you
-- need the deterministic property.
--
-- `foldUDFM` also preserves determinism.
--
-- Normal @UniqFM@ when you turn it into a list will use Data.IntMap.toList function that
-- returns the elements in the order of the keys. The keys in @UniqFM@ are always @Uniques@,
-- so you end up with with a list ordered by @Uniques@.
-- The order of @Uniques@ is known to be not stable across rebuilds.
-- See Note [Unique Determinism] in GHC.Types.Unique.
--
--
-- There's more than one way to implement this. The implementation here tags every value with
-- the insertion time that can later be used to sort the values when asked to convert to a list.
--
-- An alternative would be to have
--
--   data UniqDFM a = UDFM (M.IntMap a) [a]
--
-- where the list determines the order. This makes deletion tricky as we'd only accumulate
-- elements in that list, but makes merging easier as you can just merge both structures
-- independently. Deletion can probably be done in amortized fashion when the size of the
-- list is twice the size of the set.

-- | A type of values tagged with insertion time
data TaggedVal val =
  TaggedVal
    val
    {-# UNPACK #-} !Int -- ^ insertion time
  deriving (Data, Foldable, Functor, Traversable)

taggedFst :: TaggedVal val -> val
taggedFst (TaggedVal v _) = v

{-
taggedFstL :: Lens (TaggedVal u) (TaggedVal v) u v
taggedFstL f (TaggedVal v k) = flip TaggedVal k <$> f v
-}

taggedSnd :: TaggedVal val -> Int
taggedSnd (TaggedVal _ i) = i

instance Eq val => Eq (TaggedVal val) where
  (TaggedVal v1 _) == (TaggedVal v2 _) = v1 == v2

-- | Type of unique deterministic finite maps
--
-- The key is just here to keep us honest. It's always safe to use a single type as key.
-- If two types don't overlap in their uniques it's also safe to index the same map at multiple
-- key types, but this is very much discouraged.
data UniqDFM k a = UDFM
    !(M.IntMap (TaggedVal a))
    -- A map where keys are Unique's values and values are tagged with insertion time
    -- The invariant is that all the tags will be distinct within a single map.
    {-# UNPACK #-} !Int
    -- Upper bound on the values' insertion time. See Note [Overflow on plusUDFM]
  deriving (Data, Functor)

instance Foldable (UniqDFM k) where
  foldr k z = foldr k z . toList
  foldMap f = foldMap f . toList
  toList (UDFM m _i) = taggedFst <$> sortBy (compare `on` taggedSnd) (toList m)

instance Traversable (UniqDFM k) where
  traverse f = fmap listToUDFM_Directly . (traverse . traverse) f . udfmToList

instance Filtrable (UniqDFM k) where
  filter p (UDFM m i) = UDFM (filter (\(TaggedVal v _) -> p v) m) i

  mapMaybe f (UDFM m i) = UDFM (mapMaybe (\(TaggedVal v k) -> flip TaggedVal k <$> f v) m) i

  mapEither f (UDFM m i) = bimap (flip UDFM i) (flip UDFM i) (mapEither (\(TaggedVal v k) -> bimap (flip TaggedVal k) (flip TaggedVal k) (f v)) m)

{-
Note [foldTM determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~
We want foldTM to be deterministic, which is why we have an instance of
TrieMap for UniqDFM, but not for UniqFM. Here's an example of some things that
go wrong if foldTM is nondeterministic. Consider:

  f a b = return (a <> b)

Depending on the order that the typechecker generates constraints you get either:

  f :: (Monad m, Monoid a) => a -> a -> m a

or:

  f :: (Monoid a, Monad m) => a -> a -> m a

The generated code will be different after desugaring as the dictionaries
will be bound in different orders, leading to potential ABI incompatibility.

One way to solve this would be to notice that the typeclasses could be sorted alphabetically.

Unfortunately that doesn't quite work with this example:

  f a b = let x = a <> a; y = b <> b in x

where you infer:

  f :: (Monoid m, Monoid m1) => m1 -> m -> m1

or:

  f :: (Monoid m1, Monoid m) => m1 -> m -> m1

Here you could decide to take the order of the type variables in the type
according to depth first traversal and use it to order the constraints.

The real trouble starts when the user enables incoherent instances and
the compiler has to make an arbitrary choice. Consider:

  class T a b where
    go :: a -> b -> String

  instance (Show b) => T Int b where
    go a b = show a ++ show b

  instance (Show a) => T a Bool where
    go a b = show a ++ show b

  f = go 10 True

GHC is free to choose either dictionary to implement f, but for the sake of determinism we'd
like it to be consistent when compiling the same sources with the same flags.

inert_dicts :: DictMap is implemented with a TrieMap. In getUnsolvedInerts it gets converted to
a bag of (Wanted) Cts using a fold. Then in solve_simple_wanteds it's merged with other
WantedConstraints. We want the conversion to a bag to be deterministic. For that purpose we use
UniqDFM instead of UniqFM to implement the TrieMap.

See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for more details on how it's made
deterministic.
-}

instance forall k. Uniquable k => IsStaticKeylessMap (UniqDFM k) where
  type KeyOf (UniqDFM k) = k
  mapAdjustLookup f k (UDFM m i) = (taggedFst <$> M.lookup k' m, UDFM (M.adjust (fmap f) k' m) i)
    where k' = getKey $ getUnique k
  mapIsSubmapOfBy f (UDFM m _) (UDFM n _) = M.isSubmapOfBy (compose2 f taggedFst taggedFst) m n

instance forall k. Uniquable k => IsKeylessMap (UniqDFM k) where
  mapEmpty = emptyUDFM
  mapAlterF = flip . alterFUDFM
  mapMergeA f (UDFM m _) (UDFM n _) = fmap listToUDFM_Directly $
      mapMaybeA (\ (key, value) -> fmap ((,) (getUnique key)) <$> f value) $
      fmap (bimap taggedFst taggedFst) <$>
      sortBy (compare `on` bimap taggedSnd taggedSnd . snd)
      (M.toList $ mapMerge Just m n)

emptyUDFM :: UniqDFM k a
emptyUDFM = UDFM M.empty 0

unitUDFM :: Uniquable k => k -> a -> UniqDFM k a
unitUDFM k v = UDFM (M.singleton (getKey $ getUnique k) (TaggedVal v 0)) 1

-- The new binding always goes to the right of existing ones
addToUDFM :: Uniquable k => UniqDFM k a -> k -> a  -> UniqDFM k a
addToUDFM m k = addToUDFM_Directly m (getUnique k)

-- The new binding always goes to the right of existing ones
addToUDFM_Directly :: UniqDFM k a -> Unique -> a -> UniqDFM k a
addToUDFM_Directly (UDFM m i) u v
  = UDFM (M.insertWith tf (getKey u) (TaggedVal v i) m) (i + 1)
  where
    tf (TaggedVal new_v _) (TaggedVal _ old_i) = TaggedVal new_v old_i
      -- Keep the old tag, but insert the new value
      -- This means that udfmToList typically returns elements
      -- in the order of insertion, rather than the reverse

addToUDFM_C_Directly
  :: (a -> a -> a)   -- old -> new -> result
  -> UniqDFM k a
  -> Unique -> a
  -> UniqDFM k a
addToUDFM_C_Directly f (UDFM m i) u v
  = UDFM (M.insertWith tf (getKey u) (TaggedVal v i) m) (i + 1)
    where
      tf (TaggedVal new_v _) (TaggedVal old_v old_i) = TaggedVal (f old_v new_v) old_i
          -- Flip the arguments, because M.insertWith uses  (new->old->result)
          --                         but f            needs (old->new->result)
          -- Like addToUDFM_Directly, keep the old tag

addToUDFM_C
  :: Uniquable k => (a -> a -> a) -- old -> new -> result
  -> UniqDFM k a -- old
  -> k -> a -- new
  -> UniqDFM k a -- result
addToUDFM_C f m k = addToUDFM_C_Directly f m (getUnique k)

addListToUDFM :: Uniquable k => UniqDFM k a -> [(k, a)] -> UniqDFM k a
addListToUDFM = foldl' (uncurry . addToUDFM)

addListToUDFM_Directly :: UniqDFM k a -> [(Unique,a)] -> UniqDFM k a
addListToUDFM_Directly = foldl' (uncurry . addToUDFM_Directly)

addListToUDFM_Directly_C :: (a -> a -> a) -> UniqDFM k a -> [(Unique,a)] -> UniqDFM k a
addListToUDFM_Directly_C f = foldl' (uncurry . addToUDFM_C_Directly f)

delFromUDFM :: Uniquable k => UniqDFM k a -> k -> UniqDFM k a
delFromUDFM (UDFM m i) k = UDFM (M.delete (getKey $ getUnique k) m) i

plusUDFM_C :: (a -> a -> a) -> UniqDFM k a -> UniqDFM k a -> UniqDFM k a
plusUDFM_C f udfml@(UDFM _ i) udfmr@(UDFM _ j)
  -- we will use the upper bound on the tag as a proxy for the set size,
  -- to insert the smaller one into the bigger one
  | i > j = insertUDFMIntoLeft_C f udfml udfmr
  | otherwise = insertUDFMIntoLeft_C f udfmr udfml

-- Note [Overflow on plusUDFM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- There are multiple ways of implementing plusUDFM.
-- The main problem that needs to be solved is overlap on times of
-- insertion between different keys in two maps.
-- Consider:
--
-- A = fromList [(a, (x, 1))]
-- B = fromList [(b, (y, 1))]
--
-- If you merge them naively you end up with:
--
-- C = fromList [(a, (x, 1)), (b, (y, 1))]
--
-- Which loses information about ordering and brings us back into
-- non-deterministic world.
--
-- The solution I considered before would increment the tags on one of the
-- sets by the upper bound of the other set. The problem with this approach
-- is that you'll run out of tags for some merge patterns.
-- Say you start with A with upper bound 1, you merge A with A to get A' and
-- the upper bound becomes 2. You merge A' with A' and the upper bound
-- doubles again. After 64 merges you overflow.
-- This solution would have the same time complexity as plusUFM, namely O(n+m).
--
-- The solution I ended up with has time complexity of
-- O(m log m + m * min (n+m, W)) where m is the smaller set.
-- It simply inserts the elements of the smaller set into the larger set in the order that they
-- were inserted into the smaller set. That's O(m log m) for extracting the elements from the
-- smaller set in the insertion order and O(m * min(n+m, W)) to insert them into the bigger set.

plusUDFM :: UniqDFM k a -> UniqDFM k a -> UniqDFM k a
plusUDFM udfml@(UDFM _ i) udfmr@(UDFM _ j)
  -- we will use the upper bound on the tag as a proxy for the set size,
  -- to insert the smaller one into the bigger one
  | i > j = insertUDFMIntoLeft udfml udfmr
  | otherwise = insertUDFMIntoLeft udfmr udfml

insertUDFMIntoLeft :: UniqDFM k a -> UniqDFM k a -> UniqDFM k a
insertUDFMIntoLeft udfml udfmr = addListToUDFM_Directly udfml $ udfmToList udfmr

insertUDFMIntoLeft_C :: (a -> a -> a) -> UniqDFM k a -> UniqDFM k a -> UniqDFM k a
insertUDFMIntoLeft_C f udfml udfmr =
  addListToUDFM_Directly_C f udfml $ udfmToList udfmr

lookupUDFM :: Uniquable k => UniqDFM k a -> k -> Maybe a
lookupUDFM (UDFM m _i) k = taggedFst <$> M.lookup (getKey $ getUnique k) m

lookupUDFM_Directly :: UniqDFM k a -> Unique -> Maybe a
lookupUDFM_Directly (UDFM m _i) k = taggedFst <$> M.lookup (getKey k) m

elemUDFM :: Uniquable k => k -> UniqDFM k a -> Bool
elemUDFM k (UDFM m _i) = M.member (getKey $ getUnique k) m

-- | Performs a nondeterministic strict fold over the UniqDFM.
-- It's O(n), same as the corresponding function on `UniqFM`.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldUDFM :: (a -> b -> b) -> b -> UniqDFM k a -> b
nonDetStrictFoldUDFM k z (UDFM m _i) = foldl' k' z m
  where
    k' acc (TaggedVal v _) = k v acc

filterUDFM_Directly :: (Unique -> a -> Bool) -> UniqDFM k a -> UniqDFM k a
filterUDFM_Directly p (UDFM m i) = UDFM (M.filterWithKey p' m) i
  where
  p' k (TaggedVal v _) = p (getUnique k) v

{-
mapMaybeAUDFM_Directly :: Applicative p => (Unique -> a -> p (Maybe b)) -> UniqDFM k a -> p (UniqDFM k b)
mapMaybeAUDFM_Directly f (UDFM m i) = flip UDFM i <$> (MapC.mapMaybeWithKeyA f' m)
  where
    f' k (TaggedVal v i) = fmap (flip TaggedVal i) <$> f (getUnique k) v
-}

-- | Converts `UniqDFM` to a list, with elements in deterministic order.
-- It's O(n log n) while the corresponding function on `UniqFM` is O(n).
udfmToList :: UniqDFM k a -> [(Unique, a)]
udfmToList (UDFM m _i) =
  [ (getUnique k, taggedFst v)
  | (k, v) <- sortBy (compare `on` (taggedSnd . snd)) $ M.toList m ]

-- Determines whether two 'UniqDFM's contain the same keys.
equalKeysUDFM :: UniqDFM k a -> UniqDFM k b -> Bool
equalKeysUDFM (UDFM m _) (UDFM n _) = liftEq (\_ _ -> True) m n

intersectUDFM :: UniqDFM k a -> UniqDFM k a -> UniqDFM k a
intersectUDFM (UDFM x i) (UDFM y _j) = UDFM (M.intersection x y) i
  -- M.intersection is left biased, that means the result will only have
  -- a subset of elements from the left set, so `i` is a good upper bound.

udfmIntersectUFM :: UniqDFM k a -> UniqFM k b -> UniqDFM k a
udfmIntersectUFM (UDFM x i) y = UDFM (M.intersection x (ufmToIntMap y)) i
  -- M.intersection is left biased, that means the result will only have
  -- a subset of elements from the left set, so `i` is a good upper bound.

disjointUDFM :: UniqDFM k a -> UniqDFM k a -> Bool
disjointUDFM (UDFM x _i) (UDFM y _j) = M.disjoint x y

disjointUdfmUfm :: UniqDFM k a -> UniqFM k b -> Bool
disjointUdfmUfm (UDFM x _i) y = M.disjoint x (ufmToIntMap y)

minusUDFM :: UniqDFM k a -> UniqDFM k b -> UniqDFM k a
minusUDFM (UDFM x i) (UDFM y _j) = UDFM (M.difference x y) i
  -- M.difference returns a subset of a left set, so `i` is a good upper bound.

udfmMinusUFM :: UniqDFM k a -> UniqFM k b -> UniqDFM k a
udfmMinusUFM (UDFM x i) y = UDFM (M.difference x (ufmToIntMap y)) i
  -- M.difference returns a subset of a left set, so `i` is a good upper bound.

ufmMinusUDFM :: UniqFM k a -> UniqDFM k b -> UniqFM k a
ufmMinusUDFM x (UDFM y _i) = unsafeIntMapToUFM (M.difference (ufmToIntMap x) y)

-- | Delete a list of elements from a UniqDFM
delListFromUDFM  :: Uniquable k => UniqDFM k a -> [k] -> UniqDFM k a
delListFromUDFM = foldl' delFromUDFM

-- | This allows for lossy conversion from UniqDFM to UniqFM
udfmToUfm :: UniqDFM k a -> UniqFM k a
udfmToUfm (UDFM m _i) = unsafeIntMapToUFM (M.map taggedFst m)

listToUDFM :: Uniquable k => [(k, a)] -> UniqDFM k a
listToUDFM = foldl' (\m (k, v) -> addToUDFM m k v) emptyUDFM

listToUDFM_Directly :: [(Unique, a)] -> UniqDFM k a
listToUDFM_Directly = foldl' (\m (u, v) -> addToUDFM_Directly m u v) emptyUDFM

-- | Apply a function to a particular element
adjustUDFM :: Uniquable k => (a -> a) -> UniqDFM k a -> k -> UniqDFM k a
adjustUDFM f (UDFM m i) k = UDFM (M.adjust (fmap f) (getKey $ getUnique k) m) i

-- | Apply a function to a particular element
adjustUDFM_Directly :: (a -> a) -> UniqDFM k a -> Unique -> UniqDFM k a
adjustUDFM_Directly f (UDFM m i) k = UDFM (M.adjust (fmap f) (getKey k) m) i

-- | The expression (alterUDFM f k map) alters value x at k, or absence thereof. alterUDFM can
-- be used to insert, delete, or update a value in UniqDFM. Use addToUDFM, delFromUDFM or
-- adjustUDFM when possible; they are more efficient.
alterUDFM
  :: Uniquable k
  => (Maybe a -> Maybe a)  -- How to adjust
  -> UniqDFM k a               -- old
  -> k                         -- new
  -> UniqDFM k a               -- result
alterUDFM f (UDFM m i) k =
  UDFM (M.alter alterf (getKey $ getUnique k) m) (i + 1)
  where
  alterf Nothing = inject $ f Nothing
  alterf (Just (TaggedVal v _)) = inject $ f (Just v)
  inject Nothing = Nothing
  inject (Just v) = Just $ TaggedVal v i

alterFUDFM
  :: (Uniquable k, Functor f)
  => (Maybe a -> f (Maybe a))  -- How to adjust
  -> UniqDFM k a               -- old
  -> k                         -- new
  -> f (UniqDFM k a)               -- result
alterFUDFM f (UDFM m i) k =
  flip UDFM (i + 1) <$> M.alterF alterf (getKey $ getUnique k) m
  where
  alterf = (fmap . fmap) (flip TaggedVal i) . f . fmap taggedFst

-- This should not be used in committed code, provided for convenience to
-- make ad-hoc conversions when developing
alwaysUnsafeUfmToUdfm :: UniqFM k a -> UniqDFM k a
alwaysUnsafeUfmToUdfm = listToUDFM_Directly . nonDetUFMToList

-- | Cast the key domain of a UniqFM.
--
-- As long as the domains don't overlap in their uniques this is safe.
unsafeCastUDFMKey :: UniqDFM i a -> UniqDFM j a
unsafeCastUDFMKey = unsafeCoerce -- Only phantom parameter changes so
                                 -- this is safe and avoids reallocation.

-- Output-ery

instance Outputable a => Outputable (UniqDFM k a) where
    ppr = pprUniqDFM ppr

pprUniqDFM :: (a -> SDoc) -> UniqDFM k a -> SDoc
pprUniqDFM ppr_elt ufm
  = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> text ":->" <+> ppr_elt a
    | (uq, a) <- udfmToList ufm ]

pprUDFM :: UniqDFM k a    -- ^ The things to be pretty printed
       -> ([a] -> SDoc) -- ^ The pretty printing function to use on the elements
       -> SDoc          -- ^ 'SDoc' where the things have been pretty printed
pprUDFM ufm pp = pp (toList ufm)
