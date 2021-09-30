{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module GHC.Data.Collections
    ( IsSet(..)
    , setInsertList, setDeleteList, setUnions
    , IsStaticKeylessMap(..), IsStaticMap(..), IsKeylessMap(..), IsMap(..)
    , mapInsertList, mapDeleteList, mapUnions
    , UniqueMap, UniqueSet
    , defaultMapAdjustLookup
    , defaultMapIsSubmapOfBy
    ) where

import GHC.Prelude

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import Data.Either.Both
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Maybe (isJust)
import Data.Monoid (All (..), Dual (..), Endo (..))
import Util ((∘), (∘∘), compose2)

class IsSet set where
  type ElemOf set

  setNull :: set -> Bool
  setSize :: set -> Int
  setMember :: ElemOf set -> set -> Bool

  setEmpty :: set
  setSingleton :: ElemOf set -> set
  setInsert :: ElemOf set -> set -> set
  setDelete :: ElemOf set -> set -> set

  setUnion :: set -> set -> set
  setDifference :: set -> set -> set
  setIntersection :: set -> set -> set
  setIsSubsetOf :: set -> set -> Bool
  setFilter :: (ElemOf set -> Bool) -> set -> set

  setFoldl :: (b -> ElemOf set -> b) -> b -> set -> b
  setFoldr :: (ElemOf set -> b -> b) -> b -> set -> b

  setElems :: set -> [ElemOf set]
  setFromList :: [ElemOf set] -> set

-- Helper functions for IsSet class
setInsertList :: IsSet set => [ElemOf set] -> set -> set
setInsertList keys set = foldl' (flip setInsert) set keys

setDeleteList :: IsSet set => [ElemOf set] -> set -> set
setDeleteList keys set = foldl' (flip setDelete) set keys

setUnions :: IsSet set => [set] -> set
setUnions [] = setEmpty
setUnions (set:sets) = foldl' setUnion set sets

class Traversable map => IsStaticKeylessMap map where
  {-# MINIMAL (mapAdjustLookup | mapAdjust, mapLookup), mapIsSubmapOfBy #-}

  type KeyOf map

  mapMember :: KeyOf map -> map a -> Bool
  mapMember k = isJust . mapLookup k
  mapLookup :: KeyOf map -> map a -> Maybe a
  mapLookup k = fst . mapAdjustLookup id k
  mapFindWithDefault :: a -> KeyOf map -> map a -> a
  mapFindWithDefault a k = fromMaybe a . mapLookup k

  mapAdjust :: (a -> a) -> KeyOf map -> map a -> map a
  mapAdjust f k = snd . mapAdjustLookup f k
  mapAdjustLookup :: (a -> a) -> KeyOf map -> map a -> (Maybe a, map a)
  mapAdjustLookup f = liftA2 (,) <$> mapLookup <*> mapAdjust f

  mapIsSubmapOfBy :: (a -> b -> Bool) -> map a -> map b -> Bool
  mapIsSubmapOf :: Eq a => map a -> map a -> Bool
  mapIsSubmapOf = mapIsSubmapOfBy (==)

class IsStaticKeylessMap map => IsStaticMap map where
  mapMapWithKey :: (KeyOf map -> a -> b) -> map a -> map b
  mapMapWithKey f = runIdentity . mapTraverseWithKey (Identity ∘∘ f)
  mapFoldlWithKey :: (b -> KeyOf map -> a -> b) -> b -> map a -> b
  mapFoldlWithKey f = flip $ appEndo . getDual . mapFoldMapWithKey \ k a -> (Dual . Endo) \ b -> f b k a
  mapFoldrWithKey :: (KeyOf map -> a -> b -> b) -> b -> map a -> b
  mapFoldrWithKey f = flip $ appEndo . mapFoldMapWithKey \ k -> Endo . f k
  mapFoldMapWithKey :: Monoid m => (KeyOf map -> a -> m) -> map a -> m
  mapFoldMapWithKey f = getConst . mapTraverseWithKey (Const ∘∘ f)
  mapTraverseWithKey :: Applicative p => (KeyOf map -> a -> p b) -> map a -> p (map b)

  mapKeys :: map a -> [KeyOf map]
  mapKeys = fmap fst . mapToList
  mapToList :: map a -> [(KeyOf map, a)]
  mapToList = mapFoldrWithKey (curry (:)) []

class (Filtrable map, IsStaticKeylessMap map) => IsKeylessMap map where
  {-# MINIMAL mapAlterF, mapMergeA, (mapEmpty | mapFromListWith) #-}

  mapEmpty :: map a
  mapEmpty = mapFromList []
  mapSingleton :: KeyOf map -> a -> map a
  mapSingleton k a = mapFromList [(k, a)]
  mapInsert :: KeyOf map -> a -> map a -> map a
  mapInsert = mapInsertWith pure
  mapInsertWith :: (a -> a -> a) -> KeyOf map -> a -> map a -> map a
  mapInsertWith f k a = mapAlter (Just . maybe a (f a)) k
  mapDelete :: KeyOf map -> map a -> map a
  mapDelete = mapAlter (\ _ -> Nothing)
  mapAlter :: (Maybe a -> Maybe a) -> KeyOf map -> map a -> map a
  mapAlter f k = runIdentity . mapAlterF (Identity . f) k
  mapAlterF :: Functor f => (Maybe a -> f (Maybe a)) -> KeyOf map -> map a -> f (map a)

  mapUnion :: map a -> map a -> map a
  mapUnion = mapUnionWith pure
  mapUnionWith :: (a -> a -> a) -> map a -> map a -> map a
  mapUnionWith f = mapMerge \ case
      JustLeft a -> Just a
      JustRight b -> Just b
      Both a b -> Just (f a b)
  mapDifference :: map a -> map b -> map a
  mapDifference = mapMerge \ case
      JustLeft a -> Just a
      _ -> Nothing
  mapIntersection :: map a -> map a -> map a
  mapIntersection = mapIntersectionWith pure
  mapIntersectionWith :: (a -> b -> c) -> map a -> map b -> map c
  mapIntersectionWith f = mapMerge \ case
      Both a b -> Just (f a b)
      _ -> Nothing
  mapMerge :: (Either' a b -> Maybe c) -> map a -> map b -> map c
  mapMerge f = runIdentity ∘∘ mapMergeA (Identity ∘ f)
  mapMergeA :: Applicative p => (Either' a b -> p (Maybe c)) -> map a -> map b -> p (map c)

  mapFromList :: [(KeyOf map, a)] -> map a
  mapFromList = mapFromListWith pure
  mapFromListWith :: (a -> a -> a) -> [(KeyOf map,a)] -> map a
  mapFromListWith f = foldr (uncurry $ mapInsertWith f) mapEmpty

class (IsKeylessMap map, IsStaticMap map) => IsMap map where
  mapUnionWithKey :: (KeyOf map -> a -> a -> a) -> map a -> map a -> map a
  mapUnionWithKey f = mapMergeWithKey \ k -> \ case
      JustLeft a -> Just a
      JustRight b -> Just b
      Both a b -> Just (f k a b)
  mapMergeWithKey :: (KeyOf map -> Either' a b -> Maybe c) -> map a -> map b -> map c
  mapMergeWithKey f = runIdentity ∘∘ mapMergeWithKeyA (Identity ∘∘ f)
  mapMergeWithKeyA :: Applicative p => (KeyOf map -> Either' a b -> p (Maybe c)) -> map a -> map b -> p (map c)

  mapFilterWithKey :: (KeyOf map -> a -> Bool) -> map a -> map a
  mapFilterWithKey f = mapMapMaybeWithKey \ k a -> a <$ guard (f k a)
  mapMapMaybeWithKey :: (KeyOf map -> a -> Maybe b) -> map a -> map b
  mapMapMaybeWithKey f = runIdentity . mapTraverseMaybeWithKey (Identity ∘∘ f)
  mapTraverseMaybeWithKey :: Applicative p => (KeyOf map -> a -> p (Maybe b)) -> map a -> p (map b)

-- Helper functions for IsMap class
mapInsertList :: IsKeylessMap map => [(KeyOf map, a)] -> map a -> map a
mapInsertList assocs map = foldl' (flip (uncurry mapInsert)) map assocs

mapDeleteList :: IsKeylessMap map => [KeyOf map] -> map a -> map a
mapDeleteList keys map = foldl' (flip mapDelete) map keys

mapUnions :: IsKeylessMap map => [map a] -> map a
mapUnions [] = mapEmpty
mapUnions (map:maps) = foldl' mapUnion map maps

-----------------------------------------------------------------------------
-- Basic instances
-----------------------------------------------------------------------------

newtype UniqueSet = US S.IntSet deriving (Eq, Ord, Show, Semigroup, Monoid)

instance IsSet UniqueSet where
  type ElemOf UniqueSet = Int

  setNull (US s) = S.null s
  setSize (US s) = S.size s
  setMember k (US s) = S.member k s

  setEmpty = US S.empty
  setSingleton k = US (S.singleton k)
  setInsert k (US s) = US (S.insert k s)
  setDelete k (US s) = US (S.delete k s)

  setUnion (US x) (US y) = US (S.union x y)
  setDifference (US x) (US y) = US (S.difference x y)
  setIntersection (US x) (US y) = US (S.intersection x y)
  setIsSubsetOf (US x) (US y) = S.isSubsetOf x y
  setFilter f (US s) = US (S.filter f s)

  setFoldl k z (US s) = S.foldl' k z s
  setFoldr k z (US s) = S.foldr k z s

  setElems (US s) = S.elems s
  setFromList ks = US (S.fromList ks)

newtype UniqueMap v = UM { unUM :: M.IntMap v }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Filtrable UniqueMap where
  filter f = UM . filter f . unUM
  mapMaybe f = UM . mapMaybe f . unUM

instance IsStaticKeylessMap UniqueMap where
  type KeyOf UniqueMap = Int

  mapMember k (UM m) = M.member k m
  mapFindWithDefault def k (UM m) = M.findWithDefault def k m
  mapAdjustLookup f k (UM m) = UM <$> M.updateLookupWithKey (\ _ -> Just . f) k m
  mapIsSubmapOfBy f (UM x) (UM y) = M.isSubmapOfBy f x y

instance IsStaticMap UniqueMap where
  mapMapWithKey f (UM m) = UM (M.mapWithKey f m)
  mapFoldlWithKey k z (UM m) = M.foldlWithKey' k z m
  mapFoldMapWithKey f (UM m) = M.foldMapWithKey f m
  mapTraverseWithKey f (UM m) = UM <$> M.traverseWithKey f m

  mapKeys (UM m) = M.keys m
  {-# INLINEABLE mapToList #-}
  mapToList (UM m) = M.toList m

instance IsKeylessMap UniqueMap where
  mapEmpty = UM M.empty
  mapSingleton k v = UM (M.singleton k v)
  mapInsert k v (UM m) = UM (M.insert k v m)
  mapInsertWith f k v (UM m) = UM (M.insertWith f k v m)
  mapDelete k (UM m) = UM (M.delete k m)
  mapAlter f k (UM m) = UM (M.alter f k m)
  mapAlterF f k (UM m) = UM <$> M.alterF f k m

  mapUnion (UM x) (UM y) = UM (M.union x y)
  mapDifference (UM x) (UM y) = UM (M.difference x y)
  mapIntersection (UM x) (UM y) = UM (M.intersection x y)

  mapFromList assocs = UM (M.fromList assocs)
  mapFromListWith f assocs = UM (M.fromListWith f assocs)

  mapMergeA = mapMergeWithKeyA . pure

instance IsMap UniqueMap where
  mapUnionWithKey f (UM x) (UM y) = UM (M.unionWithKey f x y)

  {-# INLINEABLE mapFilterWithKey #-}
  mapFilterWithKey f (UM m) = UM (M.filterWithKey f m)
  mapTraverseMaybeWithKey f (UM m) = UM . catMaybes <$> M.traverseWithKey f m

  mapMergeWithKeyA f (UM m) (UM n) = UM <$> mapMergeWithKeyA f m n

instance Ord k => IsStaticKeylessMap (Map k) where
    type KeyOf (Map k) = k

    mapIsSubmapOfBy = Map.isSubmapOfBy
    mapAdjustLookup f = Map.updateLookupWithKey (\ _ -> Just . f)

instance Ord k => IsStaticMap (Map k) where
    mapTraverseWithKey = Map.traverseWithKey

instance Ord k => IsKeylessMap (Map k) where
    mapAlterF = Map.alterF
    mapFromListWith = Map.fromListWith
    mapMergeA = mapMergeWithKeyA . pure

instance Ord k => IsMap (Map k) where
    mapMergeWithKeyA f = Map.mergeA (Map.traverseMaybeMissing $ \ k -> f k . JustLeft) (Map.traverseMaybeMissing $ \ k -> f k . JustRight) (Map.zipWithMaybeAMatched $ \ k -> f k ∘∘ Both)
    mapTraverseMaybeWithKey = fmap catMaybes ∘∘ Map.traverseWithKey

instance IsStaticKeylessMap IntMap where
    type KeyOf IntMap = Int

    mapIsSubmapOfBy = M.isSubmapOfBy
    mapAdjustLookup f = M.updateLookupWithKey (\ _ -> Just . f)

instance IsStaticMap IntMap where
    mapTraverseWithKey = M.traverseWithKey

instance IsKeylessMap IntMap where
    mapAlterF = M.alterF
    mapFromListWith = M.fromListWith
    mapMergeA = mapMergeWithKeyA . pure

instance IsMap IntMap where
    mapMergeWithKeyA f = mapTraverseMaybeWithKey f ∘∘ M.mergeWithKey (pure $ Just ∘∘ Both) (fmap JustLeft) (fmap JustRight)
    mapTraverseMaybeWithKey = fmap catMaybes ∘∘ M.traverseWithKey

instance IsStaticKeylessMap Maybe where
    type KeyOf Maybe = ()
    mapIsSubmapOfBy _ Nothing _ = True
    mapIsSubmapOfBy _ (Just _) Nothing = False
    mapIsSubmapOfBy f (Just a) (Just b) = f a b
    mapAdjustLookup f () = (,) <*> fmap f

instance IsStaticMap Maybe where
    mapTraverseWithKey = traverse . ($ ())

instance IsKeylessMap Maybe where
    mapEmpty = Nothing
    mapAlterF f () = f
    mapMergeA f = mapMaybeA f ∘∘ fromMaybes

instance IsMap Maybe where
    mapTraverseMaybeWithKey = mapMaybeA . ($ ())
    mapMergeWithKeyA f = mapMaybeA (f ()) ∘∘ fromMaybes

instance (IsStaticKeylessMap m, IsStaticKeylessMap n) => IsStaticKeylessMap (Compose m n) where
    type KeyOf (Compose m n) = (KeyOf m, KeyOf n)
    mapAdjustLookup f (i, j) = fmap Compose . bimap (mapLookup j =<<) id . mapAdjustLookup (mapAdjust f j) i . getCompose
    mapIsSubmapOfBy f = (compose2 . mapIsSubmapOfBy . mapIsSubmapOfBy) f getCompose getCompose

instance (IsStaticMap m, IsStaticMap n) => IsStaticMap (Compose m n) where
    mapTraverseWithKey f = fmap Compose ∘ mapTraverseWithKey (\ i -> mapTraverseWithKey (\ j -> f (i, j))) ∘ getCompose

instance (IsKeylessMap m, IsKeylessMap n) => IsKeylessMap (Compose m n) where
    mapEmpty = Compose mapEmpty
    mapAlterF f (i, j) = fmap Compose . mapAlterF (maybe (fmap (mapSingleton j) <$> f Nothing) (fmap Just . mapAlterF f j)) i . getCompose
    mapMergeA f =
        fmap Compose ∘∘
        compose2 (mapMergeA $
                  fmap Just ∘ either' (mapMaybeA $ f ∘ JustLeft)
                                      (mapMaybeA $ f ∘ JustRight)
                                      (mapMergeA f)) getCompose getCompose

instance (IsMap m, IsMap n) => IsMap (Compose m n) where
    mapMergeWithKeyA f =
        fmap Compose ∘∘
        compose2 (mapMergeWithKeyA $ \ i ->
                  fmap Just ∘ either' (mapTraverseMaybeWithKey $ \ j -> f (i, j) ∘ JustLeft)
                                      (mapTraverseMaybeWithKey $ \ j -> f (i, j) ∘ JustRight)
                                      (mapMergeWithKeyA $ \ j -> f (i, j))) getCompose getCompose
    mapTraverseMaybeWithKey f = fmap Compose . mapTraverseMaybeWithKey (\ i -> fmap Just . mapTraverseMaybeWithKey (\ j -> f (i, j))) . getCompose

instance (IsStaticKeylessMap m, IsStaticKeylessMap n) => IsStaticKeylessMap (Product m n) where
    type KeyOf (Product m n) = Either (KeyOf m) (KeyOf n)
    mapAdjustLookup f k (Pair a b) = case k of
        Left  i -> flip Pair b <$> mapAdjustLookup f i a
        Right j -> id   Pair a <$> mapAdjustLookup f j b
    mapIsSubmapOfBy f (Pair a1 b1) (Pair a2 b2) = mapIsSubmapOfBy f a1 a2 && mapIsSubmapOfBy f b1 b2

instance (IsStaticMap m, IsStaticMap n) => IsStaticMap (Product m n) where
    mapTraverseWithKey f (Pair a b) =
        Pair <$> mapTraverseWithKey (f ∘ Left) a <*> mapTraverseWithKey (f ∘ Right) b

instance (IsKeylessMap m, IsKeylessMap n) => IsKeylessMap (Product m n) where
    mapEmpty = Pair mapEmpty mapEmpty
    mapAlterF f k (Pair a b) = case k of
        Left  i -> flip Pair b <$> mapAlterF f i a
        Right j -> id   Pair a <$> mapAlterF f j b
    mapMergeA f (Pair a₀ b₀) (Pair a₁ b₁) = Pair <$> mapMergeA f a₀ a₁ <*> mapMergeA f b₀ b₁

instance (IsMap m, IsMap n) => IsMap (Product m n) where
    mapMergeWithKeyA f (Pair a₀ b₀) (Pair a₁ b₁) = Pair <$> mapMergeWithKeyA (f . Left) a₀ a₁ <*> mapMergeWithKeyA (f . Right) b₀ b₁
    mapTraverseMaybeWithKey f (Pair a b) = Pair <$> mapTraverseMaybeWithKey (f . Left) a <*> mapTraverseMaybeWithKey (f . Right) b

defaultMapAdjustLookup :: IsKeylessMap map => (a -> a) -> KeyOf map -> map a -> (Maybe a, map a)
defaultMapAdjustLookup f = mapAlterF ((,) <*> fmap f)

defaultMapIsSubmapOfBy :: IsKeylessMap map => (a -> b -> Bool) -> map a -> map b -> Bool
defaultMapIsSubmapOfBy f = getAll ∘∘ getConst ∘∘ (mapMergeA . fmap (Const . All)) \ case
    JustLeft _ -> True
    JustRight _ -> False
    Both a b -> f a b

#if !MIN_VERSION_containers(0,6,5)
instance Filtrable IntMap where
    mapMaybe = M.mapMaybe

instance Ord k => Filtrable (Map k) where
    mapMaybe = Map.mapMaybe
#endif
