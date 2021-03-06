{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Cmm.Dataflow.Label
    ( Label
    , LabelMap
    , LabelSet
    , FactBase
    , lookupFact
    , mkHooplLabel
    ) where

import GHC.Prelude

import GHC.Utils.Outputable

-- TODO: This should really just use GHC's Unique and Uniq{Set,FM}
import GHC.Data.Collections

import GHC.Types.Unique (Uniquable(..))

-----------------------------------------------------------------------------
--              Label
-----------------------------------------------------------------------------

newtype Label = Label { lblToUnique :: Int }
  deriving (Eq, Ord)

mkHooplLabel :: Int -> Label
mkHooplLabel = Label

instance Show Label where
  show (Label n) = "L" ++ show n

instance Uniquable Label where
  getUnique label = getUnique (lblToUnique label)

instance Outputable Label where
  ppr label = ppr (getUnique label)

instance OutputableP env Label where
  pdoc _ l = ppr l

-----------------------------------------------------------------------------
-- LabelSet

newtype LabelSet = LS UniqueSet deriving (Eq, Ord, Show, Monoid, Semigroup)

instance IsSet LabelSet where
  type ElemOf LabelSet = Label

  setNull (LS s) = setNull s
  setSize (LS s) = setSize s
  setMember (Label k) (LS s) = setMember k s

  setEmpty = LS setEmpty
  setSingleton (Label k) = LS (setSingleton k)
  setInsert (Label k) (LS s) = LS (setInsert k s)
  setDelete (Label k) (LS s) = LS (setDelete k s)

  setUnion (LS x) (LS y) = LS (setUnion x y)
  setDifference (LS x) (LS y) = LS (setDifference x y)
  setIntersection (LS x) (LS y) = LS (setIntersection x y)
  setIsSubsetOf (LS x) (LS y) = setIsSubsetOf x y
  setFilter f (LS s) = LS (setFilter (f . mkHooplLabel) s)
  setFoldl k z (LS s) = setFoldl (\a v -> k a (mkHooplLabel v)) z s
  setFoldr k z (LS s) = setFoldr (\v a -> k (mkHooplLabel v) a) z s

  setElems (LS s) = map mkHooplLabel (setElems s)
  setFromList ks = LS (setFromList (map lblToUnique ks))

-----------------------------------------------------------------------------
-- LabelMap

newtype LabelMap v = LM { unLM :: UniqueMap v }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Filtrable LabelMap where
  mapMaybe f = LM . mapMaybe f . unLM

instance IsStaticKeylessMap LabelMap where
  type KeyOf LabelMap = Label

  mapMember (Label k) (LM m) = mapMember k m
  mapLookup (Label k) (LM m) = mapLookup k m
  mapFindWithDefault def (Label k) (LM m) = mapFindWithDefault def k m

  mapAdjust f (Label k) (LM m) = LM (mapAdjust f k m)
  mapAdjustLookup f (Label k) (LM m) = LM <$> mapAdjustLookup f k m

  mapIsSubmapOfBy f (LM x) (LM y) = mapIsSubmapOfBy f x y

instance IsStaticMap LabelMap where
  mapMapWithKey f (LM m) = LM (mapMapWithKey (f . mkHooplLabel) m)
  mapFoldlWithKey k z (LM m) =
      mapFoldlWithKey (\a -> k a . mkHooplLabel) z m
  mapFoldMapWithKey f (LM m) = mapFoldMapWithKey (f . mkHooplLabel) m
  mapTraverseWithKey f (LM m) = LM <$> mapTraverseWithKey (f . mkHooplLabel) m

  mapKeys (LM m) = map mkHooplLabel (mapKeys m)
  {-# INLINEABLE mapToList #-}
  mapToList (LM m) = [(mkHooplLabel k, v) | (k, v) <- mapToList m]

instance IsKeylessMap LabelMap where
  mapEmpty = LM mapEmpty
  mapSingleton (Label k) v = LM (mapSingleton k v)
  mapInsert (Label k) v (LM m) = LM (mapInsert k v m)
  mapInsertWith f (Label k) v (LM m) = LM (mapInsertWith f k v m)
  mapDelete (Label k) (LM m) = LM (mapDelete k m)
  mapAlter f (Label k) (LM m) = LM (mapAlter f k m)
  mapAlterF f (Label k) (LM m) = LM <$> mapAlterF f k m

  mapUnion (LM x) (LM y) = LM (mapUnion x y)
  mapDifference (LM x) (LM y) = LM (mapDifference x y)
  mapIntersection (LM x) (LM y) = LM (mapIntersection x y)

  mapFromList assocs = LM (mapFromList [(lblToUnique k, v) | (k, v) <- assocs])
  mapFromListWith f assocs = LM (mapFromListWith f [(lblToUnique k, v) | (k, v) <- assocs])

  mapMergeA f (LM m) (LM n) = LM <$> mapMergeA f m n

instance IsMap LabelMap where
  mapUnionWithKey f (LM x) (LM y) = LM (mapUnionWithKey (f . mkHooplLabel) x y)
  mapTraverseMaybeWithKey f (LM x) = LM <$> mapTraverseMaybeWithKey (f . mkHooplLabel) x

  {-# INLINEABLE mapFilterWithKey #-}
  mapFilterWithKey f (LM m) = LM (mapFilterWithKey (f . mkHooplLabel) m)

  mapMergeWithKeyA f (LM m) (LM n) = LM <$> mapMergeWithKeyA (f . mkHooplLabel) m n

-----------------------------------------------------------------------------
-- Instances

instance Outputable LabelSet where
  ppr = ppr . setElems

instance Outputable a => Outputable (LabelMap a) where
  ppr = ppr . mapToList

instance OutputableP env a => OutputableP env (LabelMap a) where
  pdoc env = pdoc env . mapToList

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = mapLookup
