{-# LANGUAGE CPP                  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

#include "lens.h"

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Data.TrieMap(
   -- * Maps over 'Maybe' values
   MaybeMap,
   -- * Maps over 'List' values
   ListMap,
   -- * Maps over 'Literal's
   LiteralMap,
   -- * Things helpful for adding additional Instances.
   XT, XTF, xtf,
   -- * Map for leaf compression
   GenMap,
   lkG, xtGF,
   lkList, xtListF,
 ) where

import GHC.Prelude

import GHC.Data.Collections
import GHC.Types.Literal
import GHC.Utils.Misc
import GHC.Utils.Outputable

import qualified Data.Map    as Map
import Control.Category ( (>>>) )
import Control.Monad( (>=>) )
import Data.Either.Both
import Data.Foldable( toList )

{-
This module implements TrieMaps, which are finite mappings
whose key is a structured value like a CoreExpr or Type.

This file implements tries over general data structures.
Implementation for tries over Core Expressions/Types are
available in GHC.Core.Map.

The regular pattern for handling TrieMaps on data structures was first
described (to my knowledge) in Connelly and Morris's 1995 paper "A
generalization of the Trie Data Structure"; there is also an accessible
description of the idea in Okasaki's book "Purely Functional Data
Structures", Section 10.3.2
-}

type XT a = Maybe a -> Maybe a  -- How to alter a non-existent elt (Nothing)
                                --               or an existing elt (Just)

type XTF f a = Maybe a -> f (Maybe a)

xtf :: (IsKeylessMap m, Functor f) => (m a -> f (m a)) -> XTF f (m a)
xtf f = fmap Just . f . fromMaybe mapEmpty

{-
************************************************************************
*                                                                      *
                   Maybes
*                                                                      *
************************************************************************

If              m is a map from k -> val
then (MaybeMap m) is a map from (Maybe k) -> val
-}

data MaybeMap m a = MM { mm_nothing  :: Maybe a, mm_just :: m a }
   deriving (Foldable, Functor, Traversable)

LENS_FIELD(mm_nothingL, mm_nothing)
LENS_FIELD(mm_justL, mm_just)

instance Filtrable m => Filtrable (MaybeMap m) where
    mapMaybe f (MM xs ys) = MM (mapMaybe f xs) (mapMaybe f ys)

instance IsStaticKeylessMap m => IsStaticKeylessMap (MaybeMap m) where
    type KeyOf (MaybeMap m) = Maybe (KeyOf m)
    mapLookup = \ case
        Nothing -> mm_nothing
        Just x -> mapLookup x . mm_just
    mapAdjust = flip \ case
        Nothing -> over mm_nothingL . fmap
        Just x -> over mm_justL . flip mapAdjust x
    mapIsSubmapOfBy f (MM xs1 ys1) (MM xs2 ys2) = mapIsSubmapOfBy f xs1 xs2 && mapIsSubmapOfBy f ys1 ys2

instance IsStaticMap m => IsStaticMap (MaybeMap m) where
    mapTraverseWithKey f (MM xs ys) = MM <$> traverse (f Nothing) xs <*> mapTraverseWithKey (f . Just) ys

instance IsKeylessMap m => IsKeylessMap (MaybeMap m) where
    mapEmpty = MM { mm_nothing = Nothing, mm_just = mapEmpty }
    mapAlter = flip \ case
        Nothing -> over mm_nothingL
        Just x -> over mm_justL . flip mapAlter x
    mapAlterF = flip \ case
        Nothing -> mm_nothingL
        Just x -> mm_justL . flip mapAlterF x
    mapMergeA f (MM xs1 ys1) (MM xs2 ys2) = MM <$> mapMaybeA f (fromMaybes xs1 xs2) <*> mapMergeA f ys1 ys2

instance IsMap m => IsMap (MaybeMap m) where
    mapTraverseMaybeWithKey f (MM xs ys) = MM <$> mapMaybeA (f Nothing) xs <*> mapTraverseMaybeWithKey (f . Just) ys
    mapMergeWithKeyA f (MM xs1 ys1) (MM xs2 ys2) = MM <$> mapMaybeA (f Nothing) (fromMaybes xs1 xs2) <*> mapMergeWithKeyA (f . Just) ys1 ys2

{-
************************************************************************
*                                                                      *
                   Lists
*                                                                      *
************************************************************************
-}

data ListMap m a
  = LM { lm_nil  :: Maybe a
       , lm_cons :: m (ListMap m a) }
  deriving (Foldable, Functor, Traversable)

LENS_FIELD(lm_nilL, lm_nil)
LENS_FIELD(lm_consL, lm_cons)

instance Filtrable m => Filtrable (ListMap m) where
    mapMaybe f (LM xs ys) = LM (mapMaybe f xs) (mapMaybe f <$> ys)

instance IsStaticKeylessMap m => IsStaticKeylessMap (ListMap m) where
    type KeyOf (ListMap m) = [KeyOf m]
    mapAdjust = flip \ case
        [] -> over lm_nilL . fmap
        k:ks -> over lm_consL . flip mapAdjust k . flip mapAdjust ks
    mapLookup = lkList mapLookup
    mapIsSubmapOfBy f (LM xs1 ys1) (LM xs2 ys2) = mapIsSubmapOfBy f xs1 xs2 && mapIsSubmapOfBy (mapIsSubmapOfBy f) ys1 ys2

instance IsStaticMap m => IsStaticMap (ListMap m) where
     mapTraverseWithKey f (LM xs ys) = LM <$> traverse (f []) xs <*> mapTraverseWithKey (\ k -> mapTraverseWithKey (f . (k :))) ys

instance IsKeylessMap m => IsKeylessMap (ListMap m) where
    mapEmpty = LM { lm_nil = Nothing, lm_cons = mapEmpty }
    mapAlterF = xtListF mapAlterF
    mapMergeA f (LM xs1 ys1) (LM xs2 ys2) = LM
        <$> mapMaybeA f (fromMaybes xs1 xs2)
        <*> mapMergeA (let f' = mapMergeA f in fmap Just . either' (`f'` mapEmpty) (mapEmpty `f'`) f') ys1 ys2

instance IsMap m => IsMap (ListMap m) where
    mapTraverseMaybeWithKey f (LM xs ys) = LM <$> mapMaybeA (f []) xs <*> mapTraverseWithKey (\ k -> mapTraverseMaybeWithKey (f . (k :))) ys
    mapMergeWithKeyA f (LM xs1 ys1) (LM xs2 ys2) = LM
        <$> mapMaybeA (f []) (fromMaybes xs1 xs2)
        <*> mapMergeWithKeyA (\ k -> let f' = mapMergeWithKeyA (f . (k :)) in fmap Just . either' (`f'` mapEmpty) (mapEmpty `f'`) f') ys1 ys2

instance (Foldable m, Outputable a) => Outputable (ListMap m a) where
  ppr m = text "List elts" <+> ppr (toList m)

lkList :: (forall b. k -> m b -> Maybe b)
        -> [k] -> ListMap m a -> Maybe a
lkList _  []     = lm_nil
lkList lk (x:xs) = lm_cons >>> lk x >=> lkList lk xs

xtListF :: (IsKeylessMap m, Functor f) => (forall b. XTF f b -> k -> m b -> f (m b))
        -> XTF f a -> [k] -> ListMap m a -> f (ListMap m a)
xtListF tr = flip go
  where
    go = \ case
        [] -> lm_nilL
        x:xs -> lm_consL . flip tr x . xtf . go xs

{-
************************************************************************
*                                                                      *
                   Basic maps
*                                                                      *
************************************************************************
-}

type LiteralMap  a = Map.Map Literal a

{-
************************************************************************
*                                                                      *
                   GenMap
*                                                                      *
************************************************************************

Note [Compressed TrieMap]
~~~~~~~~~~~~~~~~~~~~~~~~~

The GenMap constructor augments TrieMaps with leaf compression.  This helps
solve the performance problem detailed in #9960: suppose we have a handful
H of entries in a TrieMap, each with a very large key, size K. If you fold over
such a TrieMap you'd expect time O(H). That would certainly be true of an
association list! But with TrieMap we actually have to navigate down a long
singleton structure to get to the elements, so it takes time O(K*H).  This
can really hurt on many type-level computation benchmarks:
see for example T9872d.

The point of a TrieMap is that you need to navigate to the point where only one
key remains, and then things should be fast.  So the point of a SingletonMap
is that, once we are down to a single (key,value) pair, we stop and
just use SingletonMap.

'EmptyMap' provides an even more basic (but essential) optimization: if there is
nothing in the map, don't bother building out the (possibly infinite) recursive
TrieMap structure!

Compressed triemaps are heavily used by GHC.Core.Map. So we have to mark some things
as INLINEABLE to permit specialization.
-}

data GenMap m a
   = EmptyMap
   | SingletonMap (KeyOf m) a
   | MultiMap (m a)
  deriving (Foldable, Functor, Traversable)

instance Filtrable m => Filtrable (GenMap m) where
    mapMaybe f = \ case
        EmptyMap -> EmptyMap
        SingletonMap k a -> maybe EmptyMap (SingletonMap k) (f a)
        MultiMap as -> MultiMap (mapMaybe f as)

instance (Outputable a, Outputable (m a)) => Outputable (GenMap m a) where
  ppr EmptyMap = text "Empty map"
  ppr (SingletonMap _ v) = text "Singleton map" <+> ppr v
  ppr (MultiMap m) = ppr m

-- TODO undecidable instance
instance (Eq (KeyOf m), IsStaticKeylessMap m) => IsStaticKeylessMap (GenMap m) where
    type KeyOf (GenMap m) = KeyOf m
    mapLookup = lkG
    mapAdjust f k = \ case
        EmptyMap -> EmptyMap
        SingletonMap k' a -> SingletonMap k' (applyWhen (k == k') f a)
        MultiMap as -> MultiMap (mapAdjust f k as)
    mapIsSubmapOfBy _ EmptyMap _ = True
    mapIsSubmapOfBy _ _ EmptyMap = False
    mapIsSubmapOfBy f (SingletonMap i a) bs = f a `any` mapLookup i bs
    mapIsSubmapOfBy f as (SingletonMap j b) = length as == 1 && flip f b `any` mapLookup j as
    mapIsSubmapOfBy f (MultiMap as) (MultiMap bs) = mapIsSubmapOfBy f as bs

-- TODO undecidable instance
instance (Eq (KeyOf m), IsStaticMap m) => IsStaticMap (GenMap m) where
    mapTraverseWithKey f = \ case
        EmptyMap -> pure EmptyMap
        SingletonMap k a -> SingletonMap k <$> f k a
        MultiMap as -> MultiMap <$> mapTraverseWithKey f as

-- TODO undecidable instance
instance (Eq (KeyOf m), IsKeylessMap m) => IsKeylessMap (GenMap m) where
    mapEmpty = EmptyMap
    mapAlterF = xtGF
    mapMergeA f EmptyMap bs = mapMaybeA (f . JustRight) bs
    mapMergeA f as EmptyMap = mapMaybeA (f . JustLeft) as
    mapMergeA f (SingletonMap i a) bs = maybe id (mapInsert i)
        <$> f (maybe (JustLeft  a) (a `Both`) (mapLookup i bs))
        <*> mapMaybeA (f . JustRight) bs
    mapMergeA f as (SingletonMap j b) = maybe id (mapInsert j)
        <$> f (maybe (JustRight b) (`Both` b) (mapLookup j as))
        <*> mapMaybeA (f . JustLeft) as
    mapMergeA f (MultiMap as) (MultiMap bs) = MultiMap <$> mapMergeA f as bs

-- TODO undecidable instance
instance (Eq (KeyOf m), IsMap m) => IsMap (GenMap m) where
    mapTraverseMaybeWithKey f = \ case
        EmptyMap -> pure EmptyMap
        SingletonMap k a -> maybe EmptyMap (SingletonMap k) <$> f k a
        MultiMap as -> MultiMap <$> mapTraverseMaybeWithKey f as
    mapMergeWithKeyA f EmptyMap bs = mapTraverseMaybeWithKey (\ k -> f k . JustRight) bs
    mapMergeWithKeyA f as EmptyMap = mapTraverseMaybeWithKey (\ k -> f k . JustLeft) as
    mapMergeWithKeyA f (SingletonMap i a) bs = maybe id (mapInsert i)
        <$> f i (maybe (JustLeft  a) (a `Both`) (mapLookup i bs))
        <*> mapTraverseMaybeWithKey (\ k -> bool (f k . JustRight) (\ _ -> pure Nothing) (i == k)) bs
    mapMergeWithKeyA f as (SingletonMap j b) = maybe id (mapInsert j)
        <$> f j (maybe (JustRight b) (`Both` b) (mapLookup j as))
        <*> mapTraverseMaybeWithKey (\ k -> bool (f k . JustLeft)  (\ _ -> pure Nothing) (j == k)) as
    mapMergeWithKeyA f (MultiMap as) (MultiMap bs) = MultiMap <$> mapMergeWithKeyA f as bs

--We want to be able to specialize these functions when defining eg
--tries over (GenMap CoreExpr) which requires INLINEABLE

{-# INLINEABLE lkG #-}
lkG :: (Eq (KeyOf m), IsStaticKeylessMap m) => KeyOf m -> GenMap m a -> Maybe a
lkG _ EmptyMap                         = Nothing
lkG k (SingletonMap k' v')             = v' <$ guard (k == k')
lkG k (MultiMap m)                     = mapLookup k m

{-# INLINEABLE xtGF #-}
xtGF :: (Eq (KeyOf m), IsKeylessMap m, Functor f) => XTF f a -> KeyOf m -> GenMap m a -> f (GenMap m a)
xtGF f k EmptyMap = f Nothing <₪> \ case
    Just v  -> SingletonMap k v
    Nothing -> EmptyMap
xtGF f k m@(SingletonMap k' v')
    | k' == k
    -- The new key matches the (single) key already in the tree.  Hence,
    -- apply @f@ to @Just v'@ and build a singleton or empty map depending
    -- on the 'Just'/'Nothing' response respectively.
    = f (Just v') <₪> \ case
        Just v'' -> SingletonMap k' v''
        Nothing  -> EmptyMap
    | otherwise
    -- We've hit a singleton tree for a different key than the one we are
    -- searching for. Hence apply @f@ to @Nothing@. If result is @Nothing@ then
    -- we can just return the old map. If not, we need a map with *two*
    -- entries. The easiest way to do that is to insert two items into an empty
    -- map of type @m a@.
    = f Nothing <₪> \ case
        Nothing  -> m
        Just v   -> (MultiMap . mapFromList) [(k, v), (k', v')]
xtGF f k (MultiMap m) = MultiMap <$> mapAlterF f k m
