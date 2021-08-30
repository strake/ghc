{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

#include "lens.h"

module GHC.Data.TrieMap(
   -- * Maps over 'Maybe' values
   MaybeMap,
   -- * Maps over 'List' values
   ListMap,
   -- * Maps over 'Literal's
   LiteralMap,
   -- * 'TrieMap' class
   TrieMap(..), insertTM, deleteTM,

   -- * Things helpful for adding additional Instances.
   (|>>), XT,
   -- * Map for leaf compression
   GenMap,
   lkG, xtG,
   xtList, lkList

 ) where

import GHC.Prelude

import GHC.Types.Literal
import GHC.Types.Unique.DFM
import GHC.Types.Unique( Uniquable )

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import GHC.Utils.Outputable
import Control.Category ( (>>>) )
import Control.Monad( (>=>) )
import Data.Foldable( toList )
import Data.Kind( Type )

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

************************************************************************
*                                                                      *
                   The TrieMap class
*                                                                      *
************************************************************************
-}

type XT a = Maybe a -> Maybe a  -- How to alter a non-existent elt (Nothing)
                                --               or an existing elt (Just)

class (Functor m, Foldable m) => TrieMap m where
   type Key m :: Type
   emptyTM  :: m a
   lookupTM :: forall b. Key m -> m b -> Maybe b
   alterTM  :: forall b. Key m -> XT b -> m b -> m b

insertTM :: TrieMap m => Key m -> a -> m a -> m a
insertTM k v = alterTM k (\_ -> Just v)

deleteTM :: TrieMap m => Key m -> m a -> m a
deleteTM k = alterTM k (\_ -> Nothing)

----------------------
-- Recall that
--   Control.Monad.(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c

infixr 1 |>>

----------------------
(|>>) :: TrieMap m2
      => (XT (m2 a) -> b)
      -> (m2 a -> m2 a)
      -> b
f |>> g = f (Just . g . fromMaybe emptyTM)

{-
************************************************************************
*                                                                      *
                   IntMaps
*                                                                      *
************************************************************************
-}

instance TrieMap IntMap.IntMap where
  type Key IntMap.IntMap = Int
  emptyTM = IntMap.empty
  lookupTM k m = IntMap.lookup k m
  alterTM = flip IntMap.alter

instance Ord k => TrieMap (Map.Map k) where
  type Key (Map.Map k) = k
  emptyTM = Map.empty
  lookupTM = Map.lookup
  alterTM = flip Map.alter


{-
Note [foldTM determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~
We want foldTM to be deterministic, which is why we have an instance of
TrieMap for UniqDFM, but not for UniqFM. Here's an example of some things that
go wrong if foldTM is nondeterministic. Consider:

  f a b = return (a <> b)

Depending on the order that the typechecker generates constraints you
get either:

  f :: (Monad m, Monoid a) => a -> a -> m a

or:

  f :: (Monoid a, Monad m) => a -> a -> m a

The generated code will be different after desugaring as the dictionaries
will be bound in different orders, leading to potential ABI incompatibility.

One way to solve this would be to notice that the typeclasses could be
sorted alphabetically.

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

GHC is free to choose either dictionary to implement f, but for the sake of
determinism we'd like it to be consistent when compiling the same sources
with the same flags.

inert_dicts :: DictMap is implemented with a TrieMap. In getUnsolvedInerts it
gets converted to a bag of (Wanted) Cts using a fold. Then in
solve_simple_wanteds it's merged with other WantedConstraints. We want the
conversion to a bag to be deterministic. For that purpose we use UniqDFM
instead of UniqFM to implement the TrieMap.

See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for more details on how it's made
deterministic.
-}

instance forall key. Uniquable key => TrieMap (UniqDFM key) where
  type Key (UniqDFM key) = key
  emptyTM = emptyUDFM
  lookupTM k m = lookupUDFM m k
  alterTM k f m = alterUDFM f m k

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

instance TrieMap m => TrieMap (MaybeMap m) where
   type Key (MaybeMap m) = Maybe (Key m)
   emptyTM  = MM { mm_nothing = Nothing, mm_just = emptyTM }
   lookupTM = \ case
       Nothing -> mm_nothing
       Just x -> lookupTM x . mm_just
   alterTM = \ case
       Nothing -> over mm_nothingL
       Just x -> over mm_justL . alterTM x

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

instance TrieMap m => TrieMap (ListMap m) where
   type Key (ListMap m) = [Key m]
   emptyTM  = LM { lm_nil = Nothing, lm_cons = emptyTM }
   lookupTM = lkList lookupTM
   alterTM  = xtList alterTM

instance (TrieMap m, Outputable a) => Outputable (ListMap m a) where
  ppr m = text "List elts" <+> ppr (toList m)

lkList :: TrieMap m => (forall b. k -> m b -> Maybe b)
        -> [k] -> ListMap m a -> Maybe a
lkList _  []     = lm_nil
lkList lk (x:xs) = lm_cons >>> lk x >=> lkList lk xs

xtList :: TrieMap m => (forall b. k -> XT b -> m b -> m b)
        -> [k] -> XT a -> ListMap m a -> ListMap m a
xtList _  []     f = over lm_nilL f
xtList tr (x:xs) f = over lm_consL $ tr x |>> xtList tr xs f

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
   | SingletonMap (Key m) a
   | MultiMap (m a)
  deriving (Foldable, Functor, Traversable)

instance (Outputable a, Outputable (m a)) => Outputable (GenMap m a) where
  ppr EmptyMap = text "Empty map"
  ppr (SingletonMap _ v) = text "Singleton map" <+> ppr v
  ppr (MultiMap m) = ppr m

-- TODO undecidable instance
instance (Eq (Key m), TrieMap m) => TrieMap (GenMap m) where
   type Key (GenMap m) = Key m
   emptyTM  = EmptyMap
   lookupTM = lkG
   alterTM  = xtG

--We want to be able to specialize these functions when defining eg
--tries over (GenMap CoreExpr) which requires INLINEABLE

{-# INLINEABLE lkG #-}
lkG :: (Eq (Key m), TrieMap m) => Key m -> GenMap m a -> Maybe a
lkG _ EmptyMap                         = Nothing
lkG k (SingletonMap k' v')             = v' <$ guard (k == k')
lkG k (MultiMap m)                     = lookupTM k m

{-# INLINEABLE xtG #-}
xtG :: (Eq (Key m), TrieMap m) => Key m -> XT a -> GenMap m a -> GenMap m a
xtG k f EmptyMap
    = case f Nothing of
        Just v  -> SingletonMap k v
        Nothing -> EmptyMap
xtG k f m@(SingletonMap k' v')
    | k' == k
    -- The new key matches the (single) key already in the tree.  Hence,
    -- apply @f@ to @Just v'@ and build a singleton or empty map depending
    -- on the 'Just'/'Nothing' response respectively.
    = case f (Just v') of
        Just v'' -> SingletonMap k' v''
        Nothing  -> EmptyMap
    | otherwise
    -- We've hit a singleton tree for a different key than the one we are
    -- searching for. Hence apply @f@ to @Nothing@. If result is @Nothing@ then
    -- we can just return the old map. If not, we need a map with *two*
    -- entries. The easiest way to do that is to insert two items into an empty
    -- map of type @m a@.
    = case f Nothing of
        Nothing  -> m
        Just v   -> (alterTM k' (const (Just v'))
                           >>> alterTM k  (const (Just v))
                           >>> MultiMap) emptyTM
xtG k f (MultiMap m) = MultiMap (alterTM k f m)
