{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TrieMap(
   CoreMap, emptyCoreMap, extendCoreMap, lookupCoreMap, foldCoreMap,
   TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap, foldTypeMap,
   CoercionMap,
   MaybeMap,
   ListMap,
   TrieMap(..), insertTM, deleteTM,
   lookupTypeMapTyCon
 ) where

import CoreSyn
import Coercion
import Literal
import Name
import Type
import TypeRep
import TyCon(TyCon)
import Var
import UniqFM
import Unique( Unique )
import FastString(FastString)
import CoAxiom(CoAxiomRule(coaxrName))

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import VarEnv
import NameEnv
import Outputable
import Control.Monad( (>=>) )

{-
This module implements TrieMaps, which are finite mappings
whose key is a structured value like a CoreExpr or Type.

The code is very regular and boilerplate-like, but there is
some neat handling of *binders*.  In effect they are deBruijn
numbered on the fly.

************************************************************************
*                                                                      *
                   The TrieMap class
*                                                                      *
************************************************************************
-}

type XT a = Maybe a -> Maybe a  -- How to alter a non-existent elt (Nothing)
                                --               or an existing elt (Just)

class TrieMap m where
   type Key m :: *
   emptyTM  :: m a
   lookupTM :: forall b. Key m -> m b -> Maybe b
   alterTM  :: forall b. Key m -> XT b -> m b -> m b
   mapTM    :: (a->b) -> m a -> m b

   foldTM   :: (a -> b -> b) -> m a -> b -> b
      -- The unusual argument order here makes
      -- it easy to compose calls to foldTM;
      -- see for example fdE below

insertTM :: TrieMap m => Key m -> a -> m a -> m a
insertTM k v m = alterTM k (\_ -> Just v) m

deleteTM :: TrieMap m => Key m -> m a -> m a
deleteTM k m = alterTM k (\_ -> Nothing) m

----------------------
-- Recall that
--   Control.Monad.(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c

(>.>) :: (a -> b) -> (b -> c) -> a -> c
-- Reverse function composition (do f first, then g)
infixr 1 >.>
(f >.> g) x = g (f x)
infixr 1 |>, |>>

(|>) :: a -> (a->b) -> b     -- Reverse application
x |> f = f x

----------------------
(|>>) :: TrieMap m2
      => (XT (m2 a) -> m1 (m2 a) -> m1 (m2 a))
      -> (m2 a -> m2 a)
      -> m1 (m2 a) -> m1 (m2 a)
(|>>) f g = f (Just . g . deMaybe)

deMaybe :: TrieMap m => Maybe (m a) -> m a
deMaybe Nothing  = emptyTM
deMaybe (Just m) = m

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
  alterTM = xtInt
  foldTM k m z = IntMap.fold k z m
  mapTM f m = IntMap.map f m

xtInt :: Int -> XT a -> IntMap.IntMap a -> IntMap.IntMap a
xtInt k f m = IntMap.alter f k m

instance Ord k => TrieMap (Map.Map k) where
  type Key (Map.Map k) = k
  emptyTM = Map.empty
  lookupTM = Map.lookup
  alterTM k f m = Map.alter f k m
  foldTM k m z = Map.fold k z m
  mapTM f m = Map.map f m

instance TrieMap UniqFM where
  type Key UniqFM = Unique
  emptyTM = emptyUFM
  lookupTM k m = lookupUFM m k
  alterTM k f m = alterUFM f m k
  foldTM k m z = foldUFM k z m
  mapTM f m = mapUFM f m

{-
************************************************************************
*                                                                      *
                   Lists
*                                                                      *
************************************************************************

If              m is a map from k -> val
then (MaybeMap m) is a map from (Maybe k) -> val
-}

data MaybeMap m a = MM { mm_nothing  :: Maybe a, mm_just :: m a }

instance TrieMap m => TrieMap (MaybeMap m) where
   type Key (MaybeMap m) = Maybe (Key m)
   emptyTM  = MM { mm_nothing = Nothing, mm_just = emptyTM }
   lookupTM = lkMaybe lookupTM
   alterTM  = xtMaybe alterTM
   foldTM   = fdMaybe
   mapTM    = mapMb

mapMb :: TrieMap m => (a->b) -> MaybeMap m a -> MaybeMap m b
mapMb f (MM { mm_nothing = mn, mm_just = mj })
  = MM { mm_nothing = fmap f mn, mm_just = mapTM f mj }

lkMaybe :: (forall b. k -> m b -> Maybe b)
        -> Maybe k -> MaybeMap m a -> Maybe a
lkMaybe _  Nothing  = mm_nothing
lkMaybe lk (Just x) = mm_just >.> lk x

xtMaybe :: (forall b. k -> XT b -> m b -> m b)
        -> Maybe k -> XT a -> MaybeMap m a -> MaybeMap m a
xtMaybe _  Nothing  f m = m { mm_nothing  = f (mm_nothing m) }
xtMaybe tr (Just x) f m = m { mm_just = mm_just m |> tr x f }

fdMaybe :: TrieMap m => (a -> b -> b) -> MaybeMap m a -> b -> b
fdMaybe k m = foldMaybe k (mm_nothing m)
            . foldTM k (mm_just m)

--------------------
data ListMap m a
  = LM { lm_nil  :: Maybe a
       , lm_cons :: m (ListMap m a) }

instance TrieMap m => TrieMap (ListMap m) where
   type Key (ListMap m) = [Key m]
   emptyTM  = LM { lm_nil = Nothing, lm_cons = emptyTM }
   lookupTM = lkList lookupTM
   alterTM  = xtList alterTM
   foldTM   = fdList
   mapTM    = mapList

mapList :: TrieMap m => (a->b) -> ListMap m a -> ListMap m b
mapList f (LM { lm_nil = mnil, lm_cons = mcons })
  = LM { lm_nil = fmap f mnil, lm_cons = mapTM (mapTM f) mcons }

lkList :: TrieMap m => (forall b. k -> m b -> Maybe b)
        -> [k] -> ListMap m a -> Maybe a
lkList _  []     = lm_nil
lkList lk (x:xs) = lm_cons >.> lk x >=> lkList lk xs

xtList :: TrieMap m => (forall b. k -> XT b -> m b -> m b)
        -> [k] -> XT a -> ListMap m a -> ListMap m a
xtList _  []     f m = m { lm_nil  = f (lm_nil m) }
xtList tr (x:xs) f m = m { lm_cons = lm_cons m |> tr x |>> xtList tr xs f }

fdList :: forall m a b. TrieMap m
       => (a -> b -> b) -> ListMap m a -> b -> b
fdList k m = foldMaybe k          (lm_nil m)
           . foldTM    (fdList k) (lm_cons m)

foldMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldMaybe _ Nothing  b = b
foldMaybe k (Just a) b = k a b

{-
************************************************************************
*                                                                      *
                   Basic maps
*                                                                      *
************************************************************************
-}

lkNamed :: NamedThing n => n -> NameEnv a -> Maybe a
lkNamed n env = lookupNameEnv env (getName n)

xtNamed :: NamedThing n => n -> XT a -> NameEnv a -> NameEnv a
xtNamed tc f m = alterNameEnv f m (getName tc)

------------------------
type LiteralMap  a = Map.Map Literal a

emptyLiteralMap :: LiteralMap a
emptyLiteralMap = emptyTM

lkLit :: Literal -> LiteralMap a -> Maybe a
lkLit = lookupTM

xtLit :: Literal -> XT a -> LiteralMap a -> LiteralMap a
xtLit = alterTM

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
-}

data GenMap m a
   = EmptyMap
   | SingletonMap (Key m) a
   | MultiMap (m a)

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
   foldTM   = fdG
   mapTM    = mapG

-- NB: Be careful about RULES and type families (#5821).  So we should make sure
-- to specify @Key TypeMapX@ (and not @DeBruijn Type@, the reduced form)

{-# SPECIALIZE lkG :: Key TypeMapX     -> TypeMapG a     -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoercionMapX -> CoercionMapG a -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoreMapX     -> CoreMapG a     -> Maybe a #-}
lkG :: (Eq (Key m), TrieMap m) => Key m -> GenMap m a -> Maybe a
lkG _ EmptyMap                         = Nothing
lkG k (SingletonMap k' v') | k == k'   = Just v'
                           | otherwise = Nothing
lkG k (MultiMap m)                     = lookupTM k m

{-# SPECIALIZE xtG :: Key TypeMapX     -> XT a -> TypeMapG a -> TypeMapG a #-}
{-# SPECIALIZE xtG :: Key CoercionMapX -> XT a -> CoercionMapG a -> CoercionMapG a #-}
{-# SPECIALIZE xtG :: Key CoreMapX     -> XT a -> CoreMapG a -> CoreMapG a #-}
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
        Just v   -> emptyTM |> alterTM k' (const (Just v'))
                           >.> alterTM k  (const (Just v))
                           >.> MultiMap
xtG k f (MultiMap m) = MultiMap (alterTM k f m)

{-# SPECIALIZE mapG :: (a -> b) -> TypeMapG a     -> TypeMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoercionMapG a -> CoercionMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoreMapG a     -> CoreMapG b #-}
mapG :: TrieMap m => (a -> b) -> GenMap m a -> GenMap m b
mapG _ EmptyMap = EmptyMap
mapG f (SingletonMap k v) = SingletonMap k (f v)
mapG f (MultiMap m) = MultiMap (mapTM f m)

{-# SPECIALIZE fdG :: (a -> b -> b) -> TypeMapG a     -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoercionMapG a -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoreMapG a     -> b -> b #-}
fdG :: TrieMap m => (a -> b -> b) -> GenMap m a -> b -> b
fdG _ EmptyMap = \z -> z
fdG k (SingletonMap _ v) = \z -> k v z
fdG k (MultiMap m) = foldTM k m

{-
************************************************************************
*                                                                      *
                   CoreMap
*                                                                      *
************************************************************************

Note [Binders]
~~~~~~~~~~~~~~
 * In general we check binders as late as possible because types are
   less likely to differ than expression structure.  That's why
      cm_lam :: CoreMapG (TypeMapG a)
   rather than
      cm_lam :: TypeMapG (CoreMapG a)

 * We don't need to look at the type of some binders, notalby
     - the case binder in (Case _ b _ _)
     - the binders in an alternative
   because they are totally fixed by the context

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* For a key (Case e b ty (alt:alts))  we don't need to look the return type
  'ty', because every alternative has that type.

* For a key (Case e b ty []) we MUST look at the return type 'ty', because
  otherwise (Case (error () "urk") _ Int  []) would compare equal to
            (Case (error () "urk") _ Bool [])
  which is utterly wrong (Trac #6097)

We could compare the return type regardless, but the wildly common case
is that it's unnecesary, so we have two fields (cm_case and cm_ecase)
for the two possibilities.  Only cm_ecase looks at the type.

See also Note [Empty case alternatives] in CoreSyn.
-}

-- | @CoreMap a@ is a map from 'CoreExpr' to @a@.  If you are a client, this
-- is the type you want.
newtype CoreMap a = CoreMap (CoreMapG a)

instance TrieMap CoreMap where
    type Key CoreMap = CoreExpr
    emptyTM = CoreMap emptyTM
    lookupTM k (CoreMap m) = lookupTM (deBruijnize k) m
    alterTM k f (CoreMap m) = CoreMap (alterTM (deBruijnize k) f m)
    foldTM k (CoreMap m) = foldTM k m
    mapTM f (CoreMap m) = CoreMap (mapTM f m)

-- | @CoreMapG a@ is a map from @DeBruijn CoreExpr@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'CoreMap'
-- inside another 'TrieMap', this is the type you want.
type CoreMapG = GenMap CoreMapX

-- | @CoreMapX a@ is the base map from @DeBruijn CoreExpr@ to @a@, but without
-- the 'GenMap' optimization.
data CoreMapX a
  = CM { cm_var   :: VarMap a
       , cm_lit   :: LiteralMap a
       , cm_co    :: CoercionMapG a
       , cm_type  :: TypeMapG a
       , cm_cast  :: CoreMapG (CoercionMapG a)
       , cm_tick  :: CoreMapG (TickishMap a)
       , cm_app   :: CoreMapG (CoreMapG a)
       , cm_lam   :: CoreMapG (BndrMap a)    -- Note [Binders]
       , cm_letn  :: CoreMapG (CoreMapG (BndrMap a))
       , cm_letr  :: ListMap CoreMapG (CoreMapG (ListMap BndrMap a))
       , cm_case  :: CoreMapG (ListMap AltMap a)
       , cm_ecase :: CoreMapG (TypeMapG a)    -- Note [Empty case alternatives]
     }

instance Eq (DeBruijn CoreExpr) where
  D env1 e1 == D env2 e2 = go e1 e2 where
    go (Var v1) (Var v2) = case (lookupCME env1 v1, lookupCME env2 v2) of
                            (Just b1, Just b2) -> b1 == b2
                            (Nothing, Nothing) -> v1 == v2
                            _ -> False
    go (Lit lit1)    (Lit lit2)      = lit1 == lit2
    go (Type t1)    (Type t2)        = D env1 t1 == D env2 t2
    go (Coercion co1) (Coercion co2) = D env1 co1 == D env2 co2
    go (Cast e1 co1) (Cast e2 co2) = D env1 co1 == D env2 co2 && go e1 e2
    go (App f1 a1)   (App f2 a2)   = go f1 f2 && go a1 a2
    -- This seems a bit dodgy, see 'eqTickish'
    go (Tick n1 e1)  (Tick n2 e2)  = n1 == n2 && go e1 e2

    go (Lam b1 e1)  (Lam b2 e2)
      =  D env1 (varType b1) == D env2 (varType b2)
      && D (extendCME env1 b1) e1 == D (extendCME env2 b2) e2

    go (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      =  go r1 r2
      && D (extendCME env1 v1) e1 == D (extendCME env2 v2) e2

    go (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = length ps1 == length ps2
      && D env1' rs1 == D env2' rs2
      && D env1' e1  == D env2' e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env1' = extendCMEs env1 bs1
        env2' = extendCMEs env2 bs2

    go (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives]
      = null a2 && go e1 e2 && D env1 t1 == D env2 t2
      | otherwise
      =  go e1 e2 && D (extendCME env1 b1) a1 == D (extendCME env2 b2) a2

    go _ _ = False

emptyE :: CoreMapX a
emptyE = CM { cm_var = emptyTM, cm_lit = emptyLiteralMap
            , cm_co = emptyTM, cm_type = emptyTM
            , cm_cast = emptyTM, cm_app = emptyTM
            , cm_lam = emptyTM, cm_letn = emptyTM
            , cm_letr = emptyTM, cm_case = emptyTM
            , cm_ecase = emptyTM, cm_tick = emptyTM }

instance TrieMap CoreMapX where
   type Key CoreMapX = DeBruijn CoreExpr
   emptyTM  = emptyE
   lookupTM = lkE
   alterTM  = xtE
   foldTM   = fdE
   mapTM    = mapE

--------------------------
mapE :: (a->b) -> CoreMapX a -> CoreMapX b
mapE f (CM { cm_var = cvar, cm_lit = clit
           , cm_co = cco, cm_type = ctype
           , cm_cast = ccast , cm_app = capp
           , cm_lam = clam, cm_letn = cletn
           , cm_letr = cletr, cm_case = ccase
           , cm_ecase = cecase, cm_tick = ctick })
  = CM { cm_var = mapTM f cvar, cm_lit = mapTM f clit
       , cm_co = mapTM f cco, cm_type = mapTM f ctype
       , cm_cast = mapTM (mapTM f) ccast, cm_app = mapTM (mapTM f) capp
       , cm_lam = mapTM (mapTM f) clam, cm_letn = mapTM (mapTM (mapTM f)) cletn
       , cm_letr = mapTM (mapTM (mapTM f)) cletr, cm_case = mapTM (mapTM f) ccase
       , cm_ecase = mapTM (mapTM f) cecase, cm_tick = mapTM (mapTM f) ctick }

--------------------------
lookupCoreMap :: CoreMap a -> CoreExpr -> Maybe a
lookupCoreMap cm e = lookupTM e cm

extendCoreMap :: CoreMap a -> CoreExpr -> a -> CoreMap a
extendCoreMap m e v = alterTM e (\_ -> Just v) m

foldCoreMap :: (a -> b -> b) -> b -> CoreMap a -> b
foldCoreMap k z m = foldTM k m z

emptyCoreMap :: CoreMap a
emptyCoreMap = emptyTM

instance Outputable a => Outputable (CoreMap a) where
  ppr m = text "CoreMap elts" <+> ppr (foldTM (:) m [])

-------------------------
fdE :: (a -> b -> b) -> CoreMapX a -> b -> b
fdE k m
  = foldTM k (cm_var m)
  . foldTM k (cm_lit m)
  . foldTM k (cm_co m)
  . foldTM k (cm_type m)
  . foldTM (foldTM k) (cm_cast m)
  . foldTM (foldTM k) (cm_tick m)
  . foldTM (foldTM k) (cm_app m)
  . foldTM (foldTM k) (cm_lam m)
  . foldTM (foldTM (foldTM k)) (cm_letn m)
  . foldTM (foldTM (foldTM k)) (cm_letr m)
  . foldTM (foldTM k) (cm_case m)
  . foldTM (foldTM k) (cm_ecase m)

-- lkE: lookup in trie for expressions
lkE :: DeBruijn CoreExpr -> CoreMapX a -> Maybe a
lkE (D env expr) cm = go expr cm
  where
    go (Var v)              = cm_var  >.> lkVar env v
    go (Lit l)              = cm_lit  >.> lkLit l
    go (Type t)             = cm_type >.> lkG (D env t)
    go (Coercion c)         = cm_co   >.> lkG (D env c)
    go (Cast e c)           = cm_cast >.> lkG (D env e) >=> lkG (D env c)
    go (Tick tickish e)     = cm_tick >.> lkG (D env e) >=> lkTickish tickish
    go (App e1 e2)          = cm_app  >.> lkG (D env e2) >=> lkG (D env e1)
    go (Lam v e)            = cm_lam  >.> lkG (D (extendCME env v) e)
                              >=> lkBndr env v
    go (Let (NonRec b r) e) = cm_letn >.> lkG (D env r)
                              >=> lkG (D (extendCME env b) e) >=> lkBndr env b
    go (Let (Rec prs) e)    = let (bndrs,rhss) = unzip prs
                                  env1 = extendCMEs env bndrs
                              in cm_letr
                                 >.> lkList (lkG . D env1) rhss
                                 >=> lkG (D env1 e)
                                 >=> lkList (lkBndr env1) bndrs
    go (Case e b ty as)     -- See Note [Empty case alternatives]
               | null as    = cm_ecase >.> lkG (D env e) >=> lkG (D env ty)
               | otherwise  = cm_case >.> lkG (D env e)
                              >=> lkList (lkA (extendCME env b)) as

xtE :: DeBruijn CoreExpr -> XT a -> CoreMapX a -> CoreMapX a
xtE (D env (Var v))              f m = m { cm_var  = cm_var m
                                                 |> xtVar env v f }
xtE (D env (Type t))             f m = m { cm_type = cm_type m
                                                 |> xtG (D env t) f }
xtE (D env (Coercion c))         f m = m { cm_co   = cm_co m
                                                 |> xtG (D env c) f }
xtE (D _   (Lit l))              f m = m { cm_lit  = cm_lit m  |> xtLit l f }
xtE (D env (Cast e c))           f m = m { cm_cast = cm_cast m |> xtG (D env e)
                                                 |>> xtG (D env c) f }
xtE (D env (Tick t e))           f m = m { cm_tick = cm_tick m |> xtG (D env e)
                                                 |>> xtTickish t f }
xtE (D env (App e1 e2))          f m = m { cm_app = cm_app m |> xtG (D env e2)
                                                 |>> xtG (D env e1) f }
xtE (D env (Lam v e))            f m = m { cm_lam = cm_lam m
                                                 |> xtG (D (extendCME env v) e)
                                                 |>> xtBndr env v f }
xtE (D env (Let (NonRec b r) e)) f m = m { cm_letn = cm_letn m
                                                 |> xtG (D (extendCME env b) e)
                                                 |>> xtG (D env r)
                                                 |>> xtBndr env b f }
xtE (D env (Let (Rec prs) e))    f m = m { cm_letr =
                                              let (bndrs,rhss) = unzip prs
                                                  env1 = extendCMEs env bndrs
                                              in cm_letr m
                                                 |>  xtList (xtG . D env1) rhss
                                                 |>> xtG (D env1 e)
                                                 |>> xtList (xtBndr env1)
                                                            bndrs f }
xtE (D env (Case e b ty as))     f m
                     | null as   = m { cm_ecase = cm_ecase m |> xtG (D env e)
                                                 |>> xtG (D env ty) f }
                     | otherwise = m { cm_case = cm_case m |> xtG (D env e)
                                                 |>> let env1 = extendCME env b
                                                     in xtList (xtA env1) as f }

-- TODO: this seems a bit dodgy, see 'eqTickish'
type TickishMap a = Map.Map (Tickish Id) a
lkTickish :: Tickish Id -> TickishMap a -> Maybe a
lkTickish = lookupTM

xtTickish :: Tickish Id -> XT a -> TickishMap a -> TickishMap a
xtTickish = alterTM

------------------------
data AltMap a   -- A single alternative
  = AM { am_deflt :: CoreMapG a
       , am_data  :: NameEnv (CoreMapG a)
       , am_lit   :: LiteralMap (CoreMapG a) }

instance TrieMap AltMap where
   type Key AltMap = CoreAlt
   emptyTM  = AM { am_deflt = emptyTM
                 , am_data = emptyNameEnv
                 , am_lit  = emptyLiteralMap }
   lookupTM = lkA emptyCME
   alterTM  = xtA emptyCME
   foldTM   = fdA
   mapTM    = mapA

instance Eq (DeBruijn CoreAlt) where
  D env1 a1 == D env2 a2 = go a1 a2 where
    go (DEFAULT, _, rhs1) (DEFAULT, _, rhs2)
        = D env1 rhs1 == D env2 rhs2
    go (LitAlt lit1, _, rhs1) (LitAlt lit2, _, rhs2)
        = lit1 == lit2 && D env1 rhs1 == D env2 rhs2
    go (DataAlt dc1, bs1, rhs1) (DataAlt dc2, bs2, rhs2)
        = dc1 == dc2 &&
          D (extendCMEs env1 bs1) rhs1 == D (extendCMEs env2 bs2) rhs2
    go _ _ = False

mapA :: (a->b) -> AltMap a -> AltMap b
mapA f (AM { am_deflt = adeflt, am_data = adata, am_lit = alit })
  = AM { am_deflt = mapTM f adeflt
       , am_data = mapNameEnv (mapTM f) adata
       , am_lit = mapTM (mapTM f) alit }

lkA :: CmEnv -> CoreAlt -> AltMap a -> Maybe a
lkA env (DEFAULT,    _, rhs)  = am_deflt >.> lkG (D env rhs)
lkA env (LitAlt lit, _, rhs)  = am_lit >.> lkLit lit >=> lkG (D env rhs)
lkA env (DataAlt dc, bs, rhs) = am_data >.> lkNamed dc
                                        >=> lkG (D (extendCMEs env bs) rhs)

xtA :: CmEnv -> CoreAlt -> XT a -> AltMap a -> AltMap a
xtA env (DEFAULT, _, rhs)    f m =
    m { am_deflt = am_deflt m |> xtG (D env rhs) f }
xtA env (LitAlt l, _, rhs)   f m =
    m { am_lit   = am_lit m   |> xtLit l |>> xtG (D env rhs) f }
xtA env (DataAlt d, bs, rhs) f m =
    m { am_data  = am_data m  |> xtNamed d
                             |>> xtG (D (extendCMEs env bs) rhs) f }

fdA :: (a -> b -> b) -> AltMap a -> b -> b
fdA k m = foldTM k (am_deflt m)
        . foldTM (foldTM k) (am_data m)
        . foldTM (foldTM k) (am_lit m)

{-
************************************************************************
*                                                                      *
                   Coercions
*                                                                      *
************************************************************************
-}

newtype CoercionMap a = CoercionMap (CoercionMapG a)

instance TrieMap CoercionMap where
    type Key CoercionMap = Coercion
    emptyTM = CoercionMap emptyTM
    lookupTM k (CoercionMap m) = lookupTM (deBruijnize k) m
    alterTM k f (CoercionMap m) = CoercionMap (alterTM (deBruijnize k) f m)
    foldTM k (CoercionMap m) = foldTM k m
    mapTM f (CoercionMap m) = CoercionMap (mapTM f m)

type CoercionMapG = GenMap CoercionMapX
data CoercionMapX a
  = KM { km_refl   :: RoleMap (TypeMapG a)
       , km_tc_app :: RoleMap (NameEnv (ListMap CoercionMapG a))
       , km_app    :: CoercionMapG (CoercionMapG a)
       , km_forall :: CoercionMapG (BndrMap a) -- See Note [Binders]
       , km_var    :: VarMap a
       , km_axiom  :: NameEnv (IntMap.IntMap (ListMap CoercionMapG a))
       , km_univ   :: RoleMap (TypeMapG (TypeMapG a))
       , km_sym    :: CoercionMapG a
       , km_trans  :: CoercionMapG (CoercionMapG a)
       , km_nth    :: IntMap.IntMap (CoercionMapG a)
       , km_left   :: CoercionMapG a
       , km_right  :: CoercionMapG a
       , km_inst   :: CoercionMapG (TypeMapG a)
       , km_sub    :: CoercionMapG a
       , km_axiom_rule :: Map.Map FastString
                                  (ListMap TypeMapG (ListMap CoercionMapG a))
       }

instance Eq (DeBruijn Coercion) where
    D env1 co1 == D env2 co2 = go co1 co2 where
        go (Refl eq1 ty1) (Refl eq2 ty2)
            = eq1 == eq2 && D env1 ty1 == D env2 ty2
        go (TyConAppCo eq1 tc1 cos1) (TyConAppCo eq2 tc2 cos2)
            = eq1 == eq2 && tc1 == tc2 && D env1 cos1 == D env2 cos2
        go (AppCo co11 co12) (AppCo co21 co22)
            = D env1 co11 == D env2 co21 &&
              D env1 co12 == D env2 co22
        go (ForAllCo v1 co1) (ForAllCo v2 co2)
            = D env1 (tyVarKind v1)     == D env2 (tyVarKind v2) &&
              D (extendCME env1 v1) co1 == D (extendCME env2 v2) co2
        go (CoVarCo cv1) (CoVarCo cv2)
            = case (lookupCME env1 cv1, lookupCME env2 cv2) of
                (Just bv1, Just bv2) -> bv1 == bv2
                (Nothing, Nothing)   -> cv1 == cv2
                _ -> False
        go (AxiomInstCo con1 ind1 cos1) (AxiomInstCo con2 ind2 cos2)
            = con1 == con2 && ind1 == ind2 && D env1 cos1 == D env2 cos2
        go (UnivCo _ r1 ty11 ty12) (UnivCo _ r2 ty21 ty22)
            = r1 == r2 && D env1 ty11 == D env2 ty21 &&
                          D env1 ty12 == D env2 ty22
        go (SymCo co1) (SymCo co2)
            = D env1 co1 == D env2 co2
        go (TransCo co11 co12) (TransCo co21 co22)
            = D env1 co11 == D env2 co21 &&
              D env1 co12 == D env2 co22
        go (NthCo d1 co1) (NthCo d2 co2)
            = d1 == d2 && D env1 co1 == D env2 co2
        go (LRCo d1 co1) (LRCo d2 co2)
            = d1 == d2 && D env1 co1 == D env2 co2
        go (InstCo co1 ty1) (InstCo co2 ty2)
            = D env1 co1 == D env2 co2 && D env1 ty1 == D env2 ty2
        go (SubCo co1) (SubCo co2)
            = D env1 co1 == D env2 co2
        go (AxiomRuleCo a1 ts1 cs1) (AxiomRuleCo a2 ts2 cs2)
            = a1 == a2 && D env1 ts1 == D env2 ts2 && D env1 cs1 == D env2 cs2
        go _ _ = False


emptyC :: CoercionMapX a
emptyC = KM { km_refl = emptyTM, km_tc_app = emptyTM
            , km_app = emptyTM, km_forall = emptyTM
            , km_var = emptyTM, km_axiom = emptyNameEnv
            , km_univ = emptyTM, km_sym = emptyTM, km_trans = emptyTM
            , km_nth = emptyTM, km_left = emptyTM, km_right = emptyTM
            , km_inst = emptyTM, km_sub = emptyTM
            , km_axiom_rule = emptyTM }

instance TrieMap CoercionMapX where
   type Key CoercionMapX = DeBruijn Coercion
   emptyTM  = emptyC
   lookupTM = lkC
   alterTM  = xtC
   foldTM   = fdC
   mapTM    = mapC

mapC :: (a->b) -> CoercionMapX a -> CoercionMapX b
mapC f (KM { km_refl = krefl, km_tc_app = ktc
           , km_app = kapp, km_forall = kforall
           , km_var = kvar, km_axiom = kax
           , km_univ   = kuniv  , km_sym = ksym, km_trans = ktrans
           , km_nth = knth, km_left = kml, km_right = kmr
           , km_inst = kinst, km_sub = ksub
           , km_axiom_rule = kaxr })
  = KM { km_refl   = mapTM (mapTM f) krefl
       , km_tc_app = mapTM (mapNameEnv (mapTM f)) ktc
       , km_app    = mapTM (mapTM f) kapp
       , km_forall = mapTM (mapTM f) kforall
       , km_var    = mapTM f kvar
       , km_axiom  = mapNameEnv (IntMap.map (mapTM f)) kax
       , km_univ   = mapTM (mapTM (mapTM f)) kuniv
       , km_sym    = mapTM f ksym
       , km_trans  = mapTM (mapTM f) ktrans
       , km_nth    = IntMap.map (mapTM f) knth
       , km_left   = mapTM f kml
       , km_right  = mapTM f kmr
       , km_inst   = mapTM (mapTM f) kinst
       , km_sub    = mapTM f ksub
       , km_axiom_rule = mapTM (mapTM (mapTM f)) kaxr
       }

lkC :: DeBruijn Coercion -> CoercionMapX a -> Maybe a
lkC (D env co) m = go co m
  where
    go (Refl r ty)             = km_refl   >.> lookupTM r >=> lkG (D env ty)
    go (TyConAppCo r tc cs)    = km_tc_app >.> lookupTM r >=> lkNamed tc >=>
                                    lkList (lkG . D env) cs
    go (AxiomInstCo ax ind cs) = km_axiom  >.> lkNamed ax >=> lookupTM ind >=>
                                    lkList (lkG . D env) cs
    go (AppCo c1 c2)           = km_app    >.> lkG (D env c1) >=> lkG (D env c2)
    go (TransCo c1 c2)         = km_trans  >.> lkG (D env c1) >=> lkG (D env c2)

    -- the provenance is not used in the map
    go (UnivCo _ r t1 t2)      = km_univ   >.> lookupTM r >=> lkG (D env t1) >=>
                                    lkG (D env t2)
    go (InstCo c t)            = km_inst   >.> lkG (D env c)  >=> lkG (D env t)
    go (ForAllCo v c)          = km_forall >.> lkG (D (extendCME env v) c) >=>
                                    lkBndr env v
    go (CoVarCo v)             = km_var    >.> lkVar env v
    go (SymCo c)               = km_sym    >.> lkG (D env c)
    go (NthCo n c)             = km_nth    >.> lookupTM n >=> lkG (D env c)
    go (LRCo CLeft  c)         = km_left   >.> lkG (D env c)
    go (LRCo CRight c)         = km_right  >.> lkG (D env c)
    go (SubCo c)               = km_sub    >.> lkG (D env c)
    go (AxiomRuleCo co ts cs)  = km_axiom_rule >.>
                                    lookupTM (coaxrName co) >=>
                                    lkList (lkG . D env) ts >=>
                                    lkList (lkG . D env) cs


xtC :: DeBruijn Coercion -> XT a -> CoercionMapX a -> CoercionMapX a
xtC (D env c) f m = case c of
 Refl r ty          -> m { km_refl   = km_refl m   |> xtR r
                                                  |>> xtG (D env ty) f }
 TyConAppCo r tc cs -> m { km_tc_app = km_tc_app m |> xtR r |>> xtNamed tc
                                                  |>> xtList (xtG . D env) cs f}
 AxiomInstCo ax ind cs -> m { km_axiom  = km_axiom m |> xtNamed ax |>> xtInt ind
                                                  |>> xtList (xtG . D env) cs f}
 AppCo c1 c2        -> m { km_app    = km_app m    |> xtG (D env c1)
                                                  |>> xtG (D env c2) f }
 TransCo c1 c2      -> m { km_trans  = km_trans m  |> xtG (D env c1)
                                                  |>> xtG (D env c2) f }
 -- the provenance is not used in the map
 UnivCo _ r t1 t2   -> m { km_univ   = km_univ m   |> xtR r
                                                  |>> xtG (D env t1)
                                                  |>> xtG (D env t2) f }
 InstCo c t         -> m { km_inst   = km_inst m   |> xtG (D env c)
                                                  |>> xtG (D env t) f}
 ForAllCo v c       -> m { km_forall = km_forall m
                                                |> xtG (D (extendCME env v) c)
                                               |>> xtBndr env v f }
 CoVarCo v          -> m { km_var    = km_var m    |> xtVar env  v f }
 SymCo c            -> m { km_sym    = km_sym m    |> xtG (D env c) f }
 NthCo n c          -> m { km_nth    = km_nth m    |> xtInt n
                                                  |>> xtG (D env c) f }
 LRCo CLeft  c      -> m { km_left   = km_left m   |> xtG (D env c) f }
 LRCo CRight c      -> m { km_right  = km_right m  |> xtG (D env c) f }
 SubCo c            -> m { km_sub    = km_sub m    |> xtG (D env c) f }
 AxiomRuleCo co ts cs -> m { km_axiom_rule = km_axiom_rule m
                                                |>  alterTM (coaxrName co)
                                                |>> xtList (xtG . D env) ts
                                                |>> xtList (xtG . D env) cs f }

fdC :: (a -> b -> b) -> CoercionMapX a -> b -> b
fdC k m = foldTM (foldTM k) (km_refl m)
        . foldTM (foldTM (foldTM k)) (km_tc_app m)
        . foldTM (foldTM k) (km_app m)
        . foldTM (foldTM k) (km_forall m)
        . foldTM k (km_var m)
        . foldTM (foldTM (foldTM k)) (km_axiom m)
        . foldTM (foldTM (foldTM k)) (km_univ   m)
        . foldTM k (km_sym m)
        . foldTM (foldTM k) (km_trans m)
        . foldTM (foldTM k) (km_nth m)
        . foldTM k          (km_left m)
        . foldTM k          (km_right m)
        . foldTM (foldTM k) (km_inst m)
        . foldTM k          (km_sub m)
        . foldTM (foldTM (foldTM k)) (km_axiom_rule m)

newtype RoleMap a = RM { unRM :: (IntMap.IntMap a) }

instance TrieMap RoleMap where
  type Key RoleMap = Role
  emptyTM = RM emptyTM
  lookupTM = lkR
  alterTM = xtR
  foldTM = fdR
  mapTM = mapR

lkR :: Role -> RoleMap a -> Maybe a
lkR Nominal          = lookupTM 1 . unRM
lkR Representational = lookupTM 2 . unRM
lkR Phantom          = lookupTM 3 . unRM

xtR :: Role -> XT a -> RoleMap a -> RoleMap a
xtR Nominal          f = RM . alterTM 1 f . unRM
xtR Representational f = RM . alterTM 2 f . unRM
xtR Phantom          f = RM . alterTM 3 f . unRM

fdR :: (a -> b -> b) -> RoleMap a -> b -> b
fdR f (RM m) = foldTM f m

mapR :: (a -> b) -> RoleMap a -> RoleMap b
mapR f = RM . mapTM f . unRM

{-
************************************************************************
*                                                                      *
                   Types
*                                                                      *
************************************************************************
-}

-- | @TypeMap a@ is a map from 'Type' to @a@.  If you are a client, this
-- is the type you want.
newtype TypeMap a = TypeMap (TypeMapG a)

-- Below are some client-oriented functions which operate on 'TypeMap'.

instance TrieMap TypeMap where
    type Key TypeMap = Type
    emptyTM = TypeMap emptyTM
    lookupTM k (TypeMap m) = lookupTM (deBruijnize k) m
    alterTM k f (TypeMap m) = TypeMap (alterTM (deBruijnize k) f m)
    foldTM k (TypeMap m) = foldTM k m
    mapTM f (TypeMap m) = TypeMap (mapTM f m)

foldTypeMap :: (a -> b -> b) -> b -> TypeMap a -> b
foldTypeMap k z m = foldTM k m z

emptyTypeMap :: TypeMap a
emptyTypeMap = emptyTM

lookupTypeMap :: TypeMap a -> Type -> Maybe a
lookupTypeMap cm t = lookupTM t cm

-- Returns the type map entries that have keys starting with the given tycon.
-- This only considers saturated applications (i.e. TyConApp ones).
lookupTypeMapTyCon :: TypeMap a -> TyCon -> [a]
lookupTypeMapTyCon (TypeMap EmptyMap) _ = []
lookupTypeMapTyCon (TypeMap (SingletonMap (D _ (TyConApp tc' _)) v)) tc
    | tc' == tc = [v]
    | otherwise = []
lookupTypeMapTyCon (TypeMap SingletonMap{}) _ = []
lookupTypeMapTyCon (TypeMap (MultiMap TM { tm_tc_app = cs })) tc =
  case lookupUFM cs tc of
    Nothing -> []
    Just xs -> foldTM (:) xs []

extendTypeMap :: TypeMap a -> Type -> a -> TypeMap a
extendTypeMap m t v = alterTM t (const (Just v)) m

-- | @TypeMapG a@ is a map from @DeBruijn Type@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'TypeMap'
-- inside another 'TrieMap', this is the type you want.
type TypeMapG = GenMap TypeMapX

-- | @TypeMapX a@ is the base map from @DeBruijn Type@ to @a@, but without the
-- 'GenMap' optimization.
data TypeMapX a
  = TM { tm_var    :: VarMap a
       , tm_app    :: TypeMapG (TypeMapG a)
       , tm_fun    :: TypeMapG (TypeMapG a)
       , tm_tc_app :: NameEnv (ListMap TypeMapG a)
       , tm_forall :: TypeMapG (BndrMap a) -- See Note [Binders]
       , tm_tylit  :: TyLitMap a
       }

instance TrieMap TypeMapX where
   type Key TypeMapX = DeBruijn Type
   emptyTM  = emptyT
   lookupTM = lkT
   alterTM  = xtT
   foldTM   = fdT
   mapTM    = mapT

instance Eq (DeBruijn Type) where
  env_t@(D env t) == env_t'@(D env' t')
    | Just new_t  <- coreView t  = D env new_t == env_t'
    | Just new_t' <- coreView t' = env_t       == D env' new_t'
    | otherwise                  =
      case (t, t') of
        (TyVarTy v, TyVarTy v')
            -> case (lookupCME env v, lookupCME env' v') of
                (Just bv, Just bv') -> bv == bv'
                (Nothing, Nothing)  -> v == v'
                _ -> False
        (AppTy t1 t2, AppTy t1' t2')
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (FunTy t1 t2, FunTy t1' t2')
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (TyConApp tc tys, TyConApp tc' tys')
            -> tc == tc' && D env tys == D env' tys'
        (LitTy l, LitTy l')
            -> l == l'
        (ForAllTy tv ty, ForAllTy tv' ty')
            -> D env (tyVarKind tv)    == D env' (tyVarKind tv') &&
               D (extendCME env tv) ty == D (extendCME env' tv') ty'
        _ -> False

instance Outputable a => Outputable (TypeMap a) where
  ppr m = text "TypeMap elts" <+> ppr (foldTM (:) m [])

emptyT :: TypeMapX a
emptyT = TM { tm_var  = emptyTM
            , tm_app  = EmptyMap
            , tm_fun  = EmptyMap
            , tm_tc_app = emptyNameEnv
            , tm_forall = EmptyMap
            , tm_tylit  = emptyTyLitMap }

mapT :: (a->b) -> TypeMapX a -> TypeMapX b
mapT f (TM { tm_var  = tvar, tm_app = tapp, tm_fun = tfun
           , tm_tc_app = ttcapp, tm_forall = tforall, tm_tylit = tlit })
  = TM { tm_var    = mapTM f tvar
       , tm_app    = mapTM (mapTM f) tapp
       , tm_fun    = mapTM (mapTM f) tfun
       , tm_tc_app = mapNameEnv (mapTM f) ttcapp
       , tm_forall = mapTM (mapTM f) tforall
       , tm_tylit  = mapTM f tlit }

-----------------
lkT :: DeBruijn Type -> TypeMapX a -> Maybe a
lkT (D env ty) m = go ty m
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (TyVarTy v)       = tm_var    >.> lkVar env v
    go (AppTy t1 t2)     = tm_app    >.> lkG (D env t1) >=> lkG (D env t2)
    go (FunTy t1 t2)     = tm_fun    >.> lkG (D env t1) >=> lkG (D env t2)
    go (TyConApp tc tys) = tm_tc_app >.> lkNamed tc >=> lkList (lkG . D env) tys
    go (LitTy l)         = tm_tylit  >.> lkTyLit l
    go (ForAllTy tv ty)  = tm_forall >.> lkG (D (extendCME env tv) ty)
                                     >=> lkBndr env tv


-----------------
xtT :: DeBruijn Type -> XT a -> TypeMapX a -> TypeMapX a
xtT (D env ty) f m
  | Just ty' <- coreView ty = xtT (D env ty') f m

xtT (D env (TyVarTy v))       f m = m { tm_var    = tm_var m |> xtVar env v f }
xtT (D env (AppTy t1 t2))     f m = m { tm_app    = tm_app m |> xtG (D env t1)
                                                 |>> xtG (D env t2) f }
xtT (D env (FunTy t1 t2))     f m = m { tm_fun    = tm_fun m |> xtG (D env t1)
                                                 |>> xtG (D env t2) f }
xtT (D env (ForAllTy tv ty))  f m = m { tm_forall = tm_forall m
                                                |> xtG (D (extendCME env tv) ty)
                                                |>> xtBndr env tv f }
xtT (D env (TyConApp tc tys)) f m = m { tm_tc_app = tm_tc_app m |> xtNamed tc
                                                |>> xtList (xtG . D env) tys f }
xtT (D _   (LitTy l))         f m = m { tm_tylit  = tm_tylit m |> xtTyLit l f }

fdT :: (a -> b -> b) -> TypeMapX a -> b -> b
fdT k m = foldTM k (tm_var m)
        . foldTM (foldTM k) (tm_app m)
        . foldTM (foldTM k) (tm_fun m)
        . foldTM (foldTM k) (tm_tc_app m)
        . foldTM (foldTM k) (tm_forall m)
        . foldTyLit k (tm_tylit m)



------------------------
data TyLitMap a = TLM { tlm_number :: Map.Map Integer a
                      , tlm_string :: Map.Map FastString a
                      }

instance TrieMap TyLitMap where
   type Key TyLitMap = TyLit
   emptyTM  = emptyTyLitMap
   lookupTM = lkTyLit
   alterTM  = xtTyLit
   foldTM   = foldTyLit
   mapTM    = mapTyLit

emptyTyLitMap :: TyLitMap a
emptyTyLitMap = TLM { tlm_number = Map.empty, tlm_string = Map.empty }

mapTyLit :: (a->b) -> TyLitMap a -> TyLitMap b
mapTyLit f (TLM { tlm_number = tn, tlm_string = ts })
  = TLM { tlm_number = Map.map f tn, tlm_string = Map.map f ts }

lkTyLit :: TyLit -> TyLitMap a -> Maybe a
lkTyLit l =
  case l of
    NumTyLit n -> tlm_number >.> Map.lookup n
    StrTyLit n -> tlm_string >.> Map.lookup n

xtTyLit :: TyLit -> XT a -> TyLitMap a -> TyLitMap a
xtTyLit l f m =
  case l of
    NumTyLit n -> m { tlm_number = tlm_number m |> Map.alter f n }
    StrTyLit n -> m { tlm_string = tlm_string m |> Map.alter f n }

foldTyLit :: (a -> b -> b) -> TyLitMap a -> b -> b
foldTyLit l m = flip (Map.fold l) (tlm_string m)
              . flip (Map.fold l) (tlm_number m)

{-
************************************************************************
*                                                                      *
                   Variables
*                                                                      *
************************************************************************
-}

type BoundVar = Int  -- Bound variables are deBruijn numbered
type BoundVarMap a = IntMap.IntMap a

data CmEnv = CME { cme_next :: BoundVar
                 , cme_env  :: VarEnv BoundVar }

emptyCME :: CmEnv
emptyCME = CME { cme_next = 0, cme_env = emptyVarEnv }

extendCME :: CmEnv -> Var -> CmEnv
extendCME (CME { cme_next = bv, cme_env = env }) v
  = CME { cme_next = bv+1, cme_env = extendVarEnv env v bv }

extendCMEs :: CmEnv -> [Var] -> CmEnv
extendCMEs env vs = foldl extendCME env vs

lookupCME :: CmEnv -> Var -> Maybe BoundVar
lookupCME (CME { cme_env = env }) v = lookupVarEnv env v

-- | @DeBruijn a@ represents @a@ modulo alpha-renaming.  This is achieved
-- by equipping the value with a 'CmEnv', which tracks an on-the-fly deBruijn
-- numbering.  This allows us to define an 'Eq' instance for @DeBruijn a@, even
-- if this was not (easily) possible for @a@.  Note: we purposely don't
-- export the constructor.  Make a helper function if you find yourself
-- needing it.
data DeBruijn a = D CmEnv a

-- | Synthesizes a @DeBruijn a@ from an @a@, by assuming that there are no
-- bound binders (an empty 'CmEnv').  This is usually what you want if there
-- isn't already a 'CmEnv' in scope.
deBruijnize :: a -> DeBruijn a
deBruijnize = D emptyCME

instance Eq (DeBruijn a) => Eq (DeBruijn [a]) where
    D _   []     == D _    []       = True
    D env (x:xs) == D env' (x':xs') = D env x  == D env' x' &&
                                      D env xs == D env' xs'
    _            == _               = False

--------- Variable binders -------------

-- | A 'BndrMap' is a 'TypeMapG' which allows us to distinguish between
-- binding forms whose binders have different types.  For example,
-- if we are doing a 'TrieMap' lookup on @\(x :: Int) -> ()@, we should
-- not pick up an entry in the 'TrieMap' for @\(x :: Bool) -> ()@:
-- we can disambiguate this by matching on the type (or kind, if this
-- a binder in a type) of the binder.
type BndrMap = TypeMapG

-- Note [Binders]
-- ~~~~~~~~~~~~~~
-- We need to use 'BndrMap' for 'Coercion', 'CoreExpr' AND 'Type', since all
-- of these data types have binding forms.

lkBndr :: CmEnv -> Var -> BndrMap a -> Maybe a
lkBndr env v m = lkG (D env (varType v)) m

xtBndr :: CmEnv -> Var -> XT a -> BndrMap a -> BndrMap a
xtBndr env v f = xtG (D env (varType v)) f

--------- Variable occurrence -------------
data VarMap a = VM { vm_bvar   :: BoundVarMap a  -- Bound variable
                   , vm_fvar   :: VarEnv a }      -- Free variable

instance TrieMap VarMap where
   type Key VarMap = Var
   emptyTM  = VM { vm_bvar = IntMap.empty, vm_fvar = emptyVarEnv }
   lookupTM = lkVar emptyCME
   alterTM  = xtVar emptyCME
   foldTM   = fdVar
   mapTM    = mapVar

mapVar :: (a->b) -> VarMap a -> VarMap b
mapVar f (VM { vm_bvar = bv, vm_fvar = fv })
  = VM { vm_bvar = mapTM f bv, vm_fvar = mapVarEnv f fv }

lkVar :: CmEnv -> Var -> VarMap a -> Maybe a
lkVar env v
  | Just bv <- lookupCME env v = vm_bvar >.> lookupTM bv
  | otherwise                  = vm_fvar >.> lkFreeVar v

xtVar :: CmEnv -> Var -> XT a -> VarMap a -> VarMap a
xtVar env v f m
  | Just bv <- lookupCME env v = m { vm_bvar = vm_bvar m |> xtInt bv f }
  | otherwise                  = m { vm_fvar = vm_fvar m |> xtFreeVar v f }

fdVar :: (a -> b -> b) -> VarMap a -> b -> b
fdVar k m = foldTM k (vm_bvar m)
          . foldTM k (vm_fvar m)

lkFreeVar :: Var -> VarEnv a -> Maybe a
lkFreeVar var env = lookupVarEnv env var

xtFreeVar :: Var -> XT a -> VarEnv a -> VarEnv a
xtFreeVar v f m = alterVarEnv f m v
