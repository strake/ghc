{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Bag: an unordered collection with duplicates
-}

{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Data.Bag (
        Bag, -- abstract type

        emptyBag, unitBag, unionBags, unionManyBags,
        concatBag, foldBag,
        isEmptyBag, isSingletonBag, consBag, snocBag,
        listToBag,
        concatMapBag, concatMapBagPair,
        flatMapBagM, flatMapBagPairM,
    ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Misc

import GHC.Utils.Monad
import Data.Data
import Data.Foldable ( toList )
import Data.List.NonEmpty ( NonEmpty (..) )

infixr 3 `consBag`
infixl 3 `snocBag`

data Bag a
  = EmptyBag
  | UnitBag a
  | TwoBags (Bag a) (Bag a) -- INVARIANT: neither branch is empty
  | ListBag (NonEmpty a)    -- INVARIANT: the list is non-empty
  deriving (Foldable, Functor, Traversable)

emptyBag :: Bag a
emptyBag = EmptyBag

unitBag :: a -> Bag a
unitBag  = UnitBag

unionManyBags :: [Bag a] -> Bag a
unionManyBags xs = foldr unionBags EmptyBag xs

-- This one is a bit stricter! The bag will get completely evaluated.

unionBags :: Bag a -> Bag a -> Bag a
unionBags EmptyBag b = b
unionBags b EmptyBag = b
unionBags b1 b2      = TwoBags b1 b2

consBag :: a -> Bag a -> Bag a
snocBag :: Bag a -> a -> Bag a

consBag elt bag = unitBag elt `unionBags` bag
snocBag bag elt = bag `unionBags` unitBag elt

isEmptyBag :: Bag a -> Bool
isEmptyBag EmptyBag = True
isEmptyBag _        = False -- NB invariants

isSingletonBag :: Bag a -> Bool
isSingletonBag EmptyBag      = False
isSingletonBag (UnitBag _)   = True
isSingletonBag (TwoBags _ _) = False          -- Neither is empty
isSingletonBag (ListBag (_:|xs))  = null xs

instance Filtrable Bag where
    filter _    EmptyBag = EmptyBag
    filter pred b@(UnitBag val) = bool EmptyBag b $ pred val
    filter pred (TwoBags b1 b2) = sat1 `unionBags` sat2
        where sat1 = filter pred b1
              sat2 = filter pred b2
    filter pred (ListBag vs)    = listToBag (filter pred (toList vs))

    catMaybes = foldr (maybe id consBag) emptyBag

    mapMaybe f = \ case
        EmptyBag        -> EmptyBag
        UnitBag x       -> case f x of
            Nothing -> EmptyBag
            Just y  -> UnitBag y
        TwoBags b1 b2   -> unionBags (mapMaybe f b1) (mapMaybe f b2)
        ListBag xs      -> listToBag (mapMaybe f (toList xs))

    filterA pred = \ case
        EmptyBag -> pure EmptyBag
        b@(UnitBag val) -> bool EmptyBag b <$> pred val
        TwoBags b1 b2 -> unionBags <$> filterA pred b1 <*> filterA pred b2
        ListBag vs -> listToBag <$> filterA pred (toList vs)

    partition pred = \ case
        EmptyBag -> (EmptyBag, EmptyBag)
        b@(UnitBag val)
          | pred val -> (b, EmptyBag)
          | otherwise -> (EmptyBag, b)
        TwoBags b1 b2 -> (unionBags pass1 pass2, unionBags fail1 fail2)
          where
            (pass1, fail1) = partition pred b1
            (pass2, fail2) = partition pred b2
        ListBag vs -> (listToBag passes, listToBag fails)
          where (passes, fails) = partition pred (toList vs)

    mapEither pred = \ case
        EmptyBag -> (EmptyBag, EmptyBag)
        UnitBag val -> case pred val of
            Left a  -> (UnitBag a, EmptyBag)
            Right b -> (EmptyBag, UnitBag b)
        TwoBags b1 b2 -> (unionBags pass1 pass2, unionBags fail1 fail2)
          where
            (pass1, fail1) = mapEither pred b1
            (pass2, fail2) = mapEither pred b2
        ListBag vs -> (listToBag passes, listToBag fails)
          where (passes, fails) = mapEither pred (toList vs)

concatBag :: Bag (Bag a) -> Bag a
concatBag = foldr unionBags emptyBag


foldBag :: (r -> r -> r) -- Replace TwoBags with this; should be associative
        -> (a -> r)      -- Replace UnitBag with this
        -> r             -- Replace EmptyBag with this
        -> Bag a
        -> r

{- Standard definition
foldBag t u e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x
foldBag t u e (TwoBags b1 b2) = (foldBag t u e b1) `t` (foldBag t u e b2)
foldBag t u e (ListBag xs)    = foldr (t.u) e xs
-}

-- More tail-recursive definition, exploiting associativity of "t"
foldBag _ _ e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x `t` e
foldBag t u e (TwoBags b1 b2) = foldBag t u (foldBag t u e b2) b1
foldBag t u e (ListBag xs)    = foldr (t.u) e xs

-- XXX: Why not Monad?
concatMapBag :: (a -> Bag b) -> Bag a -> Bag b
concatMapBag _ EmptyBag        = EmptyBag
concatMapBag f (UnitBag x)     = f x
concatMapBag f (TwoBags b1 b2) = unionBags (concatMapBag f b1) (concatMapBag f b2)
concatMapBag f (ListBag xs)    = foldr (unionBags . f) emptyBag xs

concatMapBagPair :: (a -> (Bag b, Bag c)) -> Bag a -> (Bag b, Bag c)
concatMapBagPair _ EmptyBag        = (EmptyBag, EmptyBag)
concatMapBagPair f (UnitBag x)     = f x
concatMapBagPair f (TwoBags b1 b2) = (unionBags r1 r2, unionBags s1 s2)
  where
    (r1, s1) = concatMapBagPair f b1
    (r2, s2) = concatMapBagPair f b2
concatMapBagPair f (ListBag xs)    = foldr go (emptyBag, emptyBag) xs
  where
    go a (s1, s2) = (unionBags r1 s1, unionBags r2 s2)
      where
        (r1, r2) = f a

flatMapBagM :: Monad m => (a -> m (Bag b)) -> Bag a -> m (Bag b)
flatMapBagM _ EmptyBag        = pure EmptyBag
flatMapBagM f (UnitBag x)     = f x
flatMapBagM f (TwoBags b1 b2) = unionBags <$> flatMapBagM f b1 <*> flatMapBagM f b2
flatMapBagM f (ListBag    xs) = foldrM k EmptyBag xs
  where
    k x b2 = [ unionBags b1 b2 | b1 <- f x ]

flatMapBagPairM :: Monad m => (a -> m (Bag b, Bag c)) -> Bag a -> m (Bag b, Bag c)
flatMapBagPairM _ EmptyBag        = pure (EmptyBag, EmptyBag)
flatMapBagPairM f (UnitBag x)     = f x
flatMapBagPairM f (TwoBags b1 b2) =
  [ (r1 `unionBags` r2, s1 `unionBags` s2)
  | (r1,s1) <- flatMapBagPairM f b1
  , (r2,s2) <- flatMapBagPairM f b2 ]
flatMapBagPairM f (ListBag    xs) = foldrM k (EmptyBag, EmptyBag) xs
  where
    k x (r2,s2) = [ (r1 `unionBags` r2, s1 `unionBags` s2) | (r1,s1) <- f x ]

listToBag :: [a] -> Bag a
listToBag [] = EmptyBag
listToBag [x] = UnitBag x
listToBag (v:vs) = ListBag (v:|vs)

instance (Outputable a) => Outputable (Bag a) where
    ppr bag = braces (pprWithCommas ppr (toList bag))

instance Data a => Data (Bag a) where
  gfoldl k z b = z listToBag `k` toList b -- traverse abstract type abstractly
  toConstr _   = abstractConstr $ "Bag("++show (typeOf (undefined::a))++")"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Bag"
  dataCast1 x  = gcast1 x
