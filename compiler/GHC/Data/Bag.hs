{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Bag: an unordered collection with duplicates
-}

{-# LANGUAGE ScopedTypeVariables, DeriveFunctor, TypeFamilies, MonadComprehensions #-}

module GHC.Data.Bag (
        Bag, -- abstract type

        emptyBag, unitBag, unionBags, unionManyBags,
        elem, length,
        filterBag, partitionBag, partitionBagWith,
        concatBag, catBagMaybes, foldBag,
        null, isSingletonBag, consBag, snocBag,
        listToBag, nonEmptyToBag, headMaybe,
        concatMapBag, concatMapBagPair, mapMaybeBag,
        flatMapBagM, flatMapBagPairM,
        anyBagM, filterBagM
    ) where

import GHC.Prelude

import GHC.Exts ( IsList(..) )
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad
import Control.Monad
import Data.Bool (bool)
import Data.Data
import Data.Maybe( mapMaybe )
import Data.List ( partition )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Foldable as Foldable
import qualified Data.Semigroup ( (<>) )

infixr 3 `consBag`
infixl 3 `snocBag`

data Bag a
  = EmptyBag
  | UnitBag a
  | TwoBags (Bag a) (Bag a) -- INVARIANT: neither branch is empty
  | ListBag (NonEmpty a)
  deriving (Functor)

emptyBag :: Bag a
emptyBag = EmptyBag

unitBag :: a -> Bag a
unitBag  = UnitBag

unionManyBags :: [Bag a] -> Bag a
unionManyBags = foldr unionBags EmptyBag

-- This one is a bit stricter! The bag will get completely evaluated.

unionBags :: Bag a -> Bag a -> Bag a
unionBags EmptyBag b = b
unionBags b EmptyBag = b
unionBags b1 b2      = TwoBags b1 b2

consBag :: a -> Bag a -> Bag a
snocBag :: Bag a -> a -> Bag a

consBag elt bag = (unitBag elt) `unionBags` bag
snocBag bag elt = bag `unionBags` (unitBag elt)

isSingletonBag :: Bag a -> Bool
isSingletonBag EmptyBag      = False
isSingletonBag (UnitBag _)   = True
isSingletonBag (TwoBags _ _) = False          -- Neither is empty
isSingletonBag (ListBag (_:|xs))  = null xs

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag _    EmptyBag = EmptyBag
filterBag pred b@(UnitBag val) = if pred val then b else EmptyBag
filterBag pred (TwoBags b1 b2) = sat1 `unionBags` sat2
    where sat1 = filterBag pred b1
          sat2 = filterBag pred b2
filterBag pred (ListBag vs)    = listToBag (filter pred (toList vs))

filterBagM :: Applicative m => (a -> m Bool) -> Bag a -> m (Bag a)
filterBagM _    EmptyBag = pure EmptyBag
filterBagM pred b@(UnitBag val) = bool EmptyBag b <$> pred val
filterBagM pred (TwoBags b1 b2) = unionBags <$> filterBagM pred b1 <*> filterBagM pred b2
filterBagM pred (ListBag vs) = listToBag <$> filterM pred (toList vs)

anyBagM :: Monad m => (a -> m Bool) -> Bag a -> m Bool
anyBagM _ EmptyBag        = return False
anyBagM p (UnitBag v)     = p v
anyBagM p (TwoBags b1 b2) = anyBagM p b1 <||> anyBagM p b2
anyBagM p (ListBag xs)    = anyM p (toList xs)

concatBag :: Bag (Bag a) -> Bag a
concatBag = foldr unionBags emptyBag

catBagMaybes :: Bag (Maybe a) -> Bag a
catBagMaybes = foldr (maybe id consBag) emptyBag

partitionBag :: (a -> Bool) -> Bag a -> (Bag a {- Satisfy predicate -},
                                         Bag a {- Don't -})
partitionBag _    EmptyBag = (EmptyBag, EmptyBag)
partitionBag pred b@(UnitBag val)
    = if pred val then (b, EmptyBag) else (EmptyBag, b)
partitionBag pred (TwoBags b1 b2)
    = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
  where (sat1, fail1) = partitionBag pred b1
        (sat2, fail2) = partitionBag pred b2
partitionBag pred (ListBag vs) = (listToBag sats, listToBag fails)
  where (sats, fails) = partition pred $ toList vs


partitionBagWith :: (a -> Either b c) -> Bag a
                    -> (Bag b {- Left  -},
                        Bag c {- Right -})
partitionBagWith _    EmptyBag = (EmptyBag, EmptyBag)
partitionBagWith pred (UnitBag val)
    = case pred val of
         Left a  -> (UnitBag a, EmptyBag)
         Right b -> (EmptyBag, UnitBag b)
partitionBagWith pred (TwoBags b1 b2)
    = (sat1 `unionBags` sat2, fail1 `unionBags` fail2)
  where (sat1, fail1) = partitionBagWith pred b1
        (sat2, fail2) = partitionBagWith pred b2
partitionBagWith pred (ListBag vs) = (listToBag sats, listToBag fails)
  where (sats, fails) = partitionWith pred $ toList vs

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

mapMaybeBag :: (a -> Maybe b) -> Bag a -> Bag b
mapMaybeBag _ EmptyBag        = EmptyBag
mapMaybeBag f (UnitBag x)     = case f x of
                                  Nothing -> EmptyBag
                                  Just y  -> UnitBag y
mapMaybeBag f (TwoBags b1 b2) = unionBags (mapMaybeBag f b1) (mapMaybeBag f b2)
mapMaybeBag f (ListBag xs)    = listToBag $ mapMaybe f $ toList xs

flatMapBagM :: Monad m => (a -> m (Bag b)) -> Bag a -> m (Bag b)
flatMapBagM _ EmptyBag        = return EmptyBag
flatMapBagM f (UnitBag x)     = f x
flatMapBagM f (TwoBags b1 b2) = do r1 <- flatMapBagM f b1
                                   r2 <- flatMapBagM f b2
                                   return (r1 `unionBags` r2)
flatMapBagM f (ListBag    xs) = foldrM k EmptyBag xs
  where
    k x b2 = do { b1 <- f x; return (b1 `unionBags` b2) }

flatMapBagPairM :: Monad m => (a -> m (Bag b, Bag c)) -> Bag a -> m (Bag b, Bag c)
flatMapBagPairM _ EmptyBag        = return (EmptyBag, EmptyBag)
flatMapBagPairM f (UnitBag x)     = f x
flatMapBagPairM f (TwoBags b1 b2) = do (r1,s1) <- flatMapBagPairM f b1
                                       (r2,s2) <- flatMapBagPairM f b2
                                       return (r1 `unionBags` r2, s1 `unionBags` s2)
flatMapBagPairM f (ListBag    xs) = foldrM k (EmptyBag, EmptyBag) xs
  where
    k x (r2,s2) = do { (r1,s1) <- f x
                     ; return (r1 `unionBags` r2, s1 `unionBags` s2) }

listToBag :: [a] -> Bag a
listToBag [] = EmptyBag
listToBag [x] = UnitBag x
listToBag (x:xs) = ListBag (x:|xs)

nonEmptyToBag :: NonEmpty a -> Bag a
nonEmptyToBag (x :| []) = UnitBag x
nonEmptyToBag (x :| xs) = ListBag (x :| xs)

headMaybe :: Bag a -> Maybe a
headMaybe EmptyBag = Nothing
headMaybe (UnitBag v) = Just v
headMaybe (TwoBags b1 _) = headMaybe b1
headMaybe (ListBag (a:|_)) = Just a

instance (Outputable a) => Outputable (Bag a) where
    ppr bag = braces (pprWithCommas ppr (toList bag))

instance Data a => Data (Bag a) where
  gfoldl k z b = z listToBag `k` toList b -- traverse abstract type abstractly
  toConstr _   = abstractConstr $ "Bag("++show (typeOf (undefined::a))++")"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Bag"
  dataCast1 x  = gcast1 x

instance Foldable.Foldable Bag where
  foldr _ z EmptyBag        = z
  foldr k z (UnitBag x)     = k x z
  foldr k z (TwoBags b1 b2) = foldr k (foldr k z b2) b1
  foldr k z (ListBag xs)    = foldr k z xs

  foldl _ z EmptyBag        = z
  foldl k z (UnitBag x)     = k z x
  foldl k z (TwoBags b1 b2) = foldl k (foldl k z b1) b2
  foldl k z (ListBag xs)    = foldl k z xs

  foldl' _ z EmptyBag        = z
  foldl' k z (UnitBag x)     = k z x
  foldl' k z (TwoBags b1 b2) = let r1 = foldl' k z b1 in seq r1 $ foldl' k r1 b2
  foldl' k z (ListBag xs)    = foldl' k z xs

  null = \ case
      EmptyBag -> True
      _ -> False

instance Traversable Bag where
  traverse _ EmptyBag        = pure EmptyBag
  traverse f (UnitBag x)     = UnitBag <$> f x
  traverse f (TwoBags b1 b2) = TwoBags <$> traverse f b1 <*> traverse f b2
  traverse f (ListBag xs)    = ListBag <$> traverse f xs

instance IsList (Bag a) where
  type Item (Bag a) = a
  fromList = listToBag
  toList   = toList

instance Semigroup (Bag a) where
  (<>) = unionBags

instance Monoid (Bag a) where
  mempty = emptyBag
