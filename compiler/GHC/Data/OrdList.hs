{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Provide trees (of instructions), so that lists of instructions can be
-- appended in linear time.
module GHC.Data.OrdList (
        OrdList, pattern NilOL, pattern ConsOL, pattern SnocOL,
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL, lastOL,
        headOL,
        mapOL, mapOL', fromOL, toOL, foldrOL, foldlOL, reverseOL, fromOLReverse,
        strictlyEqOL, strictlyOrdOL
) where

import GHC.Prelude
import Data.Foldable

import GHC.Utils.Misc (strictMap)
import GHC.Utils.Outputable
import GHC.Utils.Panic

import qualified Data.Semigroup as Semigroup

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = None
  | One a
  | Many [a]          -- Invariant: non-empty
  | Cons a (OrdList a)
  | Snoc (OrdList a) a
  | Two (OrdList a) -- Invariant: non-empty
        (OrdList a) -- Invariant: non-empty
  deriving (Functor)

instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (fromOL ol)  -- Convert to list and print that

instance Semigroup (OrdList a) where
  (<>) = appOL

instance Monoid (OrdList a) where
  mempty = nilOL
  mappend = (Semigroup.<>)
  mconcat = concatOL

instance Foldable OrdList where
  foldr   = foldrOL
  foldl'  = foldlOL
  toList  = fromOL
  null    = isNilOL
  length  = lengthOL

instance Traversable OrdList where
  traverse f xs = toOL <$> traverse f (fromOL xs)

nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a
headOL   :: OrdList a   -> a
lastOL   :: OrdList a   -> a
lengthOL :: OrdList a   -> Int

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
concatOL aas = foldr appOL None aas

pattern NilOL :: OrdList a
pattern NilOL <- (isNilOL -> True) where
  NilOL = None

-- | An unboxed 'Maybe' type with two unboxed fields in the 'Just' case.
-- Useful for defining 'viewCons' and 'viewSnoc' without overhead.
type VMaybe a b = (# (# a, b #) | (# #) #)
pattern VJust :: a -> b -> VMaybe a b
pattern VJust a b = (# (# a, b #) | #)
pattern VNothing :: VMaybe a b
pattern VNothing = (# | (# #) #)
{-# COMPLETE VJust, VNothing #-}

pattern ConsOL :: a -> OrdList a -> OrdList a
pattern ConsOL x xs <- (viewCons -> VJust x xs) where
  ConsOL x xs = consOL x xs
{-# COMPLETE NilOL, ConsOL #-}
viewCons :: OrdList a -> VMaybe a (OrdList a)
viewCons (One a)       = VJust a NilOL
viewCons (Cons a as) = VJust a as
viewCons (Snoc as a) = case viewCons as of
  VJust a' as' -> VJust a' (Snoc as' a)
  VNothing     -> VJust a NilOL
viewCons (Two as1 as2) = case viewCons as1 of
  VJust a' as1' -> VJust a' (Two as1' as2)
  VNothing      -> viewCons as2
viewCons _ = VNothing

pattern SnocOL :: OrdList a -> a -> OrdList a
pattern SnocOL xs x <- (viewSnoc -> VJust xs x) where
  SnocOL xs x = snocOL xs x
{-# COMPLETE NilOL, SnocOL #-}
viewSnoc :: OrdList a -> VMaybe (OrdList a) a
viewSnoc (One a)       = VJust NilOL a
viewSnoc (Many (reverse -> a:as)) = VJust (Many (reverse as)) a
viewSnoc (Snoc as a) = VJust as a
viewSnoc (Cons a as) = case viewSnoc as of
  VJust as' a' -> VJust (Cons a as') a'
  VNothing     -> VJust NilOL a
viewSnoc (Two as1 as2) = case viewSnoc as2 of
  VJust as2' a' -> VJust (Two as1 as2') a'
  VNothing      -> viewSnoc as1
viewSnoc _ = VNothing

headOL None        = panic "headOL"
headOL (One a)     = a
headOL (Many as)   = head as
headOL (Cons a _)  = a
headOL (Snoc as _) = headOL as
headOL (Two as _)  = headOL as

lastOL None        = panic "lastOL"
lastOL (One a)     = a
lastOL (Many as)   = last as
lastOL (Cons _ as) = lastOL as
lastOL (Snoc _ a)  = a
lastOL (Two _ as)  = lastOL as

lengthOL None        = 0
lengthOL (One _)     = 1
lengthOL (Many as)   = length as
lengthOL (Cons _ as) = 1 + length as
lengthOL (Snoc as _) = 1 + length as
lengthOL (Two as bs) = length as + length bs

isNilOL None = True
isNilOL _    = False

None  `appOL` b     = b
a     `appOL` None  = a
One a `appOL` b     = Cons a b
a     `appOL` One b = Snoc a b
a     `appOL` b     = Two a b

fromOL :: OrdList a -> [a]
fromOL a = go a []
  where go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = a : go b acc
        go (Snoc a b) acc = go a (b:acc)
        go (Two a b)  acc = go a (go b acc)
        go (Many xs)  acc = xs ++ acc

fromOLReverse :: OrdList a -> [a]
fromOLReverse a = go a []
        -- acc is already in reverse order
  where go :: OrdList a -> [a] -> [a]
        go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = go b (a : acc)
        go (Snoc a b) acc = b : go a acc
        go (Two a b)  acc = go b (go a acc)
        go (Many xs)  acc = reverse xs ++ acc

mapOL :: (a -> b) -> OrdList a -> OrdList b
mapOL = fmap

mapOL' :: (a->b) -> OrdList a -> OrdList b
mapOL' _ None        = None
mapOL' f (One x)     = One $! f x
mapOL' f (Cons x xs) = let !x1 = f x
                           !xs1 = mapOL' f xs
                       in Cons x1 xs1
mapOL' f (Snoc xs x) = let !x1 = f x
                           !xs1 = mapOL' f xs
                       in Snoc xs1 x1
mapOL' f (Two b1 b2) = let !b1' = mapOL' f b1
                           !b2' = mapOL' f b2
                       in Two b1' b2'
mapOL' f (Many xs)   = Many $! strictMap f xs

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL = foldr

-- | Strict left fold.
foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL = foldl'

toOL :: [a] -> OrdList a
toOL [] = None
toOL [x] = One x
toOL xs = Many xs

reverseOL :: OrdList a -> OrdList a
reverseOL None = None
reverseOL (One x) = One x
reverseOL (Cons a b) = Snoc (reverseOL b) a
reverseOL (Snoc a b) = Cons b (reverseOL a)
reverseOL (Two a b)  = Two (reverseOL b) (reverseOL a)
reverseOL (Many xs)  = Many (reverse xs)

-- | Compare not only the values but also the structure of two lists
strictlyEqOL :: Eq a => OrdList a   -> OrdList a -> Bool
strictlyEqOL None         None       = True
strictlyEqOL (One x)     (One y)     = x == y
strictlyEqOL (Cons a as) (Cons b bs) = a == b && as `strictlyEqOL` bs
strictlyEqOL (Snoc as a) (Snoc bs b) = a == b && as `strictlyEqOL` bs
strictlyEqOL (Two a1 a2) (Two b1 b2) = a1 `strictlyEqOL` b1 && a2 `strictlyEqOL` b2
strictlyEqOL (Many as)   (Many bs)   = as == bs
strictlyEqOL _            _          = False

-- | Compare not only the values but also the structure of two lists
strictlyOrdOL :: Ord a => OrdList a   -> OrdList a -> Ordering
strictlyOrdOL None         None       = EQ
strictlyOrdOL None         _          = LT
strictlyOrdOL (One x)     (One y)     = compare x y
strictlyOrdOL (One _)      _          = LT
strictlyOrdOL (Cons a as) (Cons b bs) =
  compare a b `mappend` strictlyOrdOL as bs
strictlyOrdOL (Cons _ _)   _          = LT
strictlyOrdOL (Snoc as a) (Snoc bs b) =
  compare a b `mappend` strictlyOrdOL as bs
strictlyOrdOL (Snoc _ _)   _          = LT
strictlyOrdOL (Two a1 a2) (Two b1 b2) =
  (strictlyOrdOL a1 b1) `mappend` (strictlyOrdOL a2 b2)
strictlyOrdOL (Two _ _)    _          = LT
strictlyOrdOL (Many as)   (Many bs)   = compare as bs
strictlyOrdOL (Many _ )   _           = GT
