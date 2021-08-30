{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


-}
{-# LANGUAGE BangPatterns #-}

-- | Provide trees (of instructions), so that lists of instructions can be
-- appended in linear time.
module GHC.Data.OrdList (
        OrdList,
        nilOL, unitOL, appOL, consOL, snocOL, concatOL, lastOL,
        headOL,
        toOL, reverseOL, fromOLReverse,
        strictlyEqOL, strictlyOrdOL
) where

import GHC.Prelude
import Data.Foldable

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
  deriving (Foldable, Functor, Traversable)

instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (toList ol)  -- Convert to list and print that

instance Semigroup (OrdList a) where
  (<>) = appOL

instance Monoid (OrdList a) where
  mempty = nilOL
  mappend = (Semigroup.<>)
  mconcat = concatOL

nilOL    :: OrdList a

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a
headOL   :: OrdList a   -> a
lastOL   :: OrdList a   -> a

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
concatOL aas = foldr appOL None aas

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

None  `appOL` b     = b
a     `appOL` None  = a
One a `appOL` b     = Cons a b
a     `appOL` One b = Snoc a b
a     `appOL` b     = Two a b

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
