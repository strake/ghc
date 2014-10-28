\begin{code}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.List
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The List data type and its operations
--
-----------------------------------------------------------------------------

module GHC.List (
   -- [] (..),          -- built-in syntax; can't be used in export list

   map, (++), filter, concat,
   head, last, tail, init, uncons, null, length, (!!),
   foldl, scanl, scanl1, scanl', foldr, foldr1, scanr, scanr1,
   iterate, repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break,
   reverse, and, or,
   any, all, elem, notElem, lookup,
   concatMap,
   zip, zip3, zipWith, zipWith3, unzip, unzip3,
   errorEmptyList,

#ifndef USE_REPORT_PRELUDE
   -- non-standard, but hidden when creating the Prelude
   -- export list.
   takeUInt_append
#endif

 ) where

import Data.Maybe
import GHC.Base

infixl 9  !!
infix  4 `elem`, `notElem`
\end{code}

%*********************************************************
%*                                                      *
\subsection{List-manipulation functions}
%*                                                      *
%*********************************************************

\begin{code}
-- | Extract the first element of a list, which must be non-empty.
head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead
{-# NOINLINE [1] head #-}

badHead :: a
badHead = errorEmptyList "head"

-- This rule is useful in cases like
--      head [y | (x,y) <- ps, x==t]
{-# RULES
"head/build"    forall (g::forall b.(a->b->b)->b->b) .
                head (build g) = g (\x _ -> x) badHead
"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) .
                head (augment g xs) = g (\x _ -> x) (head xs)
 #-}

-- | Decompose a list into its head and tail. If the list is empty,
-- returns 'Nothing'. If the list is non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
--
-- /Since: 4.8.0.0/
uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)

-- | Extract the elements after the head of a list, which must be non-empty.
tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  errorEmptyList "tail"

-- | Extract the last element of a list, which must be finite and non-empty.
last                    :: [a] -> a
#ifdef USE_REPORT_PRELUDE
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  errorEmptyList "last"
#else
-- use foldl to allow fusion
last = foldl (\_ x -> x) (errorEmptyList "last")
#endif

-- | Return all the elements of a list except the last one.
-- The list must be non-empty.
init                    :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  errorEmptyList "init"
#else
-- eliminate repeated cases
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs
#endif

-- | Test whether a list is empty.
null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- | /O(n)/. 'length' returns the length of a finite list as an 'Int'.
-- It is an instance of the more general 'Data.List.genericLength',
-- the result type of which may be any kind of number.
{-# NOINLINE [1] length #-}
length                  :: [a] -> Int
length l                =  lenAcc l 0#

lenAcc :: [a] -> Int# -> Int
lenAcc []     a# = I# a#
lenAcc (_:xs) a# = lenAcc xs (a# +# 1#)

incLen :: a -> (Int# -> Int) -> Int# -> Int
incLen _ g x = g (x +# 1#)

-- These rules make length into a good consumer
-- Note that we use a higher-order-style use of foldr, so that
-- the accumulating parameter can be evaluated strictly
-- See Trac #876 for what goes wrong otherwise
{-# RULES
"length"     [~1] forall xs. length xs = foldr incLen I# xs 0#
"lengthList" [1]  foldr incLen I# = lenAcc
 #-}

-- | 'filter', applied to a predicate and a list, returns the list of
-- those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]

{-# NOINLINE [1] filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs

{-# NOINLINE [0] filterFB #-}
filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
filterFB c p x r | p x       = x `c` r
                 | otherwise = r

{-# RULES
"filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
"filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
 #-}

-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
--     filterFB (filterFB c p) q a b
--   = if q a then filterFB c p a b else b
--   = if q a then (if p a then c a b else b) else b
--   = if q a && p a then c a b else b
--   = filterFB c (\x -> q x && p x) a b
-- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- gave rise to a live bug report.  SLPJ.


-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a list, reduces the list
-- using the binary operator, from left to right:
--
-- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
--
-- The list must be finite.

-- We write foldl as a non-recursive thing, so that it
-- can be inlined, and then (often) strictness-analysed,
-- and hence the classic space leak on foldl (+) 0 xs

foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
{-# INLINE foldl #-}
foldl k z0 xs = foldr (\(v::a) (fn::b->b) (z::b) -> fn (k z v)) (id :: b -> b) xs z0
-- Implementing foldl via foldr is only a good idea if the compiler can optimize
-- the resulting code (eta-expand the recursive "go"), so this needs -fcall-arity!
-- Also see #7994

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.

-- This peculiar arrangement is necessary to prevent scanl being rewritten in
-- its own right-hand side.
{-# NOINLINE [1] scanl #-}
scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl                   = scanlGo
  where
    scanlGo           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo f q ls    = q : (case ls of
                               []   -> []
                               x:xs -> scanlGo f (f q x) xs)

-- Note [scanl rewrite rules]
{-# RULES
"scanl"  [~1] forall f a bs . scanl f a bs =
  build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)
"scanlList" [1] forall f (a::a) bs .
    foldr (scanlFB f (:)) (constScanl []) bs a = tail (scanl f a bs)
 #-}

{-# INLINE [0] scanlFB #-}
scanlFB :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB f c = \b g x -> let b' = f x b in b' `c` g b'

{-# INLINE [0] constScanl #-}
constScanl :: a -> b -> a
constScanl = const


-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []

-- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
-- This peculiar form is needed to prevent scanl' from being rewritten
-- in its own right hand side.
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f q ls    = q `seq` q : (case ls of
                                []   -> []
                                x:xs -> scanlGo' f (f q x) xs)

-- Note [scanl rewrite rules]
{-# RULES
"scanl'"  [~1] forall f a bs . scanl' f a bs =
  build (\c n -> a `c` foldr (scanlFB' f c) (flipSeqScanl' n) bs a)
"scanlList'" [1] forall f a bs .
    foldr (scanlFB' f (:)) (flipSeqScanl' []) bs a = tail (scanl' f a bs)
 #-}

{-# INLINE [0] scanlFB' #-}
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g x -> let b' = f x b in b' `seq` b' `c` g b'

{-# INLINE [0] flipSeqScanl' #-}
flipSeqScanl' :: a -> b -> a
flipSeqScanl' = flip seq

{-
Note [scanl rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~

In most cases, when we rewrite a form to one that can fuse, we try to rewrite it
back to the original form if it does not fuse. For scanl, we do something a
little different. In particular, we rewrite

scanl f a bs

to

build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)

When build is inlined, this becomes

a : foldr (scanlFB f (:)) (constScanl []) bs a

To rewrite this form back to scanl, we would need a rule that looked like

forall f a bs. a : foldr (scanlFB f (:)) (constScanl []) bs a = scanl f a bs

The problem with this rule is that it has (:) at its head. This would have the
effect of changing the way the inliner looks at (:), not only here but
everywhere.  In most cases, this makes no difference, but in some cases it
causes it to come to a different decision about whether to inline something.
Based on nofib benchmarks, this is bad for performance. Therefore, we instead
match on everything past the :, which is just the tail of scanl.
-}

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty lists.

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  errorEmptyList "foldr1"

-- | 'scanr' is the right-to-left dual of 'scanl'.
-- Note that
--
-- > head (scanr f z xs) == foldr f z xs.

{-# NOINLINE [1] scanr #-}
scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs

{-# INLINE [0] strictUncurryScanr #-}
strictUncurryScanr :: (a -> b -> c) -> (a, b) -> c
strictUncurryScanr f pair = case pair of
                              (x, y) -> f x y

{-# INLINE [0] scanrFB #-}
scanrFB :: (a -> b -> b) -> (b -> c -> c) -> a -> (b, c) -> (b, c)
scanrFB f c = \x (r, est) -> (f x r, r `c` est)

{-# RULES
"scanr" [~1] forall f q0 ls . scanr f q0 ls =
  build (\c n -> strictUncurryScanr c (foldr (scanrFB f c) (q0,n) ls))
"scanrList" [1] forall f q0 ls .
               strictUncurryScanr (:) (foldr (scanrFB f (:)) (q0,[]) ls) =
                 scanr f q0 ls
 #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []             =  []
scanr1 _ [x]            =  [x]
scanr1 f (x:xs)         =  f x q : qs
                           where qs@(q:_) = scanr1 f xs

-- | 'iterate' @f x@ returns an infinite list of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]

{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

{-# NOINLINE [0] iterateFB #-}
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x0 = go x0
  where go x = x `c` go (f x)

{-# RULES
"iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
"iterateFB"  [1]                iterateFB (:) = iterate
 #-}


-- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

{-# INLINE [0] repeatFB #-}     -- ditto
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs


{-# RULES
"repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
"repeatFB"  [1]  repeatFB (:)       = repeat
 #-}

-- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- every element.
-- It is an instance of the more general 'Data.List.genericReplicate',
-- in which @n@ may be of any integral type.
{-# INLINE replicate #-}
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

-- | 'cycle' ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle                   :: [a] -> [a]
cycle []                = error "Prelude.cycle: empty list"
cycle xs                = xs' where xs' = xs ++ xs'

-- | 'takeWhile', applied to a predicate @p@ and a list @xs@, returns the
-- longest prefix (possibly empty) of @xs@ of elements that satisfy @p@:
--
-- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
-- > takeWhile (< 9) [1,2,3] == [1,2,3]
-- > takeWhile (< 0) [1,2,3] == []
--

{-# NOINLINE [1] takeWhile #-}
takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []

{-# INLINE [0] takeWhileFB #-}
takeWhileFB :: (a -> Bool) -> (a -> b -> b) -> b -> a -> b -> b
takeWhileFB p c n = \x r -> if p x then x `c` r else n

-- The takeWhileFB rule is similar to the filterFB rule. It works like this:
-- takeWhileFB q (takeWhileFB p c n) n =
-- \x r -> if q x then (takeWhileFB p c n) x r else n =
-- \x r -> if q x then (\x' r' -> if p x' then x' `c` r' else n) x r else n =
-- \x r -> if q x then (if p x then x `c` r else n) else n =
-- \x r -> if q x && p x then x `c` r else n =
-- takeWhileFB (\x -> q x && p x) c n
{-# RULES
"takeWhile"     [~1] forall p xs. takeWhile p xs =
                                build (\c n -> foldr (takeWhileFB p c n) n xs)
"takeWhileList" [1]  forall p.    foldr (takeWhileFB p (:) []) [] = takeWhile p
"takeWhileFB"        forall c n p q. takeWhileFB q (takeWhileFB p c n) n =
                        takeWhileFB (\x -> q x && p x) c n
 #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@:
--
-- > dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
-- > dropWhile (< 9) [1,2,3] == []
-- > dropWhile (< 0) [1,2,3] == [1,2,3]
--

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@:
--
-- > take 5 "Hello World!" == "Hello"
-- > take 3 [1,2,3,4,5] == [1,2,3]
-- > take 3 [1,2] == [1,2]
-- > take 3 [] == []
-- > take (-1) [1,2] == []
-- > take 0 [1,2] == []
--
-- It is an instance of the more general 'Data.List.genericTake',
-- in which @n@ may be of any integral type.
take                   :: Int -> [a] -> [a]

-- | 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
--
-- > drop 6 "Hello World!" == "World!"
-- > drop 3 [1,2,3,4,5] == [4,5]
-- > drop 3 [1,2] == []
-- > drop 3 [] == []
-- > drop (-1) [1,2] == [1,2]
-- > drop 0 [1,2] == [1,2]
--
-- It is an instance of the more general 'Data.List.genericDrop',
-- in which @n@ may be of any integral type.
drop                   :: Int -> [a] -> [a]

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.
splitAt                :: Int -> [a] -> ([a],[a])

#ifdef USE_REPORT_PRELUDE
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs

drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs

splitAt n xs           =  (take n xs, drop n xs)

#else /* hack away */
{-# RULES
"take"     [~1] forall n xs . take n xs = takeFoldr n xs
"takeList"  [1] forall n xs . foldr (takeFB (:) []) (takeConst []) xs n = takeUInt n xs
 #-}

{-# INLINE takeFoldr #-}
takeFoldr :: Int -> [a] -> [a]
takeFoldr (I# n#) xs
  = build (\c nil -> if isTrue# (n# <=# 0#) then nil else
                     foldr (takeFB c nil) (takeConst nil) xs n#)

{-# NOINLINE [0] takeConst #-}
-- just a version of const that doesn't get inlined too early, so we
-- can spot it in rules.  Also we need a type sig due to the unboxed Int#.
takeConst :: a -> Int# -> a
takeConst x _ = x

{-# INLINE [0] takeFB #-}
takeFB :: (a -> b -> b) -> b -> a -> (Int# -> b) -> Int# -> b
-- The \m accounts for the fact that takeFB is used in a higher-order
-- way by takeFoldr, so it's better to inline.  A good example is
--     take n (repeat x)
-- for which we get excellent code... but only if we inline takeFB
-- when given four arguments
takeFB c n x xs
  = \ m -> if isTrue# (m <=# 1#)
           then x `c` n
           else x `c` xs (m -# 1#)

{-# INLINE [0] take #-}
take (I# n#) xs = takeUInt n# xs

-- The general code for take, below, checks n <= maxInt
-- No need to check for maxInt overflow when specialised
-- at type Int or Int# since the Int must be <= maxInt

takeUInt :: Int# -> [b] -> [b]
takeUInt n xs
  | isTrue# (n >=# 0#) = take_unsafe_UInt n xs
  | otherwise          = []

take_unsafe_UInt :: Int# -> [b] -> [b]
take_unsafe_UInt 0#  _  = []
take_unsafe_UInt m   ls =
  case ls of
    []     -> []
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

takeUInt_append :: Int# -> [b] -> [b] -> [b]
takeUInt_append n xs rs
  | isTrue# (n >=# 0#) = take_unsafe_UInt_append n xs rs
  | otherwise          = []

take_unsafe_UInt_append :: Int# -> [b] -> [b] -> [b]
take_unsafe_UInt_append 0#  _ rs  = rs
take_unsafe_UInt_append m  ls rs  =
  case ls of
    []     -> rs
    (x:xs) -> x : take_unsafe_UInt_append (m -# 1#) xs rs

drop (I# n#) ls
  | isTrue# (n# <# 0#) = ls
  | otherwise          = drop# n# ls
    where
        drop# :: Int# -> [a] -> [a]
        drop# 0# xs      = xs
        drop# _  xs@[]   = xs
        drop# m# (_:xs)  = drop# (m# -# 1#) xs

splitAt (I# n#) ls
  | isTrue# (n# <# 0#) = ([], ls)
  | otherwise          = splitAt# n# ls
    where
        splitAt# :: Int# -> [a] -> ([a], [a])
        splitAt# 0# xs     = ([], xs)
        splitAt# _  xs@[]  = (xs, xs)
        splitAt# m# (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt# (m# -# 1#) xs

#endif /* USE_REPORT_PRELUDE */

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- > span (< 9) [1,2,3] == ([1,2,3],[])
-- > span (< 0) [1,2,3] == ([],[1,2,3])
--
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@

span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.

break                   :: (a -> Bool) -> [a] -> ([a],[a])
#ifdef USE_REPORT_PRELUDE
break p                 =  span (not . p)
#else
-- HBC version (stolen)
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)
#endif

-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse                 :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
reverse                 =  foldl (flip (:)) []
#else
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
#endif

-- | 'and' returns the conjunction of a Boolean list.  For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value at a finite index of a finite or infinite list.
and                     :: [Bool] -> Bool

-- | 'or' returns the disjunction of a Boolean list.  For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value at a finite index of a finite or infinite list.
or                      :: [Bool] -> Bool
#ifdef USE_REPORT_PRELUDE
and                     =  foldr (&&) True
or                      =  foldr (||) False
#else
and []          =  True
and (x:xs)      =  x && and xs
or []           =  False
or (x:xs)       =  x || or xs

{-# NOINLINE [1] and #-}
{-# NOINLINE [1] or #-}

{-# RULES
"and/build"     forall (g::forall b.(Bool->b->b)->b->b) .
                and (build g) = g (&&) True
"or/build"      forall (g::forall b.(Bool->b->b)->b->b) .
                or (build g) = g (||) False
 #-}
#endif

-- | Applied to a predicate and a list, 'any' determines if any element
-- of the list satisfies the predicate.  For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value for the predicate applied to an element at a finite index of a finite or infinite list.
any                     :: (a -> Bool) -> [a] -> Bool

-- | Applied to a predicate and a list, 'all' determines if all elements
-- of the list satisfy the predicate. For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value for the predicate applied to an element at a finite index of a finite or infinite list.
all                     :: (a -> Bool) -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
any p                   =  or . map p
all p                   =  and . map p
#else
any _ []        = False
any p (x:xs)    = p x || any p xs

all _ []        =  True
all p (x:xs)    =  p x && all p xs

{-# NOINLINE [1] any #-}
{-# NOINLINE [1] all #-}

{-# RULES
"any/build"     forall p (g::forall b.(a->b->b)->b->b) .
                any p (build g) = g ((||) . p) False
"all/build"     forall p (g::forall b.(a->b->b)->b->b) .
                all p (build g) = g ((&&) . p) True
 #-}
#endif

-- | 'elem' is the list membership predicate, usually written in infix form,
-- e.g., @x \`elem\` xs@.  For the result to be
-- 'False', the list must be finite; 'True', however, results from an element equal to @x@ found at a finite index of a finite or infinite list.
elem                    :: (Eq a) => a -> [a] -> Bool

-- | 'notElem' is the negation of 'elem'.
notElem                 :: (Eq a) => a -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
elem x                  =  any (== x)
notElem x               =  all (/= x)
#else
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys

notElem _ []    =  True
notElem x (y:ys)=  x /= y && notElem x ys
#endif

-- | 'lookup' @key assocs@ looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- | Map a function over a list and concatenate the results.
concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []

{-# NOINLINE [1] concatMap #-}

{-# RULES
"concatMap" forall f xs . concatMap f xs =
    build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
 #-}


-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = foldr (++) []

{-# NOINLINE [1] concat #-}

{-# RULES
  "concat" forall xs. concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
-- We don't bother to turn non-fusible applications of concat back into concat
 #-}

\end{code}


\begin{code}
-- | List index (subscript) operator, starting from 0.
-- It is an instance of the more general 'Data.List.genericIndex',
-- which takes an index of any integral type.
(!!)                    :: [a] -> Int -> a
#ifdef USE_REPORT_PRELUDE
xs     !! n | n < 0 =  error "Prelude.!!: negative index"
[]     !! _         =  error "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)
#else
-- HBC version (stolen), then unboxified
xs !! (I# n0) | isTrue# (n0 <# 0#) =  error "Prelude.(!!): negative index\n"
              | otherwise          =  sub xs n0
                         where
                            sub :: [a] -> Int# -> a
                            sub []     _ = error "Prelude.(!!): index too large\n"
                            sub (y:ys) n = if isTrue# (n ==# 0#)
                                           then y
                                           else sub ys (n -# 1#)
#endif
\end{code}


%*********************************************************
%*                                                      *
\subsection{The zip family}
%*                                                      *
%*********************************************************

\begin{code}
foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 k z = go
  where
        go []    ys      = ys `seq` z -- see #9495 for the seq
        go _xs   []      = z
        go (x:xs) (y:ys) = k x y (go xs ys)
{-# INLINE [0] foldr2 #-}

foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left _k  z _x _r []     = z
foldr2_left  k _z  x  r (y:ys) = k x y (r ys)

foldr2_right :: (a -> b -> c -> d) -> d -> b -> ([a] -> c) -> [a] -> d
foldr2_right _k z  _y _r []     = z
foldr2_right  k _z  y  r (x:xs) = k x y (r xs)

-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
-- foldr2 k z xs ys = foldr (foldr2_right k z) (\_ -> z) ys xs
{-# RULES
"foldr2/left"   forall k z ys (g::forall b.(a->b->b)->b->b) .
                  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys

"foldr2/right"  forall k z xs (g::forall b.(a->b->b)->b->b) .
                  foldr2 k z xs (build g) = g (foldr2_right k z) (\_ -> z) xs
 #-}
\end{code}

Zips for larger tuples are in the List module.

\begin{code}
----------------------------------------------
-- | 'zip' takes two lists and returns a list of corresponding pairs.
-- If one input list is short, excess elements of the longer list are
-- discarded.
--
-- NOTE: GHC's implementation of @zip@ deviates slightly from the
-- standard. In particular, Haskell 98 and Haskell 2010 require that
-- @zip [x1,x2,...,xn] (y1:y2:...:yn:_|_) = [(x1,y1),(x2,y2),...,(xn,yn)]@
-- In GHC, however,
-- @zip [x1,x2,...,xn] (y1:y2:...:yn:_|_) = (x1,y1):(x2,y2):...:(xn,yn):_|_@
-- That is, you cannot use termination of the left list to avoid hitting
-- bottom in the right list.

-- This deviation is necessary to make fusion with 'build' in the right
-- list preserve semantics.
{-# NOINLINE [1] zip #-}
zip :: [a] -> [b] -> [(a,b)]
zip []     bs     = bs `seq` [] -- see #9495 for the seq
zip _as    []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs

{-# INLINE [0] zipFB #-}
zipFB :: ((a, b) -> c -> d) -> a -> b -> c -> d
zipFB c = \x y r -> (x,y) `c` r

{-# RULES
"zip"      [~1] forall xs ys. zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
"zipList"  [1]  foldr2 (zipFB (:)) []   = zip
 #-}
\end{code}

\begin{code}
----------------------------------------------
-- | 'zip3' takes three lists and returns a list of triples, analogous to
-- 'zip'.
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []
\end{code}


-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.

\begin{code}
----------------------------------------------
-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'zipWith' (+)@ is applied to two lists to produce the
-- list of corresponding sums.
--
-- NOTE: GHC's implementation of @zipWith@ deviates slightly from the
-- standard. In particular, Haskell 98 and Haskell 2010 require that
-- @zipWith (,) [x1,x2,...,xn] (y1:y2:...:yn:_|_) = [(x1,y1),(x2,y2),...,(xn,yn)]@
-- In GHC, however,
-- @zipWith (,) [x1,x2,...,xn] (y1:y2:...:yn:_|_) = (x1,y1):(x2,y2):...:(xn,yn):_|_@
-- That is, you cannot use termination of the left list to avoid hitting
-- bottom in the right list.

-- This deviation is necessary to make fusion with 'build' in the right
-- list preserve semantics.

{-# NOINLINE [1] zipWith #-}
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith _f []     bs     = bs `seq` [] -- see #9495 for the seq
zipWith _f _as    []     = []
zipWith f  (a:as) (b:bs) = f a b : zipWith f as bs

-- zipWithFB must have arity 2 since it gets two arguments in the "zipWith"
-- rule; it might not get inlined otherwise
{-# INLINE [0] zipWithFB #-}
zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB c f = \x y r -> (x `f` y) `c` r

{-# RULES
"zipWith"       [~1] forall f xs ys.    zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
"zipWithList"   [1]  forall f.  foldr2 (zipWithFB (:) f) [] = zipWith f
  #-}
\end{code}

\begin{code}
-- | The 'zipWith3' function takes a function which combines three
-- elements, as well as three lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []

-- | 'unzip' transforms a list of pairs into a list of first components
-- and a list of second components.
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

-- | The 'unzip3' function takes a list of triples and returns three
-- lists, analogous to 'unzip'.
unzip3   :: [(a,b,c)] -> ([a],[b],[c])
{-# INLINE unzip3 #-}
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                  ([],[],[])
\end{code}


%*********************************************************
%*                                                      *
\subsection{Error code}
%*                                                      *
%*********************************************************

Common up near identical calls to `error' to reduce the number
constant strings created when compiled:

\begin{code}
errorEmptyList :: String -> a
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

prel_list_str :: String
prel_list_str = "Prelude."
\end{code}
