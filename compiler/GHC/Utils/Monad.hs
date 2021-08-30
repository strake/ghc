-- | Utilities related to Monad and Applicative classes
--   Mostly for backwards compatibility.

module GHC.Utils.Monad
        ( Applicative(..)
        , (<$>)

        , MonadFix(..)
        , MonadIO(..)

        , zipWith3M, zipWith3M_, zipWith4M, zipWithAndUnzipM
        , mapAndUnzipM, mapAndUnzip3M, mapAndUnzip4M, mapAndUnzip5M
        , mapAccumLM
        , concatMapM
        , anyM, allM, orM, andM
        , foldlM, foldlM_, foldrM
        , whenM, unlessM
        , filterOutM
        ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import GHC.Prelude
import GHC.Utils.Misc (unzip, unzip3, unzip4, unzip5)

import Control.Applicative
import Control.Monad hiding (mapAndUnzipM)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Foldable (foldlM, foldrM, toList)
import Data.List (zipWith4)
import Data.Tuple (swap)

-------------------------------------------------------------------------------
-- Common functions
--  These are used throughout the compiler
-------------------------------------------------------------------------------

{-

Note [Inline @zipWithNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle for 'zipWith3M', 'zipWith4M' and 'zipWith3M_' is the same
as for 'zipWithM' and 'zipWithM_' in "Control.Monad", see
Note [Fusion for zipN/zipWithN] in GHC/List.hs for more details.

The 'zipWithM'/'zipWithM_' functions are inlined so that the `zipWith` and
`sequenceA` functions with which they are defined have an opportunity to fuse.

Furthermore, 'zipWith3M'/'zipWith4M' and 'zipWith3M_' have been explicitly
rewritten in a non-recursive way similarly to 'zipWithM'/'zipWithM_', and for
more than just uniformity: after [D5241](https://phabricator.haskell.org/D5241)
for issue #14037, all @zipN@/@zipWithN@ functions fuse, meaning
'zipWith3M'/'zipWIth4M' and 'zipWith3M_'@ now behave like 'zipWithM' and
'zipWithM_', respectively, with regards to fusion.

As such, since there are not any differences between 2-ary 'zipWithM'/
'zipWithM_' and their n-ary counterparts below aside from the number of
arguments, the `INLINE` pragma should be replicated in the @zipWithNM@
functions below as well.

-}

zipWith3M :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
{-# INLINE zipWith3M #-}
-- Inline so that fusion with 'zipWith3' and 'sequenceA' has a chance to fire.
-- See Note [Inline @zipWithNM@ functions] above.
zipWith3M f xs ys zs = sequenceA (zipWith3 f xs ys zs)

zipWith3M_ :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
{-# INLINE zipWith3M_ #-}
-- Inline so that fusion with 'zipWith4' and 'sequenceA' has a chance to fire.
-- See  Note [Inline @zipWithNM@ functions] above.
zipWith3M_ f xs ys zs = sequenceA_ (zipWith3 f xs ys zs)

zipWith4M :: Monad m => (a -> b -> c -> d -> m e)
          -> [a] -> [b] -> [c] -> [d] -> m [e]
{-# INLINE zipWith4M #-}
-- Inline so that fusion with 'zipWith5' and 'sequenceA' has a chance to fire.
-- See  Note [Inline @zipWithNM@ functions] above.
zipWith4M f xs ys ws zs = sequenceA (zipWith4 f xs ys ws zs)

zipWithAndUnzipM :: Monad m
                 => (a -> b -> m (c, d)) -> [a] -> [b] -> m ([c], [d])
{-# INLINABLE zipWithAndUnzipM #-}
-- See Note [flatten_args performance] in GHC.Tc.Solver.Flatten for why this
-- pragma is essential.
zipWithAndUnzipM f (x:xs) (y:ys) =
  [ (c:cs, d:ds) | (c, d) <- f x y, (cs, ds) <- zipWithAndUnzipM f xs ys ]
zipWithAndUnzipM _ _ _ = return ([], [])

{-

Note [Inline @mapAndUnzipNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle is the same as 'mapAndUnzipM' in "Control.Monad".
The 'mapAndUnzipM' function is inlined so that the `unzip` and `traverse`
functions with which it is defined have an opportunity to fuse, see
Note [Inline @unzipN@ functions] in Data/OldList.hs for more details.

Furthermore, the @mapAndUnzipNM@ functions have been explicitly rewritten in a
non-recursive way similarly to 'mapAndUnzipM', and for more than just
uniformity: after [D5249](https://phabricator.haskell.org/D5249) for Trac
ticket #14037, all @unzipN@ functions fuse, meaning 'mapAndUnzip3M',
'mapAndUnzip4M' and 'mapAndUnzip5M' now behave like 'mapAndUnzipM' with regards
to fusion.

As such, since there are not any differences between 2-ary 'mapAndUnzipM' and
its n-ary counterparts below aside from the number of arguments, the `INLINE`
pragma should be replicated in the @mapAndUnzipNM@ functions below as well.

-}

mapAndUnzipM :: (Monad m, Traversable t) => (a -> m (b,c)) -> t a -> m (t b, t c)
{-# INLINE mapAndUnzipM #-}
-- Inline so that fusion with 'unzip' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzipM f xs =  unzip <$> traverse f xs

-- | mapAndUnzipM for triples
mapAndUnzip3M :: (Monad m, Traversable t) => (a -> m (b,c,d)) -> t a -> m (t b, t c, t d)
{-# INLINE mapAndUnzip3M #-}
-- Inline so that fusion with 'unzip3' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip3M f xs =  unzip3 <$> traverse f xs

mapAndUnzip4M :: (Monad m, Traversable t) => (a -> m (b,c,d,e)) -> t a -> m (t b, t c, t d, t e)
{-# INLINE mapAndUnzip4M #-}
-- Inline so that fusion with 'unzip4' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip4M f xs =  unzip4 <$> traverse f xs

mapAndUnzip5M :: (Monad m, Traversable t) => (a -> m (b,c,d,e,f)) -> t a -> m (t b, t c, t d, t e, t f)
{-# INLINE mapAndUnzip5M #-}
-- Inline so that fusion with 'unzip5' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip5M f xs =  unzip5 <$> traverse f xs

-- TODO: mapAccumLM is used in many places. Surely most of
-- these don't actually want to be lazy. We should add a strict
-- variant and use it where appropriate.

-- | Monadic version of mapAccumL
mapAccumLM :: (Monad m, Traversable t)
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> t x                      -- ^ inputs
            -> m (acc, t y)             -- ^ final state, outputs
mapAccumLM f s = fmap swap . flip runStateT s . traverse f'
  where
    f' = StateT . (fmap . fmap) swap . flip f

-- | Monadic version of concatMap
concatMapM :: (Applicative m, Monad n, Traversable n) => (a -> m (n b)) -> n a -> m (n b)
concatMapM f = fmap join . traverse f

-- | Monadic version of 'any', aborts the computation at the first @True@ value
anyM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
anyM f = go . toList
  where
    go [] = pure False
    go (x:xs) = f x >>= bool (go xs) (pure True)

-- | Monad version of 'all', aborts the computation at the first @False@ value
allM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
allM f = go . toList
  where
    go []     = pure True
    go (b:bs) = f b >>= bool (pure False) (go bs)

-- | Monadic version of or
orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= bool m2 (pure True)

-- | Monadic version of and
andM :: Monad m => m Bool -> m Bool -> m Bool
andM m1 m2 = m1 >>= bool (pure False) m2

-- | Monadic version of foldl that discards its result
foldlM_ :: (Monad m, Foldable t) => (a -> b -> m a) -> a -> t b -> m ()
foldlM_ = foldM_

-- | Monadic version of @when@, taking the condition in the monad
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do { b <- mb; when b thing }

-- | Monadic version of @unless@, taking the condition in the monad
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb thing = do { b <- mb; unless b thing }

-- | Like 'filterM', only it reverses the sense of the test.
filterOutM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterOutM p = foldr (\ x -> liftA2 (bool (x:) id) (p x)) (pure [])
