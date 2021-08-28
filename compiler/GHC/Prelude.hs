-- | Custom GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.

-- Every module in GHC
--   * Is compiled with -XNoImplicitPrelude
--   * Explicitly imports GHC.Prelude

module GHC.Prelude (module GHC.Prelude, module X) where

-- We export the 'Semigroup' class but w/o the (<>) operator to avoid
-- clashing with the (Outputable.<>) operator which is heavily used
-- through GHC's code-base.

import Prelude as X hiding (filter, unzip, unzip3, unzip4, unzip5)
import Control.Applicative as X (Alternative (..))
import Control.Monad as X (guard)
import Data.Bool as X (bool)
import Data.Filtrable as X
import Data.Foldable as X (asum, foldMap', foldl', for_, sequenceA_, traverse_)
import Data.Function as X (on)
import Data.Maybe as X (fromMaybe)
import Data.Monoid as X (Monoid (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Traversable as X (for, mapAccumL)
import Lens.Micro (_1, _2, _3)
import Lens.Micro as X (Lens, Lens', over, set)
import Lens.Micro.Extras (view)
import Util as X ((<₪>), (&), altMap)

{-
Note [Why do we import Prelude here?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The files ghc-boot-th.cabal, ghc-boot.cabal, ghci.cabal and
ghc-heap.cabal contain the directive default-extensions:
NoImplicitPrelude. There are two motivations for this:
  - Consistency with the compiler directory, which enables
    NoImplicitPrelude;
  - Allows loading the above dependent packages with ghc-in-ghci,
    giving a smoother development experience when adding new
    extensions.
-}

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

unzip3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
unzip3 xs = (view _1 <$> xs, view _2 <$> xs, view _3 <$> xs)
{-# INLINE unzip3 #-}
