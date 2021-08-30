module GHC.Utils.Lens.Monad where

import GHC.Prelude

import Data.Functor.Const (Const (..))
import Data.Functor.Reader.Class as Reader
import Data.Functor.State.Class as State
import Data.Monoid (First (..))
import Lens.Micro
import Lens.Micro.Internal (foldMapOf)

view :: IsReader m => Getting a (EnvType m) a -> m a
view l = Reader.asks (getConst . l Const)
{-# INLINE view #-}

preview :: IsReader m => Getting (First a) (EnvType m) a -> m (Maybe a)
preview l = Reader.asks (getFirst . foldMapOf l (First . Just))
{-# INLINE preview #-}

use :: IsState m => Getting a (StateType m) a -> m a
use l = State.gets (view l)
{-# INLINE use #-}

preuse :: IsState m => Getting (First a) (StateType m) a -> m (Maybe a)
preuse l = State.gets (preview l)
{-# INLINE preuse #-}

assign :: (Applicative m, IsState m) => ASetter (StateType m) (StateType m) a b -> b -> m ()
assign l x = State.modify (set l x)
{-# INLINE assign #-}

stating :: IsState m => LensLike ((,) z) (StateType m) (StateType m) a b -> (a -> (z, b)) -> m z
stating l = State.state . l
{-# INLINE stating #-}

modifying :: IsState m => LensLike ((,) a) (StateType m) (StateType m) a b -> (a -> b) -> m a
modifying l f = stating l ((,) <*> f)
{-# INLINE modifying #-}

modifying_ :: (Applicative m, IsState m) => ASetter (StateType m) (StateType m) a b -> (a -> b) -> m ()
modifying_ l = State.modify . over l
{-# INLINE modifying_ #-}

setting :: IsState m => LensLike ((,) a) (StateType m) (StateType m) a b -> b -> m a
setting l = modifying l . pure
{-# INLINE setting #-}

setting_ :: (Applicative m, IsState m) => ASetter (StateType m) (StateType m) a b -> b -> m ()
setting_ l = State.modify . set l
{-# INLINE setting_ #-}

locally :: IsLocal f g => ASetter (EnvType g) (EnvType f) a b -> (a -> b) -> f c -> g c
locally l = Reader.local . over l
{-# INLINE locally #-}

{-
locallyM :: LensLike (Compose g ((,) a)) (EnvType g) (EnvType f) a b -> (a -> g b) -> f c -> g (a, c)
locallyM l =

locallyM_ :: LensLike g (EnvType g) (EnvType f) a b -> (a -> g b) -> f c -> g c
locallyM_ = _
-}
