module GHC.Utils.Lens.Monad where

import GHC.Prelude

import Control.Monad.Reader.Class as Reader
import Control.Monad.State.Class as State
import Data.Functor.Const (Const (..))
import Data.Monoid (First (..))
import Lens.Micro
import Lens.Micro.Internal (foldMapOf)

view :: MonadReader s m => Getting a s a -> m a
view l = Reader.asks (getConst . l Const)
{-# INLINE view #-}

preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = Reader.asks (getFirst . foldMapOf l (First . Just))
{-# INLINE preview #-}

use :: MonadState s m => Getting a s a -> m a
use l = State.gets (view l)
{-# INLINE use #-}

preuse :: MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = State.gets (preview l)
{-# INLINE preuse #-}

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l x = State.modify (set l x)
{-# INLINE assign #-}

stating :: MonadState s m => LensLike ((,) z) s s a b -> (a -> (z, b)) -> m z
stating l = State.state . l
{-# INLINE stating #-}

modifying :: MonadState s m => LensLike ((,) a) s s a b -> (a -> b) -> m a
modifying l f = stating l ((,) <*> f)
{-# INLINE modifying #-}

modifying_ :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
modifying_ l = State.modify . over l
{-# INLINE modifying_ #-}

setting :: MonadState s m => LensLike ((,) a) s s a b -> b -> m a
setting l = modifying l . pure
{-# INLINE setting #-}

setting_ :: MonadState s m => ASetter s s a b -> b -> m ()
setting_ l = State.modify . set l
{-# INLINE setting_ #-}

locally :: MonadReader r m => ASetter r r a b -> (a -> b) -> m c -> m c
locally l = Reader.local . over l
{-# INLINE locally #-}
