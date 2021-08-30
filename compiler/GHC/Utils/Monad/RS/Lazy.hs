{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GHC.Utils.Monad.RS.Lazy (module GHC.Utils.Monad.RS.Lazy, ask, asks, local, get, put, gets, state, modify) where

import GHC.Prelude

import Control.Monad (ap, join)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))

import Data.Functor.Reader.Class
import Data.Functor.State.Class

newtype RST r s m a = RST { runRST :: r -> s -> m (a, s) }
    deriving (Functor)

type RS r s = RST r s Identity

runRS :: ∀ r s a . RS r s a -> r -> s -> (a, s)
runRS = coerce (runRST :: RST r s Identity a -> _)

rs :: ∀ r s a . (r -> s -> (a, s)) -> RS r s a
rs = coerce (RST :: _ -> RST r s Identity a)

instance Monad m => Applicative (RST r s m) where
   pure x = RST $ \_ s -> pure (x, s)
   (<*>) = ap

instance Monad m => Monad (RST r s m) where
    m >>= n = RST $ \r s -> runRST m r s >>= \ case
        (x, s') -> runRST (n x) r s'

instance MonadFix m => MonadFix (RST r s m) where
    mfix f = RST $ \ r s -> mfix $ \ ~(a, _) -> runRST (f a) r s

instance MonadIO m => MonadIO (RST r s m) where
    liftIO x = RST $ \ _ s -> flip (,) s <$> liftIO x

evalRST :: Functor m => RST r s m a -> r -> s -> m a
evalRST = (fmap . fmap . fmap) fst . runRST

execRST :: Functor m => RST r s m a -> r -> s -> m s
execRST = (fmap . fmap . fmap) snd . runRST

evalRS :: RS r s a -> r -> s -> a
evalRS = (fmap . fmap) fst . runRS

execRS :: RS r s a -> r -> s -> s
execRS = (fmap . fmap) snd . runRS

instance Applicative m => IsState (RST r s m) where
    type StateType (RST r s m) = s

    {-# SPECIALIZE get :: RS r s s #-}
    get = RST (pure (pure . join (,)))

    {-# SPECIALIZE put :: s -> RS r s () #-}
    put s' = RST \_ _ -> pure ((), s')

    {-# SPECIALIZE state :: (s -> (a, s)) -> RS r s a #-}
    state f = RST (pure (pure . f))

instance Applicative m => IsReader (RST r s m) where
    type EnvType (RST r s m) = r

    {-# SPECIALIZE ask :: RS r s r #-}
    ask = RST (curry pure)

instance Applicative m => IsLocal (RST x s m) (RST y s m) where
    local f (RST x) = RST (x . f)
    {-# SPECIALIZE local :: (r' -> r) -> RS r s a -> RS r' s a #-}
