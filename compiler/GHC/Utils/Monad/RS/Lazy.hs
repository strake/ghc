{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module GHC.Utils.Monad.RS.Lazy where

import GHC.Prelude

import Control.Monad (ap, join)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))

import qualified Control.Monad.Reader.Class as M
import qualified Control.Monad.State.Class as M

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

{-# SPECIALIZE ask :: RS r s r #-}
ask :: Applicative m => RST r s m r
ask = RST $ \r s -> pure (r, s)

{-# SPECIALIZE asks :: (r -> a) -> RS r s a #-}
asks :: Applicative m => (r -> a) -> RST r s m a
asks f = RST $ \r s -> pure (f r, s)

{-# SPECIALIZE local :: (r' -> r) -> RS r s a -> RS r' s a #-}
local :: Applicative m => (r' -> r) -> RST r s m a -> RST r' s m a
local f (RST x) = RST (x . f)

{-# SPECIALIZE get :: RS r s s #-}
get :: Applicative m => RST r s m s
get = RST (pure (pure . join (,)))

{-# SPECIALIZE gets :: (s -> a) -> RS r s a #-}
gets :: Applicative m => (s -> a) -> RST r s m a
gets f = RST $ \_ s -> pure (f s, s)

{-# SPECIALIZE put :: s -> RS r s () #-}
put :: Applicative m => s -> RST r s m ()
put s' = RST $ \_ _ -> pure ((), s')

{-# SPECIALIZE modify :: (s -> s) -> RS r s () #-}
modify :: Applicative m => (s -> s) -> RST r s m ()
modify f = RST (pure (pure . (,) () . f))

{-# SPECIALIZE state :: (s -> (a, s)) -> RS r s a #-}
state :: Applicative m => (s -> (a, s)) -> RST r s m a
state f = RST (pure (pure . f))

evalRST :: Functor m => RST r s m a -> r -> s -> m a
evalRST = (fmap . fmap . fmap) fst . runRST

execRST :: Functor m => RST r s m a -> r -> s -> m s
execRST = (fmap . fmap . fmap) snd . runRST

evalRS :: RS r s a -> r -> s -> a
evalRS = (fmap . fmap) fst . runRS

execRS :: RS r s a -> r -> s -> s
execRS = (fmap . fmap) snd . runRS

instance Monad m => M.MonadState s (RST r s m) where
    get = get
    put = put
    state = state

instance Monad m => M.MonadReader r (RST r s m) where
    ask = ask
    local = local
