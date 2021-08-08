{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}
module Maybes (
        module Data.Maybe,

        MaybeErr(..), -- Instance of Monad
        failME, isSuccess,

        orElse,
        firstJust, firstJusts,
        whenIsJust,
        expectJust,

        MaybeT(..), liftMaybeT
    ) where

import Control.Applicative
import Control.Monad
#if __GLASGOW_HASKELL__ > 710
import Control.Monad.Fail
#endif
import Data.Maybe

infixr 4 `orElse`

{-
************************************************************************
*                                                                      *
\subsection[Maybe type]{The @Maybe@ type}
*                                                                      *
************************************************************************
-}

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust a b = firstJusts [a, b]

-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: [Maybe a] -> Maybe a
firstJusts = msum

expectJust :: String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust _   (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)

whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust (Just x) f = f x
whenIsJust Nothing  _ = return ()

-- | Flipped version of @fromMaybe@, useful for chaining.
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

{-
************************************************************************
*                                                                      *
\subsection[MaybeT type]{The @MaybeT@ monad transformer}
*                                                                      *
************************************************************************
-}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f x = MaybeT $ fmap (fmap f) $ runMaybeT x

#if __GLASGOW_HASKELL__ < 710
-- Pre-AMP change
instance (Monad m, Applicative m) => Applicative (MaybeT m) where
#else
instance (Monad m) => Applicative (MaybeT m) where
#endif
  pure = MaybeT . pure . Just
  (<*>) = ap

#if __GLASGOW_HASKELL__ < 710
-- Pre-AMP change
instance (Monad m, Applicative m) => Monad (MaybeT m) where
#else
instance (Monad m) => Monad (MaybeT m) where
#endif
  return = pure
  x >>= f = MaybeT $ runMaybeT x >>= maybe (pure Nothing) (runMaybeT . f)
  fail _ = MaybeT $ pure Nothing


#if __GLASGOW_HASKELL__ > 710
instance Monad m => MonadFail (MaybeT m) where
  fail _ = MaybeT $ return Nothing
#endif

#if __GLASGOW_HASKELL__ < 710
-- Pre-AMP change
instance (Monad m, Applicative m) => Alternative (MaybeT m) where
#else
instance (Monad m) => Alternative (MaybeT m) where
#endif
  empty = mzero
  (<|>) = mplus

#if __GLASGOW_HASKELL__ < 710
-- Pre-AMP change
instance (Monad m, Applicative m) => MonadPlus (MaybeT m) where
#else
instance Monad m => MonadPlus (MaybeT m) where
#endif
  mzero       = MaybeT $ pure Nothing
  p `mplus` q = MaybeT $ do ma <- runMaybeT p
                            case ma of
                              Just a  -> pure (Just a)
                              Nothing -> runMaybeT q

liftMaybeT :: Monad m => m a -> MaybeT m a
liftMaybeT act = MaybeT $ Just `liftM` act

{-
************************************************************************
*                                                                      *
\subsection[MaybeErr type]{The @MaybeErr@ type}
*                                                                      *
************************************************************************
-}

data MaybeErr err val = Succeeded val | Failed err

instance Functor (MaybeErr err) where
  fmap = liftM

instance Applicative (MaybeErr err) where
  pure  = Succeeded
  (<*>) = ap

instance Monad (MaybeErr err) where
  return = pure
  Succeeded v >>= k = k v
  Failed e    >>= _ = Failed e

isSuccess :: MaybeErr err val -> Bool
isSuccess (Succeeded {}) = True
isSuccess (Failed {})    = False

failME :: err -> MaybeErr err val
failME e = Failed e
