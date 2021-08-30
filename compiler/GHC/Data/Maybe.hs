{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Data.Maybe (
        module Data.Maybe,

        catMaybes, mapMaybe,

        MaybeErr(..), -- Instance of Monad
        failME, isSuccess,

        orElse,
        firstJust, firstJusts,
        expectJust,
        rightToMaybe,

        -- * MaybeT
        MaybeT(..), liftMaybeT, tryMaybeT
    ) where

import GHC.Prelude

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe
import Data.Maybe hiding (catMaybes, mapMaybe)
import GHC.Utils.Exception (catch, SomeException(..))
import GHC.Utils.Misc (HasCallStack)

infixr 4 `orElse`

{-
************************************************************************
*                                                                      *
\subsection[Maybe type]{The @Maybe@ type}
*                                                                      *
************************************************************************
-}

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust = ((<|>))

-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: [Maybe a] -> Maybe a
firstJusts = asum

expectJust :: HasCallStack => String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust _   (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)

-- | Flipped version of @fromMaybe@, useful for chaining.
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x

{-
************************************************************************
*                                                                      *
\subsection[MaybeT type]{The @MaybeT@ monad transformer}
*                                                                      *
************************************************************************
-}

-- We had our own MaybeT in the past. Now we reuse transformer's MaybeT

liftMaybeT :: Monad m => m a -> MaybeT m a
liftMaybeT act = MaybeT (Just <$> act)

-- | Try performing an 'IO' action, failing on error.
tryMaybeT :: IO a -> MaybeT IO a
tryMaybeT action = MaybeT $ catch (Just <$> action) handler
  where
    handler (SomeException _) = return Nothing

{-
************************************************************************
*                                                                      *
\subsection[MaybeErr type]{The @MaybeErr@ type}
*                                                                      *
************************************************************************
-}

data MaybeErr err val = Succeeded val | Failed err
    deriving (Functor)

instance Applicative (MaybeErr err) where
  pure  = Succeeded
  Failed err <*> _ = Failed err
  _ <*> Failed err = Failed err
  Succeeded f <*> Succeeded x = Succeeded (f x)

instance Monad (MaybeErr err) where
  Succeeded v >>= k = k v
  Failed e    >>= _ = Failed e

isSuccess :: MaybeErr err val -> Bool
isSuccess (Succeeded {}) = True
isSuccess (Failed {})    = False

failME :: err -> MaybeErr err val
failME = Failed
