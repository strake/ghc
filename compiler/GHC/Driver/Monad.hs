{-# LANGUAGE DeriveFunctor, DerivingVia, RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2010
--
-- The Session type and related functionality
--
-- -----------------------------------------------------------------------------

module GHC.Driver.Monad (
        -- * 'Ghc' monad stuff
        GhcMonad(..),
        Ghc(..),
        GhcT(..), liftGhcT,
        reflectGhc, reifyGhc,
        getSessionDynFlags,
        liftIO,
        Session(..), withSession, modifySession, modifySessionM, stateSessionM,
        withTempSession,

        -- ** Warnings
        logWarnings, printException,
        WarnErrLogger, defaultWarnErrLogger
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Utils.Monad
import GHC.Utils.Exception
import GHC.Utils.Error

import GHC.Types.SourceError

import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.Trans.Reader
import Data.IORef

-- -----------------------------------------------------------------------------
-- | A monad that has all the features needed by GHC API calls.
--
-- In short, a GHC monad
--
--   - allows embedding of IO actions,
--
--   - can log warnings,
--
--   - allows handling of (extensible) exceptions, and
--
--   - maintains a current session.
--
-- If you do not use 'Ghc' or 'GhcT', make sure to call 'GHC.initGhcMonad'
-- before any call to the GHC API functions can occur.
--
class (Functor m, ExceptionMonad m, HasDynFlags m) => GhcMonad m where
  getSession :: m HscEnv
  setSession :: HscEnv -> m ()

-- | Call the argument with the current session.
withSession :: GhcMonad m => (HscEnv -> m a) -> m a
withSession f = getSession >>= f

-- | Grabs the DynFlags from the Session
getSessionDynFlags :: GhcMonad m => m DynFlags
getSessionDynFlags = withSession (return . hsc_dflags)

-- | Set the current session to the result of applying the current session to
-- the argument.
modifySession :: GhcMonad m => (HscEnv -> HscEnv) -> m ()
modifySession f = do h <- getSession
                     setSession $! f h

-- | Set the current session to the result of applying the current session to
-- the argument.
modifySessionM :: GhcMonad m => (HscEnv -> m HscEnv) -> m ()
modifySessionM f = do h <- getSession
                      h' <- f h
                      setSession $! h'

stateSessionM :: GhcMonad m => (HscEnv -> m (a, HscEnv)) -> m a
stateSessionM f = do
    h <- getSession
    (a, h') <- f h
    a <$ (setSession $! h')

withSavedSession :: GhcMonad m => m a -> m a
withSavedSession m = do
  saved_session <- getSession
  m `MC.finally` setSession saved_session

-- | Call an action with a temporarily modified Session.
withTempSession :: GhcMonad m => (HscEnv -> HscEnv) -> m a -> m a
withTempSession f m =
  withSavedSession $ modifySession f >> m

-- -----------------------------------------------------------------------------
-- | A monad that allows logging of warnings.

logWarnings :: GhcMonad m => WarningMessages -> m ()
logWarnings warns = do
  dflags <- getSessionDynFlags
  liftIO $ printOrThrowWarnings dflags warns

-- -----------------------------------------------------------------------------
-- | A minimal implementation of a 'GhcMonad'.  If you need a custom monad,
-- e.g., to maintain additional state consider wrapping this monad or using
-- 'GhcT'.
newtype Ghc a = Ghc { unGhc :: Session -> IO a }
  deriving (Functor)
  deriving (Applicative, Monad, MonadFail, MonadFix, MonadThrow, MonadCatch, MonadMask) via (ReaderT Session IO)

-- | The Session is a handle to the complete state of a compilation
-- session.  A compilation session consists of a set of modules
-- constituting the current program or library, the context for
-- interactive evaluation, and various caches.
data Session = Session !(IORef HscEnv)

instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA

instance HasDynFlags Ghc where
  getDynFlags = getSessionDynFlags

instance GhcMonad Ghc where
  getSession = Ghc $ \(Session r) -> readIORef r
  setSession s' = Ghc $ \(Session r) -> writeIORef r s'

-- | Reflect a computation in the 'Ghc' monad into the 'IO' monad.
--
-- You can use this to call functions returning an action in the 'Ghc' monad
-- inside an 'IO' action.  This is needed for some (too restrictive) callback
-- arguments of some library functions:
--
-- > libFunc :: String -> (Int -> IO a) -> IO a
-- > ghcFunc :: Int -> Ghc a
-- >
-- > ghcFuncUsingLibFunc :: String -> Ghc a -> Ghc a
-- > ghcFuncUsingLibFunc str =
-- >   reifyGhc $ \s ->
-- >     libFunc $ \i -> do
-- >       reflectGhc (ghcFunc i) s
--
reflectGhc :: Ghc a -> Session -> IO a
reflectGhc = unGhc

-- > Dual to 'reflectGhc'.  See its documentation.
reifyGhc :: (Session -> IO a) -> Ghc a
reifyGhc = Ghc

-- -----------------------------------------------------------------------------
-- | A monad transformer to add GHC specific features to another monad.
--
-- Note that the wrapped monad must support IO and handling of exceptions.
newtype GhcT m a = GhcT { unGhcT :: Session -> m a }
  deriving (Functor)
  deriving (Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask) via (ReaderT Session m)

liftGhcT :: m a -> GhcT m a
liftGhcT m = GhcT $ \_ -> m

instance MonadIO m => HasDynFlags (GhcT m) where
  getDynFlags = GhcT $ \(Session r) -> liftM hsc_dflags (liftIO $ readIORef r)

instance ExceptionMonad m => GhcMonad (GhcT m) where
  getSession = GhcT $ \(Session r) -> liftIO $ readIORef r
  setSession s' = GhcT $ \(Session r) -> liftIO $ writeIORef r s'


-- | Print the error message and all warnings.  Useful inside exception
--   handlers.  Clears warnings after printing.
printException :: GhcMonad m => SourceError -> m ()
printException err = do
  dflags <- getSessionDynFlags
  liftIO $ printBagOfErrors dflags (srcErrorMessages err)

-- | A function called to log warnings and errors.
type WarnErrLogger = forall m. GhcMonad m => Maybe SourceError -> m ()

defaultWarnErrLogger :: WarnErrLogger
defaultWarnErrLogger = traverse_ printException
