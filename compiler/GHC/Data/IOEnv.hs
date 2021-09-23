{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
--
-- (c) The University of Glasgow 2002-2006
--

-- | The IO Monad with an environment
--
-- The environment is passed around as a Reader monad but
-- as its in the IO monad, mutable references can be used
-- for updating state.
--
module GHC.Data.IOEnv (
        IOEnv, -- Instance of Monad

        -- Monad utilities
        module GHC.Utils.Monad,

        -- Errors
        failM, failWithM,
        IOEnvFailure(..),

        runIOEnv, unsafeInterleaveM, uninterruptibleMaskM_,
        tryM, tryAllM, tryMostM, fixM,

        -- I/O operations
        IORef, newMutVar, readMutVar, writeMutVar, updMutVar,
        atomicUpdMutVar, atomicUpdMutVar'
  ) where

import GHC.Prelude

import GHC.Driver.Session
import {-# SOURCE #-} GHC.Driver.Hooks
import GHC.Utils.Exception
import GHC.Unit.Module
import GHC.Utils.Panic

import Data.IORef       ( IORef, newIORef, readIORef, writeIORef, modifyIORef,
                          atomicModifyIORef, atomicModifyIORef' )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.IO        ( fixIO )
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Trans.Reader (ReaderT (..))
import GHC.Utils.Monad
import Control.Applicative (Alternative(..))
import Data.Functor.Reader.Class

----------------------------------------------------------------------
-- Defining the monad type
----------------------------------------------------------------------


newtype IOEnv env a = IOEnv { unIOEnv :: env -> IO a }
  deriving (Functor)
  deriving (Applicative, Alternative, Monad, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadPlus) via (ReaderT env IO)

instance IsReader (IOEnv env) where
    type EnvType (IOEnv env) = env
    ask = IOEnv pure

instance IsLocal (IOEnv x) (IOEnv y) where
    local f = IOEnv . (. f) . unIOEnv

instance MonadFail (IOEnv m) where
    fail _ = failM -- Ignore the string

failM :: MonadIO m => m a
failM = liftIO $ throwIO IOEnvFailure

failWithM :: MonadIO m => String -> m a
failWithM s = liftIO $ ioError (userError s)

data IOEnvFailure = IOEnvFailure

instance Show IOEnvFailure where
    show IOEnvFailure = "IOEnv failure"

instance Exception IOEnvFailure

instance ContainsDynFlags env => HasDynFlags (IOEnv env) where
    getDynFlags = do env <- ask
                     return $! extractDynFlags env

instance ContainsHooks env => HasHooks (IOEnv env) where
    getHooks = do env <- ask
                  return $! extractHooks env

instance ContainsModule env => HasModule (IOEnv env) where
    getModule = asks extractModule

----------------------------------------------------------------------
-- Fundamental combinators specific to the monad
----------------------------------------------------------------------


---------------------------
runIOEnv :: env -> IOEnv env a -> IO a
runIOEnv env (IOEnv m) = m env


---------------------------
{-# NOINLINE fixM #-}
  -- Aargh!  Not inlining fixM alleviates a space leak problem.
  -- Normally fixM is used with a lazy tuple match: if the optimiser is
  -- shown the definition of fixM, it occasionally transforms the code
  -- in such a way that the code generator doesn't spot the selector
  -- thunks.  Sigh.

fixM :: (a -> IOEnv env a) -> IOEnv env a
fixM f = IOEnv (\ env -> fixIO (\ r -> unIOEnv (f r) env))

instance MonadFix (IOEnv env) where mfix = fixM

---------------------------
tryM :: IOEnv env r -> IOEnv env (Either IOEnvFailure r)
-- Reflect UserError exceptions (only) into IOEnv monad
-- Other exceptions are not caught; they are simply propagated as exns
--
-- The idea is that errors in the program being compiled will give rise
-- to UserErrors.  But, say, pattern-match failures in GHC itself should
-- not be caught here, else they'll be reported as errors in the program
-- begin compiled!
tryM (IOEnv thing) = IOEnv (\ env -> tryIOEnvFailure (thing env))

tryIOEnvFailure :: IO a -> IO (Either IOEnvFailure a)
tryIOEnvFailure = try

-- XXX We shouldn't be catching everything, e.g. timeouts
tryAllM :: IOEnv env r -> IOEnv env (Either SomeException r)
-- Catch *all* exceptions
-- This is used when running a Template-Haskell splice, when
-- even a pattern-match failure is a programmer error
tryAllM (IOEnv thing) = IOEnv (\ env -> try (thing env))

tryMostM :: IOEnv env r -> IOEnv env (Either SomeException r)
tryMostM (IOEnv thing) = IOEnv (\ env -> tryMost (thing env))

---------------------------
unsafeInterleaveM :: IOEnv env a -> IOEnv env a
unsafeInterleaveM (IOEnv m) = IOEnv (\ env -> unsafeInterleaveIO (m env))

uninterruptibleMaskM_ :: IOEnv env a -> IOEnv env a
uninterruptibleMaskM_ (IOEnv m) = IOEnv (\ env -> uninterruptibleMask_ (m env))

----------------------------------------------------------------------
-- Accessing input/output
----------------------------------------------------------------------

newMutVar :: MonadIO m => a -> m (IORef a)
newMutVar val = liftIO (newIORef val)

writeMutVar :: MonadIO m => IORef a -> a -> m ()
writeMutVar var val = liftIO (writeIORef var val)

readMutVar :: MonadIO m => IORef a -> m a
readMutVar var = liftIO (readIORef var)

updMutVar :: MonadIO m => IORef a -> (a -> a) -> m ()
updMutVar var upd = liftIO (modifyIORef var upd)

-- | Atomically update the reference.  Does not force the evaluation of the
-- new variable contents.  For strict update, use 'atomicUpdMutVar''.
atomicUpdMutVar :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicUpdMutVar var upd = liftIO (atomicModifyIORef var upd)

-- | Strict variant of 'atomicUpdMutVar'.
atomicUpdMutVar' :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicUpdMutVar' var upd = liftIO (atomicModifyIORef' var upd)
