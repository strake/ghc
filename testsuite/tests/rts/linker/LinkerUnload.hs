module LinkerUnload (init) where

import GHC
import GHC.Driver.Session
import GHC.Driver.Backend
import qualified GHC.Linker.Loader as Loader
import System.Environment
import GHC.Utils.Monad ( MonadIO(..) )

foreign export ccall loadPackages :: IO ()

loadPackages :: IO ()
loadPackages = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags { backend = NoBackend
                         , ghcLink  = LinkInMemory }
    pkgs <- setSessionDynFlags dflags'
    hsc_env <- getSession
    liftIO $ Loader.loadPackages hsc_env pkgs
