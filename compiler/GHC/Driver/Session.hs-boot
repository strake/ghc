module GHC.Driver.Session where

import GHC.Prelude
import GHC.Platform
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.State

data DynFlags

targetPlatform           :: DynFlags -> Platform
pkgState                 :: DynFlags -> PackageState
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
initSDocContext          :: DynFlags -> PprStyle -> SDocContext
