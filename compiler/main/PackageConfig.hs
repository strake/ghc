{-# LANGUAGE CPP #-}

-- |
-- Package configuration information: essentially the interface to Cabal, with
-- some utilities
--
-- (c) The University of Glasgow, 2004
--
module PackageConfig (
        -- $package_naming

        -- * PackageKey
        mkPackageKey, packageConfigId,

        -- * The PackageConfig type: information about a package
        PackageConfig,
        InstalledPackageInfo_(..), display,
        Version(..),
        PackageIdentifier(..),
        defaultPackageConfig,
        packageConfigToInstalledPackageInfo,
        installedPackageInfoToPackageConfig
    ) where

#include "HsVersions.h"

import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Package hiding (PackageKey, mkPackageKey)
import qualified Distribution.Package as Cabal
import Distribution.Text
import Distribution.Version

import Maybes
import Module

-- -----------------------------------------------------------------------------
-- Our PackageConfig type is just InstalledPackageInfo from Cabal. Later we
-- might need to extend it with some GHC-specific stuff, but for now it's fine.

type PackageConfig = InstalledPackageInfo_ Module.ModuleName

defaultPackageConfig :: PackageConfig
defaultPackageConfig = emptyInstalledPackageInfo

-- -----------------------------------------------------------------------------
-- PackageKey (package names, versions and dep hash)

-- $package_naming
-- #package_naming#
-- Mostly the compiler deals in terms of 'PackageKey's, which are md5 hashes
-- of a package ID, keys of its dependencies, and Cabal flags. You're expected
-- to pass in the package key in the @-this-package-key@ flag. However, for
-- wired-in packages like @base@ & @rts@, we don't necessarily know what the
-- version is, so these are handled specially; see #wired_in_packages#.

-- | Turn a Cabal 'PackageIdentifier' into a GHC 'PackageKey'
mkPackageKey :: Cabal.PackageKey -> PackageKey
mkPackageKey = stringToPackageKey . display

-- | Get the GHC 'PackageKey' right out of a Cabalish 'PackageConfig'
packageConfigId :: PackageConfig -> PackageKey
packageConfigId = mkPackageKey . packageKey

-- | Turn a 'PackageConfig', which contains GHC 'Module.ModuleName's into a Cabal specific
-- 'InstalledPackageInfo' which contains Cabal 'Distribution.ModuleName.ModuleName's
packageConfigToInstalledPackageInfo :: PackageConfig -> InstalledPackageInfo
packageConfigToInstalledPackageInfo
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     reexportedModules = r,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map convert e,
                 reexportedModules = map (fmap convert) r,
                 hiddenModules  = map convert h }
    where convert :: Module.ModuleName -> Distribution.ModuleName.ModuleName
          convert = (expectJust "packageConfigToInstalledPackageInfo") . simpleParse . moduleNameString

-- | Turn an 'InstalledPackageInfo', which contains Cabal 'Distribution.ModuleName.ModuleName's
-- into a GHC specific 'PackageConfig' which contains GHC 'Module.ModuleName's
installedPackageInfoToPackageConfig :: InstalledPackageInfo_ String -> PackageConfig
installedPackageInfoToPackageConfig
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     reexportedModules = r,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map mkModuleName e,
                 reexportedModules = map (fmap mkModuleName) r,
                 hiddenModules  = map mkModuleName h }

