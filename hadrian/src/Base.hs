module Base (
    -- * General utilities
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.Semigroup,
    module Hadrian.Utilities,

    -- * Shake
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,
    module Development.Shake.Util,

    -- * Basic data types
    module Hadrian.Package,
    module Stage,
    module Way,

    -- * Files
    configH, ghcVersionH,

    -- * Paths
    hadrianPath, configPath, configFile, sourcePath, shakeFilesDir,
    generatedDir, generatedPath, stageBinPath, stageLibPath, templateHscPath,
    ghcDeps, relativePackageDbPath, packageDbPath, packageDbStamp, ghcSplitPath
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.List.Extra
import Data.Maybe
import Data.Semigroup
import Development.Shake hiding (parallel, unit, (*>), Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util
import Hadrian.Utilities
import Hadrian.Package

import Stage
import Way

-- | Hadrian lives in the 'hadrianPath' directory of the GHC tree.
hadrianPath :: FilePath
hadrianPath = "hadrian"

-- TODO: Move this to build directory?
-- | Path to system configuration files, such as 'configFile'.
configPath :: FilePath
configPath = hadrianPath -/- "cfg"

-- | Path to the system configuration file generated by the @configure@ script.
configFile :: FilePath
configFile = configPath -/- "system.config"

-- | Path to source files of the build system, e.g. this file is located at
-- @sourcePath -/- "Base.hs"@. We use this to track some of the source files.
sourcePath :: FilePath
sourcePath = hadrianPath -/- "src"

-- TODO: Change @mk/config.h@ to @shake-build/cfg/config.h@.
-- | Path to the generated @mk/config.h@ file.
configH :: FilePath
configH = "mk/config.h"

ghcVersionH :: Action FilePath
ghcVersionH = generatedPath <&> (-/- "ghcversion.h")

-- | The directory in 'buildRoot' containing the Shake database and other
-- auxiliary files generated by Hadrian.
shakeFilesDir :: FilePath
shakeFilesDir = "hadrian"

-- | The directory in 'buildRoot' containing generated source files that are not
-- package-specific, e.g. @ghcplatform.h@.
generatedDir :: FilePath
generatedDir = "generated"

generatedPath :: Action FilePath
generatedPath = buildRoot <&> (-/- generatedDir)

-- | Path to the package database for a given build stage, relative to the build
-- root. Note that @StageN@, where @N > 1@, uses the 'Stage1' package database.
relativePackageDbPath :: Stage -> FilePath
relativePackageDbPath stage = stageString (min stage Stage1) -/- "lib/package.conf.d"

-- | Path to the package database used in a given 'Stage', including
--   the build root.
packageDbPath :: Stage -> Action FilePath
packageDbPath stage = buildRoot <&> (-/- relativePackageDbPath stage)

-- | We use a stamp file to track the existence of a package database.
packageDbStamp :: FilePath
packageDbStamp = ".stamp"

-- | @bin@ directory for the given 'Stage' (including the build root)
stageBinPath :: Stage -> Action FilePath
stageBinPath stage = buildRoot <&> (-/- stageString stage -/- "bin")

-- | @lib@ directory for the given 'Stage' (including the build root)
stageLibPath :: Stage -> Action FilePath
stageLibPath stage = buildRoot <&> (-/- stageString stage -/- "lib")

-- | Files the `ghc` binary depends on
ghcDeps :: Stage -> Action [FilePath]
ghcDeps stage = mapM (\f -> stageLibPath stage <&> (-/- f))
      [ "ghc-usage.txt"
      , "ghci-usage.txt"
      , "llvm-targets"
      , "llvm-passes"
      , "platformConstants"
      , "settings" ]

-- ref: utils/hsc2hs/ghc.mk
-- | Path to 'hsc2hs' template.
templateHscPath :: Stage -> Action FilePath
templateHscPath stage = stageLibPath stage <&> (-/- "template-hsc.h")

-- | @ghc-split@ is a Perl script used by GHC when run with @-split-objs@ flag.
-- It is generated in "Rules.Generate". This function returns the path relative
-- to the build root under which we will copy @ghc-split@.
ghcSplitPath :: Stage -> FilePath
ghcSplitPath stage = stageString stage -/- "bin" -/- "ghc-split"
