{-# LANGUAGE OverloadedStrings #-}

-- | Linking Haskell units
module GHC.Linker.Unit
   ( collectLinkOpts
   , collectArchives
   , collectLibraryPaths
   , getPackageLinkOpts
   , getPackageLibraryPath
   , getLibs
   , packageHsLibs
   )
where

import GHC.Prelude
import GHC.Platform.Ways
import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Data.ShortText (ShortText)
import qualified GHC.Data.ShortText as ST

import GHC.Driver.Session

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import System.Directory
import System.FilePath

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [UnitId] -> IO [FilePath]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths (ways dflags) <$> getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: Set Way -> [UnitInfo] -> [FilePath]
collectLibraryPaths ways = ordNub . filter notNull . concatMap (libraryDirsForWay ways)

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [UnitId] -> IO ([ShortText], [ShortText], [ShortText])
getPackageLinkOpts dflags pkgs =
  collectLinkOpts dflags <$> getPreloadPackagesAnd dflags pkgs

collectLinkOpts :: DynFlags -> [UnitInfo] -> ([ShortText], [ShortText], [ShortText])
collectLinkOpts dflags ps =
  ( concatMap (map ("-l" <>) . packageHsLibs dflags) ps
  , concatMap (map ("-l" <>) . unitExtDepLibsSys) ps
  , concatMap unitLinkerOptions ps
  )

collectArchives :: DynFlags -> UnitInfo -> IO [FilePath]
collectArchives dflags pc =
  filterM doesFileExist
  [ searchPath </> "lib" ++ ST.unpack lib ++ ".a"
  | searchPath <- searchPaths
  , lib <- libs ]
  where
    searchPaths = ordNub . filter notNull . libraryDirsForWay (ways dflags) $ pc
    libs        = packageHsLibs dflags pc <> unitExtDepLibsSys pc

getLibs :: DynFlags -> [UnitId] -> IO [(ShortText,ShortText)]
getLibs dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  fmap fold . forM ps $ \p -> do
    let candidates =
          [ (ST.pack l <> "/" <> f, f)
          | l <- collectLibraryPaths (ways dflags) [p]
          , f <- ["lib" <> n <> ".a" | n <- packageHsLibs dflags p] ]
    filterM (doesFileExist . ST.unpack . fst) candidates

packageHsLibs :: DynFlags -> UnitInfo -> [ShortText]
packageHsLibs dflags p = map (mkDynName . addSuffix) (unitLibraries p)
  where
        ways0 = ways dflags

        ways1 = Set.filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug and profiled RTSs include support for -eventlog
        ways2 | WayDebug `Set.member` ways1 || WayProf `Set.member` ways1
              = Set.filter (/= WayEventLog) ways1
              | otherwise
              = ways1

        tag     = ST.pack $ waysTag (Set.filter (not . wayRTSOnly) ways2)
        rts_tag = ST.pack $ waysTag ways2

        mkDynName :: ShortText -> ShortText
        mkDynName x
         | WayDyn `Set.notMember` ways dflags = x
         | Just _ <- ST.stripPrefix "HS" x =
              x <> ST.pack ('-' : programName dflags ++ projectVersion dflags)
           -- For non-Haskell libraries, we use the name "Cfoo". The .a
           -- file is libCfoo.a, and the .so is libfoo.so. That way the
           -- linker knows what we mean for the vanilla (-lCfoo) and dyn
           -- (-lfoo) ways. We therefore need to strip the 'C' off here.
         | Just x' <- ST.stripPrefix "C" x = x'
         | otherwise
            = panic ("Don't understand library name " ++ ST.unpack x)

        -- Add _thr and other rts suffixes to packages named
        -- `rts` or `rts-1.0`. Why both?  Traditionally the rts
        -- package is called `rts` only.  However the tooling
        -- usually expects a package name to have a version.
        -- As such we will gradually move towards the `rts-1.0`
        -- package name, at which point the `rts` package name
        -- will eventually be unused.
        --
        -- This change elevates the need to add custom hooks
        -- and handling specifically for the `rts` package for
        -- example in ghc-cabal.
        addSuffix :: ShortText -> ShortText
        addSuffix rts@"HSrts"    = rts       <> expandTag rts_tag
        addSuffix rts@"HSrts-1.0"= rts       <> expandTag rts_tag
        addSuffix other_lib      = other_lib <> expandTag tag

        expandTag :: ShortText -> ShortText
        expandTag t | ST.null t = ""
                    | otherwise = "_" <> t

-- | Either the 'unitLibraryDirs' or 'unitLibraryDynDirs' as appropriate for the way.
libraryDirsForWay :: Foldable f => f Way -> UnitInfo -> [FilePath]
libraryDirsForWay ways
  | WayDyn `elem` ways = fmap ST.unpack . unitLibraryDynDirs
  | otherwise          = fmap ST.unpack . unitLibraryDirs
