{-# LANGUAGE CPP #-}

-- | Dynamic linker
module GHC.Linker.Dynamic
   ( linkDynLib
   -- * Platform-specifics
   , libmLinkOpts
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways

import GHC.Driver.Session

import GHC.Unit.Types
import GHC.Unit.State
import GHC.Linker.Unit
import GHC.SysTools.Tasks

import GHC.Data.ShortText as ST

import System.FilePath

linkDynLib :: DynFlags -> [String] -> [UnitId] -> IO ()
linkDynLib dflags o_files dep_packages
 = do
    let platform = targetPlatform dflags
        os = platformOS platform

        verbFlags = getVerbFlags dflags
        o_file = outputFile dflags

    pkgs_with_rts <- getPreloadPackagesAnd dflags dep_packages

    let pkg_lib_paths = collectLibraryPaths (ways dflags) pkgs_with_rts
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | ( osElfTarget (platformOS (targetPlatform dflags)) ||
             osMachOTarget (platformOS (targetPlatform dflags)) ) &&
           dynLibLoader dflags == SystemDependent &&
           -- Only if we want dynamic libraries
           WayDyn `elem` ways dflags &&
           -- Only use RPath if we explicitly asked for it
           gopt Opt_RPath dflags
            = ["-L" ++ l, "-Xlinker", "-rpath", "-Xlinker", l]
              -- See Note [-Xlinker -rpath vs -Wl,-rpath]
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    -- In general we don't want to link our dynamic libs against the RTS
    -- package, because the RTS lib comes in several flavours and we want to be
    -- able to pick the flavour when a binary is linked.
    --
    -- But:
    --   * on Windows we need to link the RTS import lib as Windows does not
    --   allow undefined symbols.
    --
    --   * the RTS library path is still added to the library search path above
    --   in case the RTS is being explicitly linked in (see #3807).
    --
    --   * if -flink-rts is used, we link with the rts.
    --
    let pkgs_sans_rts = filter ((/= rtsUnitId) . mkUnit) pkgs_with_rts
        pkgs
         | OSMinGW32 <- os         = pkgs_with_rts
         | gopt Opt_LinkRts dflags = pkgs_with_rts
         | otherwise               = pkgs_sans_rts
        pkg_link_opts = package_hs_libs ++ extra_libs ++ other_flags
          where (package_hs_libs, extra_libs, other_flags) = collectLinkOpts dflags pkgs

        -- probably _stub.o files
        -- and last temporary shared object file
    let extra_ld_inputs = ldInputs dflags

    case os of
        OSMinGW32 -> do
            -------------------------------------------------------------
            -- Making a DLL
            -------------------------------------------------------------
            let output_fn = case o_file of
                            Just s -> s
                            Nothing -> "HSdll.dll"

            runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    , Option "-shared"
                    ] ++
                    [ FileOption "-Wl,--out-implib=" (output_fn ++ ".a")
                    | gopt Opt_SharedImplib dflags
                    ]
                 ++ map (FileOption "") o_files

                 -- Permit the linker to auto link _symbol to _imp_symbol
                 -- This lets us link against DLLs without needing an "import library"
                 ++ [Option "-Wl,--enable-auto-import"]

                 ++ extra_ld_inputs
                 ++ map Option (
                    lib_path_opts
                 ++ pkg_lib_path_opts
                 ++ (ST.unpack <$> pkg_link_opts)
                ))
        _ -> do
            -------------------------------------------------------------------
            -- Making a DSO
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
                unregisterised = platformUnregisterised (targetPlatform dflags)
            let bsymbolicFlag = -- we need symbolic linking to resolve
                                -- non-PIC intra-package-relocations for
                                -- performance (where symbolic linking works)
                                -- See Note [-Bsymbolic assumptions by GHC]
                                ["-Wl,-Bsymbolic" | not unregisterised]

            runLink dflags (
                    map Option verbFlags
                 ++ libmLinkOpts
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-shared" ]
                 ++ map Option bsymbolicFlag
                    -- Set the library soname. We use -h rather than -soname as
                    -- Solaris 10 doesn't support the latter:
                 ++ [ Option ("-Wl,-h," ++ takeFileName output_fn) ]
                 ++ extra_ld_inputs
                 ++ map Option lib_path_opts
                 ++ map Option pkg_lib_path_opts
                 ++ map Option (ST.unpack <$> pkg_link_opts)
              )

-- | Some platforms require that we explicitly link against @libm@ if any
-- math-y things are used (which we assume to include all programs). See #14022.
libmLinkOpts :: [Option]
libmLinkOpts =
#if defined(HAVE_LIBM)
  [Option "-lm"]
#else
  []
#endif

{-
Note [-Bsymbolic assumptions by GHC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC has a few assumptions about interaction of relocations in NCG and linker:

1. -Bsymbolic resolves internal references when the shared library is linked,
   which is important for performance.
2. When there is a reference to data in a shared library from the main program,
   the runtime linker relocates the data object into the main program using an
   R_*_COPY relocation.
3. If we used -Bsymbolic, then this results in multiple copies of the data
   object, because some references have already been resolved to point to the
   original instance. This is bad!

We work around [3.] for native compiled code by avoiding the generation of
R_*_COPY relocations.

Unregisterised compiler can't evade R_*_COPY relocations easily thus we disable
-Bsymbolic linking there.

See related tickets: #4210, #15338
-}
