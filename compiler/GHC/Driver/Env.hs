{-# LANGUAGE CPP #-}

#include "lens.h"

module GHC.Driver.Env
   ( Hsc(..)
   , HscEnv (..)
   , runHsc
   , mkInteractiveHscEnv
   , runInteractiveHsc
   , hscEPS
   , hptCompleteSigs
   , hptAllInstances
   , hptInstancesBelow
   , hptAnns
   , hptAllThings
   , hptSomeThingsBelowUs
   , hptRules
   , prepareAnnotations
   , lookupType
   , lookupIfaceByModule
   , hsc_dflagsL, hsc_targetsL, hsc_mod_graphL, hsc_ICL, hsc_HPTL, hsc_EPSL, hsc_NCL, hsc_FCL, hsc_type_env_varL, hsc_interpL, hsc_loaderL, hsc_pluginsL, hsc_static_pluginsL
   )
where

import GHC.Prelude

import GHC.Driver.Session
import {-# SOURCE #-} GHC.Driver.Plugins
import {-# SOURCE #-} GHC.Driver.Hooks

import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types (Interp)
import GHC.Linker.Types ( Loader )

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo
import GHC.Unit.External
import GHC.Unit.Finder.Types

import GHC.Core         ( CoreRule )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( ClsInst )

import GHC.Types.Annotations ( Annotation, AnnEnv, mkAnnEnv, plusAnnEnv )
import GHC.Types.CompleteMatch
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.Target
import GHC.Types.TypeEnv
import GHC.Types.TyThing

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Unit.Module.Graph

import GHC.Utils.Outputable
import GHC.Utils.Outputable.Ppr
import GHC.Utils.Monad
import GHC.Utils.Monad.RS.Lazy
import GHC.Utils.Error
import GHC.Utils.Misc

import Control.Monad (join)
import Data.Foldable (foldr', toList)
import Data.IORef
import qualified Data.Set as Set
import Data.Set (Set)

-- | The Hsc monad: Passing an environment and warning state
newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))
    deriving (Functor)
    deriving (Applicative, Monad, MonadIO) via RST HscEnv WarningMessages IO

instance HasDynFlags Hsc where
    getDynFlags = Hsc $ \e w -> pure (hsc_dflags e, w)

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env (Hsc hsc) = do
    (a, w) <- hsc hsc_env empty
    a <$ printOrThrowWarnings (hsc_dflags hsc_env) w

-- | Switches in the DynFlags and Plugins from the InteractiveContext
mkInteractiveHscEnv :: HscEnv -> HscEnv
mkInteractiveHscEnv hsc_env = hsc_env
  { hsc_dflags  = ic_dflags  ic
  , hsc_plugins = ic_plugins ic
  }
  where
    ic = hsc_IC hsc_env

-- | A variant of runHsc that switches in the DynFlags and Plugins from the
-- InteractiveContext before running the Hsc computation.
runInteractiveHsc :: HscEnv -> Hsc a -> IO a
runInteractiveHsc hsc_env = runHsc (mkInteractiveHscEnv hsc_env)

-- | HscEnv is like 'GHC.Driver.Monad.Session', except that some of the fields are immutable.
-- An HscEnv is used to compile a single module from plain Haskell source
-- code (after preprocessing) to either C, assembly or C--. It's also used
-- to store the dynamic linker state to allow for multiple linkers in the
-- same address space.
-- Things like the module graph don't change during a single compilation.
--
-- Historical note: \"hsc\" used to be the name of the compiler binary,
-- when there was a separate driver and compiler.  To compile a single
-- module, the driver would invoke hsc on the source code... so nowadays
-- we think of hsc as the layer of the compiler that deals with compiling
-- a single module.
data HscEnv
  = HscEnv {
        hsc_dflags :: DynFlags,
                -- ^ The dynamic flag settings

        hsc_targets :: [Target],
                -- ^ The targets (or roots) of the current session

        hsc_mod_graph :: ModuleGraph,
                -- ^ The module graph of the current session

        hsc_IC :: InteractiveContext,
                -- ^ The context for evaluating interactive statements

        hsc_HPT    :: HomePackageTable,
                -- ^ The home package table describes already-compiled
                -- home-package modules, /excluding/ the module we
                -- are compiling right now.
                -- (In one-shot mode the current module is the only
                -- home-package module, so hsc_HPT is empty.  All other
                -- modules count as \"external-package\" modules.
                -- However, even in GHCi mode, hi-boot interfaces are
                -- demand-loaded into the external-package table.)
                --
                -- 'hsc_HPT' is not mutable because we only demand-load
                -- external packages; the home package is eagerly
                -- loaded, module by module, by the compilation manager.
                --
                -- The HPT may contain modules compiled earlier by @--make@
                -- but not actually below the current module in the dependency
                -- graph.
                --
                -- (This changes a previous invariant: changed Jan 05.)

        hsc_EPS :: {-# UNPACK #-} !(IORef ExternalPackageState),
                -- ^ Information about the currently loaded external packages.
                -- This is mutable because packages will be demand-loaded during
                -- a compilation run as required.

        hsc_NC  :: {-# UNPACK #-} !(IORef NameCache),
                -- ^ As with 'hsc_EPS', this is side-effected by compiling to
                -- reflect sucking in interface files.  They cache the state of
                -- external interface files, in effect.

        hsc_FC   :: {-# UNPACK #-} !(IORef FinderCache),
                -- ^ The cached result of performing finding in the file system

        hsc_type_env_var :: Maybe (Module, IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'GHC.Tc.Utils.tcg_type_env_var' for
                -- 'GHC.Tc.Utils.TcGblEnv'.  See also Note [hsc_type_env_var hack]

        , hsc_interp :: Maybe Interp
                -- ^ target code interpreter (if any) to use for TH and GHCi.
                -- See Note [Target code interpreter]

        , hsc_loader :: Loader
                -- ^ Loader (dynamic linker)

        , hsc_hooks :: !Hooks
                -- ^ Hooks

        , hsc_plugins :: ![LoadedPlugin]
                -- ^ plugins dynamically loaded after processing arguments. What
                -- will be loaded here is directed by DynFlags.pluginModNames.
                -- Arguments are loaded from DynFlags.pluginModNameOpts.
                --
                -- The purpose of this field is to cache the plugins so they
                -- don't have to be loaded each time they are needed.  See
                -- 'GHC.Runtime.Loader.initializePlugins'.

        , hsc_static_plugins :: ![StaticPlugin]
                -- ^ static plugins which do not need dynamic loading. These plugins are
                -- intended to be added by GHC API users directly to this list.
                --
                -- To add dynamically loaded plugins through the GHC API see
                -- 'addPluginModuleName' instead.
 }

LENS_FIELD(hsc_dflagsL, hsc_dflags)
LENS_FIELD(hsc_targetsL, hsc_targets)
LENS_FIELD(hsc_mod_graphL, hsc_mod_graph)
LENS_FIELD(hsc_ICL, hsc_IC)
LENS_FIELD(hsc_HPTL, hsc_HPT)
LENS_FIELD(hsc_EPSL, hsc_EPS)
LENS_FIELD(hsc_NCL, hsc_NC)
LENS_FIELD(hsc_FCL, hsc_FC)
LENS_FIELD(hsc_type_env_varL, hsc_type_env_var)
LENS_FIELD(hsc_interpL, hsc_interp)
LENS_FIELD(hsc_loaderL, hsc_loader)
LENS_FIELD(hsc_pluginsL, hsc_plugins)
LENS_FIELD(hsc_static_pluginsL, hsc_static_plugins)

{-

Note [Target code interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Template Haskell and GHCi use an interpreter to execute code that is built for
the compiler target platform (= code host platform) on the compiler host
platform (= code build platform).

The internal interpreter can be used when both platforms are the same and when
the built code is compatible with the compiler itself (same way, etc.). This
interpreter is not always available: for instance stage1 compiler doesn't have
it because there might be an ABI mismatch between the code objects (built by
stage1 compiler) and the stage1 compiler itself (built by stage0 compiler).

In most cases, an external interpreter can be used instead: it runs in a
separate process and it communicates with the compiler via a two-way message
passing channel. The process is lazily spawned to avoid overhead when it is not
used.

The target code interpreter to use can be selected per session via the
`hsc_interp` field of `HscEnv`. There may be no interpreter available at all, in
which case Template Haskell and GHCi will fail to run. The interpreter to use is
configured via command-line flags (in `GHC.setSessionDynFlags`).


-}

-- Note [hsc_type_env_var hack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- hsc_type_env_var is used to initialize tcg_type_env_var, and
-- eventually it is the mutable variable that is queried from
-- if_rec_types to get a TypeEnv.  So, clearly, it's something
-- related to knot-tying (see Note [Tying the knot]).
-- hsc_type_env_var is used in two places: initTcRn (where
-- it initializes tcg_type_env_var) and initIfaceCheck
-- (where it initializes if_rec_types).
--
-- But why do we need a way to feed a mutable variable in?  Why
-- can't we just initialize tcg_type_env_var when we start
-- typechecking?  The problem is we need to knot-tie the
-- EPS, and we may start adding things to the EPS before type
-- checking starts.
--
-- Here is a concrete example. Suppose we are running
-- "ghc -c A.hs", and we have this file system state:
--
--  A.hs-boot   A.hi-boot **up to date**
--  B.hs        B.hi      **up to date**
--  A.hs        A.hi      **stale**
--
-- The first thing we do is run checkOldIface on A.hi.
-- checkOldIface will call loadInterface on B.hi so it can
-- get its hands on the fingerprints, to find out if A.hi
-- needs recompilation.  But loadInterface also populates
-- the EPS!  And so if compilation turns out to be necessary,
-- as it is in this case, the thunks we put into the EPS for
-- B.hi need to have the correct if_rec_types mutable variable
-- to query.
--
-- If the mutable variable is only allocated WHEN we start
-- typechecking, then that's too late: we can't get the
-- information to the thunks.  So we need to pre-commit
-- to a type variable in 'hscIncrementalCompile' BEFORE we
-- check the old interface.
--
-- This is all a massive hack because arguably checkOldIface
-- should not populate the EPS. But that's a refactor for
-- another day.

-- | Retrieve the ExternalPackageState cache.
hscEPS :: HscEnv -> IO ExternalPackageState
hscEPS hsc_env = readIORef (hsc_EPS hsc_env)

hptCompleteSigs :: HscEnv -> [CompleteMatch]
hptCompleteSigs = hptAllThings  (md_complete_sigs . hm_details)

-- | Find all the instance declarations (of classes and families) from
-- the Home Package Table filtered by the provided predicate function.
-- Used in @tcRnImports@, to select the instances that are in the
-- transitive closure of imports from the currently compiled module.
hptAllInstances :: HscEnv -> ([ClsInst], [FamInst])
hptAllInstances hsc_env
  = let (insts, famInsts) = unzip $ flip hptAllThings hsc_env $ \mod_info -> do
                let details = hm_details mod_info
                return (md_insts details, md_fam_insts details)
    in (join insts, join famInsts)

-- | Find instances visible from the given set of imports
hptInstancesBelow :: HscEnv -> ModuleName -> Set ModuleNameWithIsBoot -> ([ClsInst], [FamInst])
hptInstancesBelow hsc_env mn mns =
  let (insts, famInsts) =
        unzip $ hptSomeThingsBelowUs (\mod_info ->
                                     let details = hm_details mod_info
                                     -- Don't include instances for the current module
                                     in if moduleName (mi_module (hm_iface mod_info)) == mn
                                          then []
                                          else [(md_insts details, md_fam_insts details)])
                             True -- Include -hi-boot
                             hsc_env
                             mns
  in (concat insts, concat famInsts)

-- | Get rules from modules "below" this one (in the dependency sense)
hptRules :: HscEnv -> Set ModuleNameWithIsBoot -> [CoreRule]
hptRules = hptSomeThingsBelowUs (md_rules . hm_details) False


-- | Get annotations from modules "below" this one (in the dependency sense)
hptAnns :: HscEnv -> Maybe (Set ModuleNameWithIsBoot) -> [Annotation]
hptAnns hsc_env (Just deps) = hptSomeThingsBelowUs (md_anns . hm_details) False hsc_env deps
hptAnns hsc_env Nothing = hptAllThings (md_anns . hm_details) hsc_env

hptAllThings :: (HomeModInfo -> [a]) -> HscEnv -> [a]
hptAllThings extract hsc_env = concatMap extract (eltsHpt (hsc_HPT hsc_env))

hptModulesBelow :: HscEnv -> Set ModuleNameWithIsBoot -> Set ModuleNameWithIsBoot
hptModulesBelow hsc_env = filtered_mods . dep_mods Set.empty
  where
    !hpt = hsc_HPT hsc_env

    -- get all the dependent modules without filtering boot/non-boot
    dep_mods !seen !deps -- invariant: intersection of deps and seen is null
      | Set.null deps = seen
      | otherwise     = dep_mods seen' deps'
          where
            get_deps d@(GWIB mod _is_boot) (home_deps,all_deps) = case lookupHpt hpt mod of
              Nothing  -> (home_deps,all_deps) -- not a home-module
              Just hmi -> let
                            !home_deps' = Set.insert d home_deps
                            !all_deps'  = Set.union all_deps (dep_direct_mods (mi_deps (hm_iface hmi)))
                          in (home_deps', all_deps')

            -- all the non-transitive deps from our deps
            (seen',new_deps) = foldr' get_deps (seen,Set.empty) deps

            -- maintain the invariant that deps haven't already been seen
            deps'    = Set.difference new_deps seen'

    -- remove boot modules when there is also a non-boot one
    filtered_mods = Set.fromDistinctAscList . filter_mods . Set.toAscList

    -- IsBoot and NotBoot modules are necessarily consecutive in the sorted list
    -- (cf Ord instance of GenWithIsBoot). Hence we only have to perform a
    -- linear sweep with a window of size 2 to remove boot modules for which we
    -- have the corresponding non-boot.
    filter_mods = \case
      (r1@(GWIB m1 b1) : r2@(GWIB m2 _) : rs)
        | m1 == m2  -> let !r' = case b1 of
                                  NotBoot -> r1
                                  IsBoot  -> r2
                       in r' : filter_mods rs
        | otherwise -> r1 : filter_mods (r2:rs)
      rs -> rs


-- | Get things from modules "below" this one (in the dependency sense)
-- C.f Inst.hptInstances
hptSomeThingsBelowUs :: (HomeModInfo -> [a]) -> Bool -> HscEnv -> Set ModuleNameWithIsBoot -> [a]
hptSomeThingsBelowUs extract include_hi_boot hsc_env deps
  | isOneShot (ghcMode (hsc_dflags hsc_env)) = []

  | otherwise
  = let hpt = hsc_HPT hsc_env
    in
    [ thing
    |   -- Find each non-hi-boot module below me
      GWIB { gwib_mod = mod, gwib_isBoot = is_boot } <- toList (hptModulesBelow hsc_env deps)
    , include_hi_boot || (is_boot == NotBoot)

        -- unsavoury: when compiling the base package with --make, we
        -- sometimes try to look up RULES etc for GHC.Prim. GHC.Prim won't
        -- be in the HPT, because we never compile it; it's in the EPT
        -- instead. ToDo: clean up, and remove this slightly bogus filter:
    , mod /= moduleName gHC_PRIM

        -- Look it up in the HPT
    , let things = case lookupHpt hpt mod of
                    Just info -> extract info
                    Nothing -> pprTrace "WARNING in hptSomeThingsBelowUs" msg []
          msg = vcat [text "missing module" <+> ppr mod,
                      text "Probable cause: out-of-date interface files"]
                        -- This really shouldn't happen, but see #962

        -- And get its dfuns
    , thing <- things ]


-- | Deal with gathering annotations in from all possible places
--   and combining them into a single 'AnnEnv'
prepareAnnotations :: HscEnv -> Maybe ModGuts -> IO AnnEnv
prepareAnnotations hsc_env mb_guts = do
    eps <- hscEPS hsc_env
    let -- Extract annotations from the module being compiled if supplied one
        mb_this_module_anns = fmap (mkAnnEnv . mg_anns) mb_guts
        -- Extract dependencies of the module if we are supplied one,
        -- otherwise load annotations from all home package table
        -- entries regardless of dependency ordering.
        home_pkg_anns  = (mkAnnEnv . hptAnns hsc_env) $ fmap (dep_direct_mods . mg_deps) mb_guts
        other_pkg_anns = eps_ann_env eps
        ann_env        = foldl1' plusAnnEnv $ catMaybes [mb_this_module_anns,
                                                         Just home_pkg_anns,
                                                         Just other_pkg_anns]
    return ann_env

-- | Find the 'TyThing' for the given 'Name' by using all the resources
-- at our disposal: the compiled modules in the 'HomePackageTable' and the
-- compiled modules in other packages that live in 'PackageTypeEnv'. Note
-- that this does NOT look up the 'TyThing' in the module being compiled: you
-- have to do that yourself, if desired
lookupType :: HscEnv -> Name -> IO (Maybe TyThing)
lookupType hsc_env name = do
   eps <- liftIO $ readIORef (hsc_EPS hsc_env)
   let pte = eps_PTE eps
       hpt = hsc_HPT hsc_env

       mod
         | isHoleName name = mkModule (homeUnit (hsc_dflags hsc_env)) (moduleName (nameModule name))
         | otherwise = nameModule name

       !ty
         | isOneShot (ghcMode (hsc_dflags hsc_env)) -- in one-shot, we don't use the HPT
         = lookupNameEnv pte name
         | otherwise = case lookupHptByModule hpt mod of
                Just hm -> lookupNameEnv (md_types (hm_details hm)) name
                Nothing -> lookupNameEnv pte name
   pure ty

-- | Find the 'ModIface' for a 'Module', searching in both the loaded home
-- and external package module information
lookupIfaceByModule
        :: HomePackageTable
        -> PackageIfaceTable
        -> Module
        -> Maybe ModIface
lookupIfaceByModule hpt pit mod
  = case lookupHptByModule hpt mod of
       Just hm -> Just (hm_iface hm)
       Nothing -> lookupModuleEnv pit mod
   -- If the module does come from the home package, why do we look in the PIT as well?
   -- (a) In OneShot mode, even home-package modules accumulate in the PIT
   -- (b) Even in Batch (--make) mode, there is *one* case where a home-package
   --     module is in the PIT, namely GHC.Prim when compiling the base package.
   -- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
   -- of its own, but it doesn't seem worth the bother.

