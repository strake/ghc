-- (c) The University of Glasgow 2002-2006

{-# LANGUAGE RankNTypes, BangPatterns #-}

module GHC.Iface.Env (
        newGlobalBinder, newInteractiveBinder,
        externaliseName,
        lookupIfaceTop,
        lookupOrig, lookupOrigIO, lookupOrigNameCache, extendNameCache,
        newIfaceName, newIfaceNames,
        extendIfaceIdEnv, extendIfaceTyVarEnv,
        tcIfaceLclId, tcIfaceTyVar, lookupIfaceVar,
        lookupIfaceTyVar, extendIfaceEnvs,
        setNameModule,

        ifaceExportNames,

        -- Name-cache stuff
        allocateGlobalBinder, updNameCacheTc, updNameCache,
        mkNameCacheUpdater, NameCacheUpdater(..),
   ) where

import GHC.Prelude

import GHC.Driver.Env

import GHC.Tc.Utils.Monad
import GHC.Core.Type
import GHC.Iface.Type
import GHC.Runtime.Context

import GHC.Unit.Module
import GHC.Unit.Module.ModIface

import GHC.Data.FastString
import GHC.Data.FastString.Env

import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Name.Cache
import GHC.Types.Unique.Supply
import GHC.Types.SrcLoc

import GHC.Utils.Lens.Monad
import GHC.Utils.Outputable
import Data.IORef

{-
*********************************************************
*                                                      *
        Allocating new Names in the Name Cache
*                                                      *
*********************************************************

See Also: Note [The Name Cache] in GHC.Types.Name.Cache
-}

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
-- Used for source code and interface files, to make the
-- Name for a thing, given its Module and OccName
-- See Note [The Name Cache]
--
-- The cache may already have a binding for this thing,
-- because we may have seen an occurrence before, but now is the
-- moment when we know its Module and SrcLoc in their full glory

newGlobalBinder mod occ loc
  = do { name <- updNameCacheTc mod occ $ \name_cache ->
                 allocateGlobalBinder name_cache mod occ loc
       ; name <$ traceIf (text "newGlobalBinder" <+>
                  (vcat [ ppr mod <+> ppr occ <+> ppr loc, ppr name])) }

newInteractiveBinder :: HscEnv -> OccName -> SrcSpan -> IO Name
-- Works in the IO monad, and gets the Module
-- from the interactive context
newInteractiveBinder hsc_env occ loc
 = do { let mod = icInteractiveModule (hsc_IC hsc_env)
       ; updNameCacheIO hsc_env mod occ $ \name_cache ->
         allocateGlobalBinder name_cache mod occ loc }

allocateGlobalBinder
  :: NameCache
  -> Module -> OccName -> SrcSpan
  -> (NameCache, Name)
-- See Note [The Name Cache]
allocateGlobalBinder name_supply mod occ loc
  = case lookupOrigNameCache (nsNames name_supply) mod occ of
        -- A hit in the cache!  We are at the binding site of the name.
        -- This is the moment when we know the SrcLoc
        -- of the Name, so we set this field in the Name we return.
        --
        -- Then (bogus) multiple bindings of the same Name
        -- get different SrcLocs can be reported as such.
        --
        -- Possible other reason: it might be in the cache because we
        --      encountered an occurrence before the binding site for an
        --      implicitly-imported Name.  Perhaps the current SrcLoc is
        --      better... but not really: it'll still just say 'imported'
        --
        -- IMPORTANT: Don't mess with wired-in names.
        --            Their wired-in-ness is in their NameSort
        --            and their Module is correct.

        Just name | isWiredInName name
                  -> (name_supply, name)
                  | otherwise
                  -> (new_name_supply, name')
                  where
                    uniq            = nameUnique name
                    name'           = mkExternalName uniq mod occ loc
                                      -- name' is like name, but with the right SrcSpan
                    new_cache       = extendNameCache (nsNames name_supply) mod occ name'
                    new_name_supply = name_supply {nsNames = new_cache}

        -- Miss in the cache!
        -- Build a completely new Name, and put it in the cache
        _ -> (new_name_supply, name)
                  where
                    (uniq, us')     = takeUniqFromSupply (nsUniqs name_supply)
                    name            = mkExternalName uniq mod occ loc
                    new_cache       = extendNameCache (nsNames name_supply) mod occ name
                    new_name_supply = name_supply {nsUniqs = us', nsNames = new_cache}

ifaceExportNames :: [IfaceExport] -> TcRnIf gbl lcl [AvailInfo]
ifaceExportNames exports = return exports

-- | A function that atomically updates the name cache given a modifier
-- function.  The second result of the modifier function will be the result
-- of the IO action.
newtype NameCacheUpdater
      = NCU { updateNameCache :: forall c. (NameCache -> (NameCache, c)) -> IO c }

mkNameCacheUpdater :: TcRnIf a b NameCacheUpdater
mkNameCacheUpdater = do { hsc_env <- getTopEnv
                        ; let !ncRef = hsc_NC hsc_env
                        ; return (NCU (updNameCache ncRef)) }

updNameCacheTc :: Module -> OccName -> (NameCache -> (NameCache, c))
               -> TcRnIf a b c
updNameCacheTc mod occ upd_fn = do {
    hsc_env <- getTopEnv
  ; liftIO $ updNameCacheIO hsc_env mod occ upd_fn }


updNameCacheIO ::  HscEnv -> Module -> OccName
               -> (NameCache -> (NameCache, c))
               -> IO c
updNameCacheIO hsc_env mod occ upd_fn = do {

    -- First ensure that mod and occ are evaluated
    -- If not, chaos can ensue:
    --      we read the name-cache
    --      then pull on mod (say)
    --      which does some stuff that modifies the name cache
    -- This did happen, with tycon_mod in GHC.IfaceToCore.tcIfaceAlt (DataAlt..)

    mod `seq` occ `seq` return ()
  ; updNameCache (hsc_NC hsc_env) upd_fn }


{-
************************************************************************
*                                                                      *
                Name cache access
*                                                                      *
************************************************************************
-}

-- | Look up the 'Name' for a given 'Module' and 'OccName'.
-- Consider alternatively using 'lookupIfaceTop' if you're in the 'IfL' monad
-- and 'Module' is simply that of the 'ModIface' you are typechecking.
lookupOrig :: Module -> OccName -> TcRnIf a b Name
lookupOrig mod occ
  = do  { traceIf (text "lookup_orig" <+> ppr mod <+> ppr occ)

        ; updNameCacheTc mod occ $ lookupNameCache mod occ }

lookupOrigIO :: HscEnv -> Module -> OccName -> IO Name
lookupOrigIO hsc_env mod occ
  = updNameCacheIO hsc_env mod occ $ lookupNameCache mod occ

lookupNameCache :: Module -> OccName -> NameCache -> (NameCache, Name)
-- Lookup up the (Module,OccName) in the NameCache
-- If you find it, return it; if not, allocate a fresh original name and extend
-- the NameCache.
-- Reason: this may the first occurrence of (say) Foo.bar we have encountered.
-- If we need to explore its value we will load Foo.hi; but meanwhile all we
-- need is a Name for it.
lookupNameCache mod occ name_cache =
  case lookupOrigNameCache (nsNames name_cache) mod occ of {
    Just name -> (name_cache, name);
    Nothing   ->
        case takeUniqFromSupply (nsUniqs name_cache) of {
          (uniq, us) ->
              let
                name      = mkExternalName uniq mod occ noSrcSpan
                new_cache = extendNameCache (nsNames name_cache) mod occ name
              in (name_cache{ nsUniqs = us, nsNames = new_cache }, name) }}

externaliseName :: Module -> Name -> TcRnIf m n Name
-- Take an Internal Name and make it an External one,
-- with the same unique
externaliseName mod name
  = do { let occ = nameOccName name
             loc = nameSrcSpan name
             uniq = nameUnique name
       ; occ `seq` return ()  -- c.f. seq in newGlobalBinder
       ; updNameCacheTc mod occ $ \ ns ->
         let name' = mkExternalName uniq mod occ loc
             ns'   = ns { nsNames = extendNameCache (nsNames ns) mod occ name' }
         in (ns', name') }

-- | Set the 'Module' of a 'Name'.
setNameModule :: Maybe Module -> Name -> TcRnIf m n Name
setNameModule Nothing n = return n
setNameModule (Just m) n =
    newGlobalBinder m (nameOccName n) (nameSrcSpan n)

{-
************************************************************************
*                                                                      *
                Type variables and local Ids
*                                                                      *
************************************************************************
-}

tcIfaceLclId :: FastString -> IfL Id
tcIfaceLclId occ = flip lookupFsEnv occ . if_id_env <$> getLclEnv >>= \ case
    Just ty_var -> return ty_var
    Nothing     -> failIfM (text "Iface id out of scope: " <+> ppr occ)

extendIfaceIdEnv :: [Id] -> IfL a -> IfL a
extendIfaceIdEnv ids
  = locally (env_lclL . if_id_envL) $
    flip extendFsEnvList [(occNameFS (getOccName id), id) | id <- ids]


tcIfaceTyVar :: FastString -> IfL TyVar
tcIfaceTyVar occ
  = do  { lcl <- getLclEnv
        ; case lookupFsEnv (if_tv_env lcl) occ of
            Just ty_var -> return ty_var
            Nothing     -> failIfM (text "Iface type variable out of scope: " <+> ppr occ)
        }

lookupIfaceTyVar :: IfaceTvBndr -> IfL (Maybe TyVar)
lookupIfaceTyVar (occ, _) = flip lookupFsEnv occ <$> view (env_lclL . if_tv_envL)

lookupIfaceVar :: IfaceBndr -> IfL (Maybe TyCoVar)
lookupIfaceVar = \ case
    IfaceIdBndr bndr -> lookupIfaceTyVar bndr
    IfaceTvBndr bndr -> lookupIfaceTyVar bndr

extendIfaceTyVarEnv :: [TyVar] -> IfL a -> IfL a
extendIfaceTyVarEnv tyvars
  = locally (env_lclL . if_tv_envL) $
    flip extendFsEnvList [(occNameFS (getOccName tv), tv) | tv <- tyvars]

extendIfaceEnvs :: [TyCoVar] -> IfL a -> IfL a
extendIfaceEnvs tcvs = extendIfaceTyVarEnv tvs . extendIfaceIdEnv cvs
  where
    (tvs, cvs) = partition isTyVar tcvs

{-
************************************************************************
*                                                                      *
                Getting from RdrNames to Names
*                                                                      *
************************************************************************
-}

-- | Look up a top-level name from the current Iface module
lookupIfaceTop :: OccName -> IfL Name
lookupIfaceTop occ
  = do  { env <- getLclEnv; lookupOrig (if_mod env) occ }

newIfaceName :: OccName -> IfL Name
newIfaceName occ
  = do  { uniq <- newUnique
        ; return $! mkInternalName uniq occ noSrcSpan }

newIfaceNames :: Traversable t => t OccName -> IfL (t Name)
newIfaceNames occs = newUniqueSupply <???> \ uniqs ->
  withUniques (\ uniq occ -> mkInternalName uniq occ noSrcSpan) uniqs occs

{-
Names in a NameCache are always stored as a Global, and have the SrcLoc
of their binding locations.

Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.
-}

updNameCache :: IORef NameCache
             -> (NameCache -> (NameCache, c))  -- The updating function
             -> IO c
updNameCache = atomicModifyIORef'

