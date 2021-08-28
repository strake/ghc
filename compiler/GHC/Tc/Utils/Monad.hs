{-# LANGUAGE ExplicitForAll, FlexibleInstances, BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

-- | Functions for working with the typechecker environment (setters,
-- getters...).
module GHC.Tc.Utils.Monad(
  -- * Initialisation
  initTc, initTcWithGbl, initTcInteractive, initTcRnIf,

  -- * Simple accessors
  getTopEnv, updTopEnv, getGblEnv, updGblEnv,
  setGblEnv, getLclEnv, updLclEnv, setLclEnv,
  getEnvs, setEnvs,
  xoptM, doptM, goptM, woptM,
  setXOptM, unsetXOptM, unsetGOptM, unsetWOptM,
  whenDOptM, whenGOptM, whenWOptM,
  whenXOptM, unlessXOptM,
  getGhcMode,
  withDoDynamicToo,
  getEpsVar,
  getEps,
  updateEps, updateEps_,
  getHpt, getEpsAndHpt,

  -- * Arrow scopes
  newArrowScope, escapeArrowScope,

  -- * Unique supply
  newUnique, newUniqueSupply, newName, newNameAt, cloneLocalName,
  newSysName, newSysLocalId, newSysLocalIds,

  -- * Debugging
  traceTc, traceRn, traceOptTcRn, dumpOptTcRn,
  dumpTcRn,
  getPrintUnqualified,
  printForUserTcRn,
  traceIf, traceHiDiffs, traceOptIf,
  debugTc,

  -- * Typechecker global environment
  getIsGHCi, getGHCiMonad, getInteractivePrintName,
  tcIsHsBootOrSig, tcIsHsig, tcSelfBootInfo, getGlobalRdrEnv,
  getRdrEnvs, getImports,
  getFixityEnv, extendFixityEnv, getRecFieldEnv,
  getDeclaredDefaultTys,
  addDependentFiles,

  -- * Error management
  getSrcSpanM, setSrcSpan, addLocM,
  wrapLocM, wrapLocFstM, wrapLocSndM,wrapLocM_,
  getErrsVar, setErrsVar,
  addErr,
  failWith, failAt,
  addErrAt, addErrs,
  checkErr,
  addMessages,
  discardWarnings,

  -- * Shared error message stuff: renamer and typechecker
  mkLongErrAt, mkErrDocAt, addLongErrAt, reportError,
  reportWarning, recoverM, mapAndRecoverM, mapAndReportM,
  attemptM, tryTc,
  askNoErrs, discardErrs, tryTcDiscardingErrs,
  checkNoErrs, whenNoErrs,
  ifErrsM, failIfErrsM,

  -- * Context management for the type checker
  getErrCtxt, setErrCtxt, addErrCtxt, addErrCtxtM, addLandmarkErrCtxt,
  addLandmarkErrCtxtM, popErrCtxt, getCtLocM, setCtLocM,

  -- * Error message generation (type checker)
  addErrTc,
  addErrTcM,
  failWithTc, failWithTcM,
  checkTc, checkTcM,
  failIfTc, failIfTcM,
  warnIfFlag, warnIfFlagAt, warnIf, warnTc, warnTcM,
  addWarnTc, addWarnTcM, addWarn, addWarnAt, add_warn,
  mkErrInfo,

  -- * Type constraints
  newTcEvBinds, newNoTcEvBinds, cloneEvBindsVar,
  addTcEvBind, addTopEvBinds,
  getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
  chooseUniqueOccTc,
  getConstraintVar, setConstraintVar,
  emitConstraints, emitStaticConstraints, emitSimple, emitSimples,
  emitImplication, emitImplications, emitInsoluble, emitHole,
  discardConstraints, captureConstraints, tryCaptureConstraints,
  pushLevelAndCaptureConstraints,
  pushTcLevelM_, pushTcLevelM,
  getTcLevel, setTcLevel, isTouchableTcM,
  getLclTypeEnv,
  traceTcConstraints,
  emitNamedTypeHole, emitAnonTypeHole,

  -- * Template Haskell context
  recordThUse, recordThSpliceUse,
  keepAlive, getStage, getStageAndBindLevel, setStage,
  addModFinalizersWithLclEnv,

  -- * Safe Haskell context
  recordUnsafeInfer, finalSafeMode, fixSafeInstances,

  -- * Stuff for the renamer's local env
  getLocalRdrEnv, setLocalRdrEnv,

  -- * Stuff for interface decls
  mkIfLclEnv,
  initIfaceTcRn,
  initIfaceCheck,
  initIfaceLcl,
  initIfaceLclWithSubst,
  initIfaceLoad,
  getIfModule,
  failIfM,
  forkM_maybe,
  forkM,
  setImplicitEnvM,

  withException,

  -- * Stuff for cost centres.
  getCCIndexM, getCCIndexTcM,

  -- * Types etc.
  module GHC.Tc.Types,
  module GHC.Data.IOEnv
  ) where

import GHC.Prelude


import GHC.Builtin.Names

import GHC.Tc.Types     -- Re-export all
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType

import GHC.Hs hiding (LIE)

import GHC.Unit
import GHC.Unit.State
import GHC.Unit.External
import GHC.Unit.Module.Warnings
import GHC.Unit.Home.ModInfo

import GHC.Core.InstEnv
import GHC.Core.FamInstEnv

import GHC.Driver.Env
import GHC.Driver.Ppr
import GHC.Driver.Session

import GHC.Runtime.Context

import GHC.Data.IOEnv -- Re-export all
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe
import qualified GHC.Data.Strict as Strict

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Constants ( debugIsOn )
import GHC.Utils.Lens.Monad

import GHC.Types.Fixity.Env
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.SafeHaskell
import GHC.Types.Id
import GHC.Types.TypeEnv
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.SrcLoc
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Ppr
import GHC.Types.Unique.Supply
import GHC.Types.Annotations
import GHC.Types.Basic( TopLevelFlag, TypeOrKind(..) )
import GHC.Types.CostCentre.State
import GHC.Types.SourceFile

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Monad.Trans.State (StateT (..))
import Data.Functor.Reader.Class
import Data.IORef
import qualified Data.Map as Map
import Lens.Micro (_1)

import {-# SOURCE #-} GHC.Tc.Utils.Env    ( tcInitTidyEnv )

{-
************************************************************************
*                                                                      *
                        initTc
*                                                                      *
************************************************************************
-}

-- | Setup the initial typechecking environment
initTc :: HscEnv
       -> HscSource
       -> Bool          -- True <=> retain renamed syntax trees
       -> Module
       -> RealSrcSpan
       -> TcM r
       -> IO (Messages, Maybe r)
                -- Nothing => error thrown by the thing inside
                -- (error messages should have been printed already)

initTc hsc_env hsc_src keep_rn_syntax mod loc do_this
 = do { keep_var     <- newIORef emptyNameSet ;
        used_gre_var <- newIORef [] ;
        th_var       <- newIORef False ;
        th_splice_var<- newIORef False ;
        infer_var    <- newIORef (True, empty) ;
        dfun_n_var   <- newIORef emptyOccSet ;
        type_env_var <- case hsc_type_env_var hsc_env of {
                           Just (_mod, te_var) -> return te_var ;
                           Nothing             -> newIORef emptyNameEnv } ;

        dependent_files_var <- newIORef [] ;
        static_wc_var       <- newIORef emptyWC ;
        cc_st_var           <- newIORef newCostCentreState ;
        th_topdecls_var      <- newIORef [] ;
        th_foreign_files_var <- newIORef [] ;
        th_topnames_var      <- newIORef emptyNameSet ;
        th_modfinalizers_var <- newIORef [] ;
        th_coreplugins_var <- newIORef [] ;
        th_state_var         <- newIORef Map.empty ;
        th_remote_state_var  <- newIORef Nothing ;
        let {
             dflags = hsc_dflags hsc_env ;

             maybe_rn_syntax :: forall a. a -> Maybe a ;
             maybe_rn_syntax empty_val
                | dopt Opt_D_dump_rn_ast dflags = Just empty_val

                | gopt Opt_WriteHie dflags       = Just empty_val

                  -- We want to serialize the documentation in the .hi-files,
                  -- and need to extract it from the renamed syntax first.
                  -- See 'GHC.HsToCore.Docs.extractDocs'.
                | gopt Opt_Haddock dflags       = Just empty_val

                | keep_rn_syntax                = Just empty_val
                | otherwise                     = Nothing ;

             gbl_env = TcGblEnv {
                tcg_th_topdecls      = th_topdecls_var,
                tcg_th_foreign_files = th_foreign_files_var,
                tcg_th_topnames      = th_topnames_var,
                tcg_th_modfinalizers = th_modfinalizers_var,
                tcg_th_coreplugins = th_coreplugins_var,
                tcg_th_state         = th_state_var,
                tcg_th_remote_state  = th_remote_state_var,

                tcg_mod            = mod,
                tcg_semantic_mod   =
                    canonicalizeModuleIfHome dflags mod,
                tcg_src            = hsc_src,
                tcg_rdr_env        = emptyGlobalRdrEnv,
                tcg_fix_env        = emptyNameEnv,
                tcg_field_env      = emptyNameEnv,
                tcg_default        = if moduleUnit mod == primUnitId
                                     || moduleUnit mod == bignumUnitId
                                     then Just []  -- See Note [Default types]
                                     else Nothing,
                tcg_type_env       = emptyNameEnv,
                tcg_type_env_var   = type_env_var,
                tcg_inst_env       = emptyInstEnv,
                tcg_fam_inst_env   = emptyFamInstEnv,
                tcg_ann_env        = emptyAnnEnv,
                tcg_th_used        = th_var,
                tcg_th_splice_used = th_splice_var,
                tcg_exports        = [],
                tcg_imports        = emptyImportAvails,
                tcg_used_gres     = used_gre_var,
                tcg_dus            = emptyDUs,

                tcg_rn_imports     = [],
                tcg_rn_exports     =
                    if hsc_src == HsigFile
                        -- Always retain renamed syntax, so that we can give
                        -- better errors.  (TODO: how?)
                        then Just []
                        else maybe_rn_syntax [],
                tcg_rn_decls       = maybe_rn_syntax emptyRnGroup,
                tcg_tr_module      = Nothing,
                tcg_binds          = emptyLHsBinds,
                tcg_imp_specs      = [],
                tcg_sigs           = emptyNameSet,
                tcg_ev_binds       = empty,
                tcg_warns          = NoWarnings,
                tcg_anns           = [],
                tcg_tcs            = [],
                tcg_insts          = [],
                tcg_fam_insts      = [],
                tcg_rules          = [],
                tcg_fords          = [],
                tcg_patsyns        = [],
                tcg_merged         = [],
                tcg_dfun_n         = dfun_n_var,
                tcg_keep           = keep_var,
                tcg_doc_hdr        = Nothing,
                tcg_hpc            = False,
                tcg_main           = Nothing,
                tcg_self_boot      = NoSelfBoot,
                tcg_safeInfer      = infer_var,
                tcg_dependent_files = dependent_files_var,
                tcg_tc_plugins     = [],
                tcg_hf_plugins     = [],
                tcg_top_loc        = loc,
                tcg_static_wc      = static_wc_var,
                tcg_complete_matches = [],
                tcg_cc_st          = cc_st_var
             } ;
        } ;

        -- OK, here's the business end!
        initTcWithGbl hsc_env gbl_env loc do_this
    }

-- | Run a 'TcM' action in the context of an existing 'GblEnv'.
initTcWithGbl :: HscEnv
              -> TcGblEnv
              -> RealSrcSpan
              -> TcM r
              -> IO (Messages, Maybe r)
initTcWithGbl hsc_env gbl_env loc do_this
 = do { lie_var      <- newIORef emptyWC
      ; errs_var     <- newIORef (empty, empty)
      ; let lcl_env = TcLclEnv {
                tcl_errs       = errs_var,
                tcl_loc        = loc,     -- Should be over-ridden very soon!
                tcl_ctxt       = [],
                tcl_rdr        = emptyLocalRdrEnv,
                tcl_th_ctxt    = topStage,
                tcl_th_bndrs   = emptyNameEnv,
                tcl_arrow_ctxt = NoArrowCtxt,
                tcl_env        = emptyNameEnv,
                tcl_bndrs      = [],
                tcl_lie        = lie_var,
                tcl_tclvl      = topTcLevel
                }

      ; maybe_res <- initTcRnIf 'a' hsc_env gbl_env lcl_env $
                     do { r <- tryM do_this
                        ; case r of
                          Right res -> return (Just res)
                          Left _    -> return Nothing }

      -- Check for unsolved constraints
      -- If we succeed (maybe_res = Just r), there should be
      -- no unsolved constraints.  But if we exit via an
      -- exception (maybe_res = Nothing), we may have skipped
      -- solving, so don't panic then (#13466)
      ; lie <- readIORef (tcl_lie lcl_env)
      ; when (isJust maybe_res && not (isEmptyWC lie)) $
        pprPanic "initTc: unsolved constraints" (ppr lie)

        -- Collect any error messages
      ; msgs <- readIORef (tcl_errs lcl_env)

      ; let { final_res | errorsFound dflags msgs = Nothing
                        | otherwise               = maybe_res }

      ; return (msgs, final_res)
      }
  where dflags = hsc_dflags hsc_env

initTcInteractive :: HscEnv -> TcM a -> IO (Messages, Maybe a)
-- Initialise the type checker monad for use in GHCi
initTcInteractive hsc_env thing_inside
  = initTc hsc_env HsSrcFile False
           (icInteractiveModule (hsc_IC hsc_env))
           (realSrcLocSpan interactive_src_loc)
           thing_inside
  where
    interactive_src_loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

{- Note [Default types]
~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is simply not available in ghc-prim and ghc-bignum packages (it
is declared in ghc-bignum). So we set the defaulting types to (Just []), meaning
there are no default types, rather than Nothing, which means "use the default
default types of Integer, Double".

If you don't do this, attempted defaulting in package ghc-prim causes
an actual crash (attempting to look up the Integer type).


************************************************************************
*                                                                      *
                Initialisation
*                                                                      *
************************************************************************
-}

initTcRnIf :: Char              -- ^ Mask for unique supply
           -> HscEnv
           -> gbl -> lcl
           -> TcRnIf gbl lcl a
           -> IO a
initTcRnIf uniq_mask hsc_env gbl_env lcl_env thing_inside
   = do { let { env = Env { env_top = hsc_env,
                            env_um  = uniq_mask,
                            env_gbl = gbl_env,
                            env_lcl = lcl_env} }

        ; runIOEnv env thing_inside
        }

{-
************************************************************************
*                                                                      *
                Simple accessors
*                                                                      *
************************************************************************
-}

getTopEnv :: (IsReader f, EnvType f ~ Env gbl lcl) => f HscEnv
getTopEnv = view env_topL

updTopEnv :: (IsLocal f f, EnvType f ~ Env gbl lcl) => (HscEnv -> HscEnv) -> f a -> f a
updTopEnv = locally env_topL

getGblEnv :: (IsReader m, EnvType m ~ Env gbl lcl) => m gbl
getGblEnv = view env_gblL

updGblEnv :: (IsLocal f g, EnvType f ~ Env gbl lcl, EnvType g ~ Env gbl' lcl) => (gbl' -> gbl) -> f a -> g a
updGblEnv = locally env_gblL

setGblEnv :: (IsLocal f g, EnvType f ~ Env gbl lcl, EnvType g ~ Env gbl' lcl) => gbl -> f a -> g a
setGblEnv = updGblEnv . pure

getLclEnv :: (IsReader m, EnvType m ~ Env gbl lcl) => m lcl
getLclEnv = view env_lclL

updLclEnv :: (IsLocal f g, EnvType f ~ Env gbl lcl, EnvType g ~ Env gbl lcl') => (lcl' -> lcl) -> f a -> g a
updLclEnv = locally env_lclL

setLclEnv :: (IsLocal f g, EnvType f ~ Env gbl lcl, EnvType g ~ Env gbl lcl') => lcl -> f a -> g a
setLclEnv = updLclEnv . pure

getEnvs :: (IsReader m, EnvType m ~ Env gbl lcl) => m (gbl, lcl)
getEnvs = (,) <$> env_gbl <*> env_lcl <$> ask

setEnvs :: (IsLocal f g, EnvType f ~ Env gbl lcl, EnvType g ~ Env gbl' lcl') => (gbl, lcl) -> f a -> g a
setEnvs (gbl_env, lcl_env) = local \ env -> env { env_gbl = gbl_env, env_lcl = lcl_env }

-- Command-line flags

xoptM :: (Functor m, HasDynFlags m) => LangExt.Extension -> m Bool
xoptM flag = xopt flag <$> getDynFlags

doptM :: (Functor m, HasDynFlags m) => DumpFlag -> m Bool
doptM flag = dopt flag <$> getDynFlags

goptM :: (Functor m, HasDynFlags m) => GeneralFlag -> m Bool
goptM flag = gopt flag <$> getDynFlags

woptM :: (Functor m, HasDynFlags m) => WarningFlag -> m Bool
woptM flag = wopt flag <$> getDynFlags

setXOptM :: (IsLocal f f, EnvType f ~ Env gbl lcl) => LangExt.Extension -> f a -> f a
setXOptM flag = locally (env_topL . hsc_dflagsL) (`xopt_set` flag)

unsetXOptM :: (IsLocal f f, EnvType f ~ Env gbl lcl) => LangExt.Extension -> f a -> f a
unsetXOptM flag = locally (env_topL . hsc_dflagsL) (`xopt_unset` flag)

unsetGOptM :: (IsLocal f f, EnvType f ~ Env gbl lcl) => GeneralFlag -> f a -> f a
unsetGOptM flag = locally (env_topL . hsc_dflagsL) (`gopt_unset` flag)

unsetWOptM :: (IsLocal f f, EnvType f ~ Env gbl lcl) => WarningFlag -> f a -> f a
unsetWOptM flag = locally (env_topL . hsc_dflagsL) (`wopt_unset` flag)

-- | Do it flag is true
whenDOptM :: (Monad m, HasDynFlags m) => DumpFlag -> m () -> m ()
whenDOptM flag thing_inside = do b <- doptM flag
                                 when b thing_inside
{-# INLINE whenDOptM #-} -- see Note [INLINE conditional tracing utilities]

whenGOptM :: (Monad m, HasDynFlags m) => GeneralFlag -> m () -> m ()
whenGOptM flag thing_inside = do b <- goptM flag
                                 when b thing_inside
{-# INLINE whenGOptM #-} -- see Note [INLINE conditional tracing utilities]

whenWOptM :: (Monad m, HasDynFlags m) => WarningFlag -> m () -> m ()
whenWOptM flag thing_inside = do b <- woptM flag
                                 when b thing_inside
{-# INLINE whenWOptM #-} -- see Note [INLINE conditional tracing utilities]

whenXOptM :: (Monad m, HasDynFlags m) => LangExt.Extension -> m () -> m ()
whenXOptM flag thing_inside = do b <- xoptM flag
                                 when b thing_inside
{-# INLINE whenXOptM #-} -- see Note [INLINE conditional tracing utilities]

unlessXOptM :: (Monad m, HasDynFlags m) => LangExt.Extension -> m () -> m ()
unlessXOptM flag thing_inside = do b <- xoptM flag
                                   unless b thing_inside
{-# INLINE unlessXOptM #-} -- see Note [INLINE conditional tracing utilities]

getGhcMode :: (Functor f, HasDynFlags f) => f GhcMode
getGhcMode = ghcMode <$> getDynFlags

withDoDynamicToo :: (IsLocal f f, EnvType f ~ Env gbl lcl) => f a -> f a
withDoDynamicToo = locally (env_topL . hsc_dflagsL) dynamicTooMkDynamicDynFlags

getEpsVar :: (IsReader m, EnvType m ~ Env gbl lcl) => m (TcRef ExternalPackageState)
getEpsVar = view (env_topL . hsc_EPSL)

getEps :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => m ExternalPackageState
getEps = getEpsVar >>= readMutVar

-- | Update the external package state.  Returns the second result of the
-- modifier function.
--
-- This is an atomic operation and forces evaluation of the modified EPS in
-- order to avoid space leaks.
updateEps
 :: (HasDynFlags m, MonadIO m, IsReader m, EnvType m ~ Env gbl lcl)
 => (ExternalPackageState -> (ExternalPackageState, a)) -> m a
updateEps fn = do
  traceIf (text "updating EPS")
  eps_var <- getEpsVar
  atomicUpdMutVar' eps_var fn

-- | Update the external package state.
--
-- This is an atomic operation and forces evaluation of the modified EPS in
-- order to avoid space leaks.
updateEps_
 :: (HasDynFlags m, MonadIO m, IsReader m, EnvType m ~ Env gbl lcl)
 => (ExternalPackageState -> ExternalPackageState) -> m ()
updateEps_ fn = updateEps (\eps -> (fn eps, ()))

getHpt :: (IsReader m, EnvType m ~ Env gbl lcl) => m HomePackageTable
getHpt = view (env_topL . hsc_HPTL)

getEpsAndHpt :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => m (ExternalPackageState, HomePackageTable)
getEpsAndHpt = [(eps, hsc_HPT env) | env <- getTopEnv, eps <- readMutVar (hsc_EPS env)]

-- | A convenient wrapper for taking a @MaybeErr MsgDoc a@ and throwing
-- an exception if it is an error.
withException :: (HasDynFlags m, MonadIO m) => m (MaybeErr MsgDoc a) -> m a
withException do_this = do
    r <- do_this
    dflags <- getDynFlags
    case r of
        Failed err -> liftIO $ throwGhcExceptionIO (ProgramError (showSDoc dflags err))
        Succeeded result -> pure result

{-
************************************************************************
*                                                                      *
                Arrow scopes
*                                                                      *
************************************************************************
-}

newArrowScope :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => f a -> f a
newArrowScope
  = updLclEnv $ \env -> env { tcl_arrow_ctxt = ArrowCtxt (tcl_rdr env) (tcl_lie env) }

-- Return to the stored environment (from the enclosing proc)
escapeArrowScope :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => f a -> f a
escapeArrowScope = updLclEnv $ \ env -> case tcl_arrow_ctxt env of
    NoArrowCtxt       -> env
    ArrowCtxt rdr_env lie -> env
      { tcl_arrow_ctxt = NoArrowCtxt, tcl_lie = lie, tcl_rdr = rdr_env }

{-
************************************************************************
*                                                                      *
                Unique supply
*                                                                      *
************************************************************************
-}

newUnique :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => m Unique
newUnique
 = do { env <- ask
      ; let mask = env_um env
      ; liftIO $! uniqFromMask mask }

newUniqueSupply :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => m UniqSupply
newUniqueSupply
 = do { env <- ask
      ; let mask = env_um env
      ; liftIO $! mkSplitUniqSupply mask }

cloneLocalName :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => Name -> m Name
-- Make a fresh Internal name with the same OccName and SrcSpan
cloneLocalName name = newNameAt (nameOccName name) (nameSrcSpan name)

newName :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => OccName -> m Name
newName occ = getSrcSpanM >>= newNameAt occ

newNameAt :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => OccName -> SrcSpan -> m Name
newNameAt occ span = newUnique <₪> \ uniq -> mkInternalName uniq occ span

newSysName :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => OccName -> m Name
newSysName occ = newUnique <₪> \ uniq -> mkSystemName uniq occ

newSysLocalId :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => FastString -> TcType -> m TcId
newSysLocalId fs ty = newUnique <₪> \ uniq -> mkSysLocal fs uniq ty

newSysLocalIds :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl, Traversable t) => FastString -> t TcType -> m (t TcId)
newSysLocalIds fs tys = newUniqueSupply <₪> \ uniqs -> withUniques (mkSysLocal fs) uniqs tys

instance MonadUnique (IOEnv (Env gbl lcl)) where
        getUniqueM = newUnique
        getUniqueSupplyM = newUniqueSupply

{-
************************************************************************
*                                                                      *
                Debugging
*                                                                      *
************************************************************************
-}

-- Note [INLINE conditional tracing utilities]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In general we want to optimise for the case where tracing is not enabled.
-- To ensure this happens, we ensure that traceTc and friends are inlined; this
-- ensures that the allocation of the document can be pushed into the tracing
-- path, keeping the non-traced path free of this extraneous work. For
-- instance, instead of
--
--     let thunk = ...
--     in if doTracing
--          then emitTraceMsg thunk
--          else return ()
--
-- where the conditional is buried in a non-inlined utility function (e.g.
-- traceTc), we would rather have:
--
--     if doTracing
--       then let thunk = ...
--            in emitTraceMsg thunk
--       else return ()
--
-- See #18168.
--

-- Typechecker trace
traceTc :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => String -> SDoc -> m ()
traceTc herald doc =
    labelledTraceOptTcRn Opt_D_dump_tc_trace herald doc
{-# INLINE traceTc #-} -- see Note [INLINE conditional tracing utilities]

-- Renamer Trace
traceRn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => String -> SDoc -> m ()
traceRn herald doc =
    labelledTraceOptTcRn Opt_D_dump_rn_trace herald doc
{-# INLINE traceRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Trace when a certain flag is enabled. This is like `traceOptTcRn`
-- but accepts a string as a label and formats the trace message uniformly.
labelledTraceOptTcRn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => DumpFlag -> String -> SDoc -> m ()
labelledTraceOptTcRn flag herald doc =
  traceOptTcRn flag (formatTraceMsg herald doc)
{-# INLINE labelledTraceOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

formatTraceMsg :: String -> SDoc -> SDoc
formatTraceMsg herald doc = hang (text herald) 2 doc

traceOptTcRn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => DumpFlag -> SDoc -> m ()
traceOptTcRn flag doc = do
  whenDOptM flag $
    dumpTcRn False (dumpOptionsFromFlag flag) "" FormatText doc
{-# INLINE traceOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Dump if the given 'DumpFlag' is set.
dumpOptTcRn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => DumpFlag -> String -> DumpFormat -> SDoc -> m ()
dumpOptTcRn flag title fmt doc = do
  whenDOptM flag $
    dumpTcRn False (dumpOptionsFromFlag flag) title fmt doc
{-# INLINE dumpOptTcRn #-} -- see Note [INLINE conditional tracing utilities]

-- | Unconditionally dump some trace output
--
-- Certain tests (T3017, Roles3, T12763 etc.) expect part of the
-- output generated by `-ddump-types` to be in 'PprUser' style. However,
-- generally we want all other debugging output to use 'PprDump'
-- style. We 'PprUser' style if 'useUserStyle' is True.
--
dumpTcRn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => Bool -> DumpOptions -> String -> DumpFormat -> SDoc -> m ()
dumpTcRn useUserStyle dumpOpt title fmt doc = do
  dflags <- getDynFlags
  printer <- getPrintUnqualified dflags
  real_doc <- wrapDocLoc doc
  let sty = if useUserStyle
              then mkUserStyle printer AllTheWay
              else mkDumpStyle printer
  liftIO $ dumpAction dflags sty dumpOpt title fmt real_doc

-- | Add current location if -dppr-debug
-- (otherwise the full location is usually way too much)
wrapDocLoc :: (HasDynFlags m, IsReader m, Monad m, EnvType m ~ Env gbl TcLclEnv) => SDoc -> m SDoc
wrapDocLoc doc = do
  dflags <- getDynFlags
  if hasPprDebug dflags
    then do
      loc <- getSrcSpanM
      return (mkLocMessage SevOutput loc doc)
    else
      return doc

getPrintUnqualified :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => DynFlags -> f PrintUnqualified
getPrintUnqualified dflags =
  mkPrintUnqualified (pkgState dflags) (homeUnit dflags) <$> tcg_rdr_env . env_gbl <$> ask

-- | Like logInfoTcRn, but for user consumption
printForUserTcRn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => SDoc -> m ()
printForUserTcRn doc
  = do { dflags <- getDynFlags
       ; printer <- getPrintUnqualified dflags
       ; liftIO (printOutputForUser dflags printer doc) }

{-
traceIf and traceHiDiffs work in the TcRnIf monad, where no RdrEnv is
available.  Alas, they behave inconsistently with the other stuff;
e.g. are unaffected by -dump-to-file.
-}

traceIf, traceHiDiffs :: (HasDynFlags m, MonadIO m) => SDoc -> m ()
traceIf      = traceOptIf Opt_D_dump_if_trace
traceHiDiffs = traceOptIf Opt_D_dump_hi_diffs
{-# INLINE traceIf #-}
{-# INLINE traceHiDiffs #-}
  -- see Note [INLINE conditional tracing utilities]

traceOptIf :: (HasDynFlags m, MonadIO m) => DumpFlag -> SDoc -> m ()
traceOptIf flag doc
  = whenDOptM flag $    -- No RdrEnv available, so qualify everything
    do { dflags <- getDynFlags
       ; liftIO (putMsg dflags doc) }
{-# INLINE traceOptIf #-}  -- see Note [INLINE conditional tracing utilities]

{-
************************************************************************
*                                                                      *
                Typechecker global environment
*                                                                      *
************************************************************************
-}

getIsGHCi :: (Functor f, HasModule f) => f Bool
getIsGHCi = isInteractiveModule <$> getModule

getGHCiMonad :: (IsReader f, EnvType f ~ Env gbl lcl) => f Name
getGHCiMonad = ic_monad . hsc_IC <$> getTopEnv

getInteractivePrintName :: (IsReader f, EnvType f ~ Env gbl lcl) => f Name
getInteractivePrintName = ic_int_print . hsc_IC <$> getTopEnv

tcIsHsBootOrSig :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f Bool
tcIsHsBootOrSig = isHsBootOrSig . tcg_src <$> getGblEnv

tcIsHsig :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f Bool
tcIsHsig = isHsigFile . tcg_src <$> getGblEnv

tcSelfBootInfo :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f SelfBootInfo
tcSelfBootInfo = tcg_self_boot <$> getGblEnv

getGlobalRdrEnv :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f GlobalRdrEnv
getGlobalRdrEnv = tcg_rdr_env <$> getGblEnv

getRdrEnvs :: (IsReader f, EnvType f ~ Env TcGblEnv TcLclEnv) => f (GlobalRdrEnv, LocalRdrEnv)
getRdrEnvs = (,) <$> tcg_rdr_env . env_gbl <*> tcl_rdr . env_lcl <$> ask

getImports :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f ImportAvails
getImports = tcg_imports <$> getGblEnv

getFixityEnv :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f FixityEnv
getFixityEnv = tcg_fix_env <$> getGblEnv

extendFixityEnv :: (IsLocal f f, EnvType f ~ Env TcGblEnv lcl) => [(Name,FixItem)] -> f a -> f a
extendFixityEnv = locally (env_gblL . tcg_fix_envL) . flip extendNameEnvList

getRecFieldEnv :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f RecFieldEnv
getRecFieldEnv = tcg_field_env <$> getGblEnv

getDeclaredDefaultTys :: (IsReader f, EnvType f ~ Env TcGblEnv lcl) => f (Maybe [Type])
getDeclaredDefaultTys = tcg_default <$> getGblEnv

addDependentFiles :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => [FilePath] -> m ()
addDependentFiles fs = do
  ref <- tcg_dependent_files <$> getGblEnv
  updMutVar ref (fs ++)

{-
************************************************************************
*                                                                      *
                Error management
*                                                                      *
************************************************************************
-}

getSrcSpanM :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f SrcSpan
        -- Avoid clash with Name.getSrcLoc
getSrcSpanM = flip RealSrcSpan Strict.Nothing . tcl_loc . env_lcl <$> ask

setSrcSpan :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => SrcSpan -> f a -> f a
setSrcSpan (RealSrcSpan real_loc _) = locally (env_lclL . tcl_locL) (pure real_loc)
-- Don't overwrite useful info with useless:
setSrcSpan (UnhelpfulSpan _) = id

addLocM :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => (a -> f b) -> Located a -> f b
addLocM fn (L loc a) = setSrcSpan loc $ fn a

wrapLocM :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => (a -> f b) -> Located a -> f (Located b)
wrapLocM fn (L loc a) = setSrcSpan loc $ L loc <$> fn a

wrapLocFstM :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => (a -> f (b,c)) -> Located a -> f (Located b, c)
wrapLocFstM fn (L loc a) = setSrcSpan loc $ over _1 (L loc) <$> fn a

wrapLocSndM :: (IsLocal f f, Functor g, EnvType f ~ Env gbl TcLclEnv) => (a -> f (g b)) -> Located a -> f (g (Located b))
wrapLocSndM fn (L loc a) = setSrcSpan loc $ fmap (L loc) <$> fn a

wrapLocM_ :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => (a -> f ()) -> Located a -> f ()
wrapLocM_ fn (L loc a) = setSrcSpan loc (fn a)

-- Reporting errors

getErrsVar :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f (TcRef Messages)
getErrsVar = tcl_errs . env_lcl <$> ask

setErrsVar :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => TcRef Messages -> f a -> f a
setErrsVar = locally (env_lclL . tcl_errsL) . pure

addErr :: MsgDoc -> TcRn ()
addErr msg = do { loc <- getSrcSpanM; addErrAt loc msg }

failWith :: MsgDoc -> TcRn a
failWith msg = addErr msg >> failM

failAt :: SrcSpan -> MsgDoc -> TcRn a
failAt loc msg = addErrAt loc msg >> failM

addErrAt :: SrcSpan -> MsgDoc -> TcRn ()
-- addErrAt is mainly (exclusively?) used by the renamer, where
-- tidying is not an issue, but it's all lazy so the extra
-- work doesn't matter
addErrAt loc msg = do { ctxt <- getErrCtxt
                      ; tidy_env <- tcInitTidyEnv
                      ; err_info <- mkErrInfo tidy_env ctxt
                      ; addLongErrAt loc msg err_info }

addErrs :: Foldable t => t (SrcSpan,MsgDoc) -> TcRn ()
addErrs = traverse_ (uncurry addErrAt)

checkErr :: Bool -> MsgDoc -> TcRn ()
-- Add the error if the bool is False
checkErr ok msg = unless ok (addErr msg)

addMessages :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => Messages -> m ()
addMessages msgs1
  = do { errs_var <- getErrsVar ;
         updMutVar errs_var (`unionMessages` msgs1) }

discardWarnings :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => m a -> m a
-- Ignore warnings inside the thing inside;
-- used to ignore-unused-variable warnings inside derived code
discardWarnings thing_inside
  = do  { errs_var <- getErrsVar
        ; (old_warns, _) <- readMutVar errs_var
        -- Revert warnings to old_warns
        ; thing_inside <* updMutVar errs_var (set _1 old_warns) }

{-
************************************************************************
*                                                                      *
        Shared error message stuff: renamer and typechecker
*                                                                      *
************************************************************************
-}

mkLongErrAt :: (Monad m, HasDynFlags m, IsReader m, EnvType m ~ Env TcGblEnv lcl) => SrcSpan -> MsgDoc -> MsgDoc -> m ErrMsg
mkLongErrAt loc msg extra =
  [ mkLongErrMsg dflags loc printer msg' extra
  | dflags <- getDynFlags
  , printer <- getPrintUnqualified dflags
  , unit_state <- pkgState <$> getDynFlags
  , let msg' = pprWithUnitState unit_state msg ]

mkErrDocAt :: (Monad m, HasDynFlags m, IsReader m, EnvType m ~ Env TcGblEnv lcl) => SrcSpan -> ErrDoc -> m ErrMsg
mkErrDocAt loc errDoc =
  [ mkErrDoc dflags loc printer errDoc'
  | dflags <- getDynFlags
  , printer <- getPrintUnqualified dflags
  , unit_state <- pkgState <$> getDynFlags
  , let f = pprWithUnitState unit_state
        errDoc' = mapErrDoc f errDoc ]

addLongErrAt :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => SrcSpan -> MsgDoc -> MsgDoc -> m ()
addLongErrAt loc msg extra = mkLongErrAt loc msg extra >>= reportError

reportError :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => ErrMsg -> m ()
reportError err
  = do { traceTc "Adding error:" (pprLocErrMsg err) ;
         errs_var <- getErrsVar ;
         updMutVar errs_var (fmap (`snocBag` err)) }

reportWarning :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => WarnReason -> ErrMsg -> m ()
reportWarning reason err
  = do { let warn = makeIntoWarning reason err
                    -- 'err' was built by mkLongErrMsg or something like that,
                    -- so it's of error severity.  For a warning we downgrade
                    -- its severity to SevWarning

       ; traceTc "Adding warning:" (pprLocErrMsg warn)
       ; errs_var <- getErrsVar
       ; updMutVar errs_var (over _1 (`snocBag` warn)) }


-----------------------
checkNoErrs :: TcM r -> TcM r
-- (checkNoErrs m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrsTc m) fails.
-- If m succeeds, it checks whether m generated any errors messages
--      (it might have recovered internally)
--      If so, it fails too.
-- Regardless, any errors generated by m are propagated to the enclosing context.
checkNoErrs main
  = do  { (res, no_errs) <- askNoErrs main
        ; res <$ unless no_errs failM }

-----------------------
whenNoErrs :: (IsReader m, MonadIO m, HasDynFlags m, EnvType m ~ Env gbl TcLclEnv) => m () -> m ()
whenNoErrs = ifErrsM $ pure ()

ifErrsM :: (IsReader m, MonadIO m, HasDynFlags m, EnvType m ~ Env gbl TcLclEnv) => m r -> m r -> m r
--      ifErrsM bale_out normal
-- does 'bale_out' if there are errors in errors collection
-- otherwise does 'normal'
ifErrsM bale_out normal
 = do { errs_var <- getErrsVar ;
        msgs <- readMutVar errs_var ;
        dflags <- getDynFlags ;
        bool normal bale_out $ errorsFound dflags msgs }

failIfErrsM :: (IsReader m, MonadIO m, HasDynFlags m, EnvType m ~ Env gbl TcLclEnv) => m ()
-- Useful to avoid error cascades
failIfErrsM = ifErrsM failM (pure ())

{- *********************************************************************
*                                                                      *
        Context management for the type checker
*                                                                      *
************************************************************************
-}

getErrCtxt :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f [ErrCtxt]
getErrCtxt = tcl_ctxt . env_lcl <$> ask

setErrCtxt :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => [ErrCtxt] -> f a -> f a
setErrCtxt = locally (env_lclL . tcl_ctxtL) . pure

-- | Add a fixed message to the error context. This message should not
-- do any tidying.
addErrCtxt :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => MsgDoc -> f a -> f a
addErrCtxt = addErrCtxtM . pure

-- | Add a message to the error context. This message may do tidying.
addErrCtxtM :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => StateT TidyEnv TcM MsgDoc -> f a -> f a
addErrCtxtM ctxt = locally (env_lclL . tcl_ctxtL) ((False, runStateT ctxt) :)

-- | Add a fixed landmark message to the error context. A landmark
-- message is always sure to be reported, even if there is a lot of
-- context. It also doesn't count toward the maximum number of contexts
-- reported.
addLandmarkErrCtxt :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => MsgDoc -> f a -> f a
addLandmarkErrCtxt = addLandmarkErrCtxtM . pure

-- | Variant of 'addLandmarkErrCtxt' that allows for monadic operations
-- and tidying.
addLandmarkErrCtxtM :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => StateT TidyEnv TcM MsgDoc -> f a -> f a
addLandmarkErrCtxtM ctxt = locally (env_lclL . tcl_ctxtL) ((True, runStateT ctxt) :)

popErrCtxt :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => f a -> f a
popErrCtxt = locally (env_lclL . tcl_ctxtL) (drop 1)

getCtLocM :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => CtOrigin -> Maybe TypeOrKind -> f CtLoc
getCtLocM origin t_or_k = getLclEnv <₪> \ env -> CtLoc
  { ctl_origin = origin
  , ctl_env    = env
  , ctl_t_or_k = t_or_k
  , ctl_depth  = initialSubGoalDepth }

setCtLocM :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => CtLoc -> f a -> f a
-- Set the SrcSpan and error context from the CtLoc
setCtLocM (CtLoc { ctl_env = lcl }) = updLclEnv \env -> env
  { tcl_loc   = tcl_loc lcl
  , tcl_bndrs = tcl_bndrs lcl
  , tcl_ctxt  = tcl_ctxt lcl }


{- *********************************************************************
*                                                                      *
             Error recovery and exceptions
*                                                                      *
********************************************************************* -}

tcTryM :: IOEnv env r -> IOEnv env (Maybe r)
-- The most basic function: catch the exception
--   Nothing => an exception happened
--   Just r  => no exception, result R
-- Errors and constraints are propagated in both cases
-- Never throws an exception
tcTryM = tryM & fmap \ case
                    Left _  -> Nothing
                    Right r -> Just r
         -- In the Left case the exception is always the IOEnv
         -- built-in in exception; see IOEnv.failM

-----------------------
capture_constraints :: (MonadIO m, IsLocal m m, EnvType m ~ Env gbl TcLclEnv) => m r -> m (r, WantedConstraints)
-- capture_constraints simply captures and returns the
--                     constraints generated by thing_inside
-- Precondition: thing_inside must not throw an exception!
-- Reason for precondition: an exception would blow past the place
-- where we read the lie_var, and we'd lose the constraints altogether
capture_constraints thing_inside
  = do { lie_var <- newMutVar emptyWC
       ; (,) <$> locally (env_lclL . tcl_lieL) (pure lie_var) thing_inside <*> readMutVar lie_var }

capture_messages :: (MonadIO m, IsLocal m m, EnvType m ~ Env gbl TcLclEnv) => m r -> m (r, Messages)
-- capture_messages simply captures and returns the
--                  errors arnd warnings generated by thing_inside
-- Precondition: thing_inside must not throw an exception!
-- Reason for precondition: an exception would blow past the place
-- where we read the msg_var, and we'd lose the constraints altogether
capture_messages thing_inside
  = do { msg_var <- newMutVar emptyMessages
       ; (,) <$> setErrsVar msg_var thing_inside <*> readMutVar msg_var }

-----------------------
-- (askNoErrs m) runs m
-- If m fails,
--    then (askNoErrs m) fails, propagating only
--         insoluble constraints
--
-- If m succeeds with result r,
--    then (askNoErrs m) succeeds with result (r, b),
--         where b is True iff m generated no errors
--
-- Regardless of success or failure,
--   propagate any errors/warnings generated by m
askNoErrs :: TcRn a -> TcRn (a, Bool)
askNoErrs thing_inside
  = do { ((mb_res, lie), msgs) <- capture_messages    $
                                  capture_constraints $
                                  tcTryM thing_inside
       ; addMessages msgs

       ; case mb_res of
           Nothing  -> do { emitConstraints (insolublesOnly lie)
                          ; failM }

           Just res -> do { emitConstraints lie
                          ; dflags <- getDynFlags
                          ; let errs_found = errorsFound dflags msgs
                                          || insolubleWC lie
                          ; return (res, not errs_found) } }

-----------------------
tryCaptureConstraints :: TcRnIf gbl TcLclEnv a -> TcRnIf gbl TcLclEnv (Maybe a, WantedConstraints)
-- (tryCaptureConstraints_maybe m) runs m,
--   and returns the type constraints it generates
-- It never throws an exception; instead if thing_inside fails,
--   it returns Nothing and the /insoluble/ constraints
-- Error messages are propagated
tryCaptureConstraints thing_inside =
  [ (mb_res, lie_to_keep)
  | (mb_res, lie) <- capture_constraints $ tcTryM thing_inside

    -- See Note [Constraints and errors]
  , let lie_to_keep = case mb_res of
                             Nothing -> insolublesOnly lie
                             Just {} -> lie ]

captureConstraints :: TcRnIf gbl TcLclEnv a -> TcRnIf gbl TcLclEnv (a, WantedConstraints)
-- (captureConstraints m) runs m, and returns the type constraints it generates
-- If thing_inside fails (throwing an exception),
--   then (captureConstraints thing_inside) fails too
--   propagating the insoluble constraints only
-- Error messages are propagated in either case
captureConstraints thing_inside
  = do { (mb_res, lie) <- tryCaptureConstraints thing_inside

            -- See Note [Constraints and errors]
            -- If the thing_inside threw an exception, emit the insoluble
            -- constraints only (returned by tryCaptureConstraints)
            -- so that they are not lost
       ; case mb_res of
           Nothing  -> do { emitConstraints lie; failM }
           Just res -> return (res, lie) }

-----------------------
attemptM :: TcRn r -> TcRn (Maybe r)
-- (attemptM thing_inside) runs thing_inside
-- If thing_inside succeeds, returning r,
--   we return (Just r), and propagate all constraints and errors
-- If thing_inside fail, throwing an exception,
--   we return Nothing, propagating insoluble constraints,
--                      and all errors
-- attemptM never throws an exception
attemptM thing_inside =
  [ mb_r
  | (mb_r, lie) <- tryCaptureConstraints thing_inside
  , () <- emitConstraints lie

    -- Debug trace
  , () <- when (isNothing mb_r) $
         traceTc "attemptM recovering with insoluble constraints" $
                 (ppr lie) ]

-----------------------
recoverM
 :: TcRn r -- Recovery action; do this if the main one fails
 -> TcRn r -- Main action: do this first; if it generates errors, propagate them all
 -> TcRn r
-- (recoverM recover thing_inside) runs thing_inside
-- If thing_inside fails, propagate its errors and insoluble constraints
--                        and run 'recover'
-- If thing_inside succeeds, propagate all its errors and constraints
--
-- Can fail, if 'recover' fails
recoverM recover = maybe recover pure <=< attemptM

-----------------------

-- | Drop elements of the input that fail, so the result
-- list can be shorter than the argument list
mapAndRecoverM :: (Filtrable f, Traversable f) => (a -> TcRn b) -> f a -> TcRn (f b)
mapAndRecoverM f = mapMaybeA (attemptM . f)

-- | Apply the function to all elements on the input list
-- If all succeed, return the list of results
-- Otherwise fail, propagating all errors
mapAndReportM :: Traversable t => (a -> TcRn b) -> t a -> TcRn (t b)
mapAndReportM f xs
  = do { mb_rs <- traverse (attemptM . f) xs
       ; maybe failM pure $ sequenceA mb_rs }

-----------------------
tryTc :: TcRn a -> TcRn (Maybe a, Messages)
-- (tryTc m) executes m, and returns
--      Just r,  if m succeeds (returning r)
--      Nothing, if m fails
-- It also returns all the errors and warnings accumulated by m
-- It always succeeds (never raises an exception)
tryTc thing_inside
 = capture_messages (attemptM thing_inside)

-----------------------
discardErrs :: (MonadIO m, IsLocal m m, EnvType m ~ Env gbl TcLclEnv) => m a -> m a
-- (discardErrs m) runs m,
--   discarding all error messages and warnings generated by m
-- If m fails, discardErrs fails, and vice versa
discardErrs m
 = do { errs_var <- newMutVar emptyMessages
      ; setErrsVar errs_var m }

-----------------------
tryTcDiscardingErrs :: TcM r -> TcM r -> TcM r
-- (tryTcDiscardingErrs recover thing_inside) tries 'thing_inside';
--      if 'main' succeeds with no error messages, it's the answer
--      otherwise discard everything from 'main', including errors,
--          and try 'recover' instead.
tryTcDiscardingErrs recover thing_inside
  = do { ((mb_res, lie), msgs) <- capture_messages    $
                                  capture_constraints $
                                  tcTryM thing_inside
        ; dflags <- getDynFlags
        ; case mb_res of
            Just res | not (errorsFound dflags msgs)
                     , not (insolubleWC lie)
              -> -- 'main' succeeded with no errors
                 do { addMessages msgs  -- msgs might still have warnings
                    ; emitConstraints lie
                    ; return res }

            _ -> -- 'main' failed, or produced an error message
                 recover     -- Discard all errors and warnings
                             -- and unsolved constraints entirely
        }

{-
************************************************************************
*                                                                      *
             Error message generation (type checker)
*                                                                      *
************************************************************************

    The addErrTc functions add an error message, but do not cause failure.
    The 'M' variants pass a TidyEnv that has already been used to
    tidy up the message; we then use it to tidy the context messages
-}

addErrTc :: MsgDoc -> TcM ()
addErrTc err_msg = do { env0 <- tcInitTidyEnv
                      ; addErrTcM (env0, err_msg) }

addErrTcM :: (TidyEnv, MsgDoc) -> TcM ()
addErrTcM (tidy_env, err_msg)
  = do { ctxt <- getErrCtxt ;
         loc  <- getSrcSpanM ;
         add_err_tcm tidy_env err_msg loc ctxt }

-- The failWith functions add an error message and cause failure

failWithTc :: MsgDoc -> TcM a               -- Add an error message and fail
failWithTc err_msg = addErrTc err_msg >> failM

failWithTcM :: (TidyEnv, MsgDoc) -> TcM a   -- Add an error message and fail
failWithTcM local_and_msg = addErrTcM local_and_msg >> failM

checkTc :: Bool -> MsgDoc -> TcM ()         -- Check that the boolean is true
checkTc b = unless b . failWithTc

checkTcM :: Bool -> (TidyEnv, MsgDoc) -> TcM ()
checkTcM b = unless b . failWithTcM

failIfTc :: Bool -> MsgDoc -> TcM ()         -- Check that the boolean is false
failIfTc b = when b . failWithTc

failIfTcM :: Bool -> (TidyEnv, MsgDoc) -> TcM ()
   -- Check that the boolean is false
failIfTcM b = when b . failWithTcM


--         Warnings have no 'M' variant, nor failure

-- | Display a warning if a condition is met,
--   and the warning is enabled
warnIfFlag :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => WarningFlag -> Bool -> MsgDoc -> m ()
warnIfFlag warn_flag is_bad msg
  = do { warn_on <- woptM warn_flag
       ; when (warn_on && is_bad) $
         addWarn (Reason warn_flag) msg }

-- | Display a warning if a condition is met,
--   and the warning is enabled
warnIfFlagAt :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => SrcSpan -> WarningFlag -> Bool -> MsgDoc -> m ()
warnIfFlagAt loc warn_flag is_bad msg
  = do { warn_on <- woptM warn_flag
       ; when (warn_on && is_bad) $
         addWarnAt (Reason warn_flag) loc msg }

-- | Display a warning if a condition is met.
warnIf :: (IsReader m, HasDynFlags m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => Bool -> MsgDoc -> m ()
warnIf is_bad msg = when is_bad (addWarn NoReason msg)

-- | Display a warning if a condition is met.
warnTc :: WarnReason -> Bool -> MsgDoc -> TcM ()
warnTc reason warn_if_true = when warn_if_true . addWarnTc reason

-- | Display a warning if a condition is met.
warnTcM :: WarnReason -> Bool -> (TidyEnv, MsgDoc) -> TcM ()
warnTcM reason warn_if_true = when warn_if_true . addWarnTcM reason

-- | Display a warning in the current context.
addWarnTc :: WarnReason -> MsgDoc -> TcM ()
addWarnTc reason msg
 = do { env0 <- tcInitTidyEnv ;
      addWarnTcM reason (env0, msg) }

-- | Display a warning in a given context.
addWarnTcM :: WarnReason -> (TidyEnv, MsgDoc) -> TcM ()
addWarnTcM reason (env0, msg)
 = do { ctxt <- getErrCtxt ;
        err_info <- mkErrInfo env0 ctxt ;
        add_warn reason msg err_info }

-- | Display a warning for the current source location.
addWarn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => WarnReason -> MsgDoc -> m ()
addWarn reason msg = add_warn reason msg mempty

-- | Display a warning for a given source location.
addWarnAt :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => WarnReason -> SrcSpan -> MsgDoc -> m ()
addWarnAt reason loc msg = add_warn_at reason loc msg mempty

-- | Display a warning, with an optional flag, for the current source
-- location.
add_warn :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => WarnReason -> MsgDoc -> MsgDoc -> m ()
add_warn reason msg extra_info
  = do { loc <- getSrcSpanM
       ; add_warn_at reason loc msg extra_info }

-- | Display a warning, with an optional flag, for a given location.
add_warn_at :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => WarnReason -> SrcSpan -> MsgDoc -> MsgDoc -> m ()
add_warn_at reason loc msg extra_info
  = do { dflags <- getDynFlags ;
         printer <- getPrintUnqualified dflags ;
         let { warn = mkLongWarnMsg dflags loc printer
                                    msg extra_info } ;
         reportWarning reason warn }


{-
-----------------------------------
        Other helper functions
-}

add_err_tcm :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => a -> MsgDoc -> SrcSpan -> [(Bool, a -> m (SDoc, a))] -> m ()
add_err_tcm tidy_env err_msg loc ctxt
 = do { err_info <- mkErrInfo tidy_env ctxt ;
        addLongErrAt loc err_msg err_info }

mkErrInfo :: Monad m => a -> [(Bool, a -> m (SDoc, a))] -> m SDoc
-- Tidy the error info, trimming excessive contexts
mkErrInfo
--  = do
--       dbg <- hasPprDebug <$> getDynFlags
--       if dbg                -- In -dppr-debug style the output
--          then return empty  -- just becomes too voluminous
--          else go dbg 0 env ctxts
 = go False 0
 where
   go :: Monad m => Bool -> Int -> a -> [(Bool, a -> m (SDoc, a))] -> m SDoc
   go _ _ _   [] = pure mempty
   go dbg n env ((is_landmark, ctxt) : ctxts)
     | is_landmark || n < mAX_CONTEXTS -- Too verbose || dbg
     = do { (msg, env') <- ctxt env
          ; let n' | is_landmark = n | otherwise = n+1
          ; (msg $$) <$> go dbg n' env' ctxts }
     | otherwise
     = go dbg n env ctxts

mAX_CONTEXTS :: Int     -- No more than this number of non-landmark contexts
mAX_CONTEXTS = 3

-- debugTc is useful for monadic debugging code

debugTc :: Applicative f => f () -> f ()
debugTc = when debugIsOn

{-
************************************************************************
*                                                                      *
             Type constraints
*                                                                      *
************************************************************************
-}

addTopEvBinds :: (IsLocal f f, EnvType f ~ Env TcGblEnv lcl) => Bag EvBind -> f a -> f a
addTopEvBinds = locally (env_gblL . tcg_ev_bindsL) . flip (<|>)

newTcEvBinds :: (MonadIO m, IsReader m, HasDynFlags m, EnvType m ~ Env TcGblEnv TcLclEnv) => m EvBindsVar
newTcEvBinds =
  [ EvBindsVar { ebv_binds = binds_ref, ebv_tcvs = tcvs_ref, ebv_uniq = uniq }
  | binds_ref <- newMutVar emptyEvBindMap
  , tcvs_ref  <- newMutVar emptyVarSet
  , uniq <- newUnique
  , () <- traceTc "newTcEvBinds" (text "unique =" <+> ppr uniq) ]

-- | Creates an EvBindsVar incapable of holding any bindings. It still
-- tracks covar usages (see comments on ebv_tcvs in "GHC.Tc.Types.Evidence"), thus
-- must be made monadically
newNoTcEvBinds :: (MonadIO m, IsReader m, HasDynFlags m, EnvType m ~ Env TcGblEnv TcLclEnv) => m EvBindsVar
newNoTcEvBinds =
  [ CoEvBindsVar { ebv_tcvs = tcvs_ref, ebv_uniq = uniq }
  | tcvs_ref  <- newMutVar emptyVarSet
  , uniq <- newUnique
  , () <- traceTc "newNoTcEvBinds" (text "unique =" <+> ppr uniq) ]

cloneEvBindsVar :: MonadIO m => EvBindsVar -> m EvBindsVar
-- Clone the refs, so that any binding created when
-- solving don't pollute the original
cloneEvBindsVar ebv@(EvBindsVar {}) =
  [ ebv { ebv_binds = binds_ref, ebv_tcvs = tcvs_ref }
  | binds_ref <- newMutVar emptyEvBindMap
  , tcvs_ref  <- newMutVar emptyVarSet ]
cloneEvBindsVar ebv@(CoEvBindsVar {})
  = [ ebv { ebv_tcvs = tcvs_ref } | tcvs_ref <- newMutVar emptyVarSet ]

getTcEvTyCoVars :: MonadIO m => EvBindsVar -> m TyCoVarSet
getTcEvTyCoVars = readMutVar . ebv_tcvs

getTcEvBindsMap :: MonadIO m => EvBindsVar -> m EvBindMap
getTcEvBindsMap (EvBindsVar { ebv_binds = ev_ref }) = readMutVar ev_ref
getTcEvBindsMap (CoEvBindsVar {}) = pure emptyEvBindMap

setTcEvBindsMap :: MonadIO m => EvBindsVar -> EvBindMap -> m ()
setTcEvBindsMap (EvBindsVar { ebv_binds = ev_ref }) binds
  = writeMutVar ev_ref binds
setTcEvBindsMap v@(CoEvBindsVar {}) ev_binds
  | isEmptyEvBindMap ev_binds
  = return ()
  | otherwise
  = pprPanic "setTcEvBindsMap" (ppr v $$ ppr ev_binds)

addTcEvBind :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => EvBindsVar -> EvBind -> m ()
-- Add a binding to the TcEvBinds by side effect
addTcEvBind (EvBindsVar { ebv_binds = ev_ref, ebv_uniq = u }) ev_bind
  = do { traceTc "addTcEvBind" $ ppr u $$ ppr ev_bind
       ; updMutVar ev_ref (`extendEvBinds` ev_bind) }
addTcEvBind (CoEvBindsVar { ebv_uniq = u }) ev_bind
  = pprPanic "addTcEvBind CoEvBindsVar" (ppr ev_bind $$ ppr u)

chooseUniqueOccTc :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => (OccSet -> OccName) -> m OccName
chooseUniqueOccTc fn
  = do { env <- getGblEnv
       ; let dfun_n_var = tcg_dfun_n env
       ; set <- readMutVar dfun_n_var
       ; let occ = fn set
       ; occ <$ writeMutVar dfun_n_var (extendOccSet set occ) }

getConstraintVar :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f (TcRef WantedConstraints)
getConstraintVar = view (env_lclL . tcl_lieL)

setConstraintVar :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => TcRef WantedConstraints -> f a -> f a
setConstraintVar = locally (env_lclL . tcl_lieL) . pure

emitStaticConstraints :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => WantedConstraints -> m ()
emitStaticConstraints static_lie
  = do { gbl_env <- getGblEnv
       ; updMutVar (tcg_static_wc gbl_env) (`andWC` static_lie) }

emitConstraints :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => WantedConstraints -> m ()
emitConstraints ct = when (not (isEmptyWC ct)) $
    do { lie_var <- getConstraintVar ;
         updMutVar lie_var (`andWC` ct) }

emitSimple :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => Ct -> m ()
emitSimple ct
  = do { lie_var <- getConstraintVar ;
         updMutVar lie_var (`addSimples` pure ct) }

emitSimples :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => Cts -> m ()
emitSimples cts
  = do { lie_var <- getConstraintVar ;
         updMutVar lie_var (`addSimples` cts) }

emitImplication :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => Implication -> m ()
emitImplication ct
  = do { lie_var <- getConstraintVar ;
         updMutVar lie_var (`addImplics` pure ct) }

emitImplications :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => Bag Implication -> m ()
emitImplications ct
  = unless (isEmptyBag ct) $
    do { lie_var <- getConstraintVar ;
         updMutVar lie_var (`addImplics` ct) }

emitInsoluble :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => Ct -> m ()
emitInsoluble ct
  = do { traceTc "emitInsoluble" (ppr ct)
       ; lie_var <- getConstraintVar
       ; updMutVar lie_var (`addInsols` pure ct) }

emitHole :: (HasDynFlags m, IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => Hole -> m ()
emitHole hole
  = do { traceTc "emitHole" (ppr hole)
       ; lie_var <- getConstraintVar
       ; updMutVar lie_var (`addHole` hole) }

-- | Throw out any constraints emitted by the thing_inside
discardConstraints :: TcRnIf gbl TcLclEnv a -> TcRnIf gbl TcLclEnv a
discardConstraints thing_inside = fst <$> captureConstraints thing_inside

-- | The name says it all. The returned TcLevel is the *inner* TcLevel.
pushLevelAndCaptureConstraints :: TcM a -> TcM (TcLevel, WantedConstraints, a)
pushLevelAndCaptureConstraints thing_inside =
  [ (tclvl', lie, res)
  | env <- getLclEnv
  , let tclvl' = pushTcLevel (tcl_tclvl env)
  , () <- traceTc "pushLevelAndCaptureConstraints {" (ppr tclvl')
  , (res, lie) <- setLclEnv (env { tcl_tclvl = tclvl' }) $ captureConstraints thing_inside
  , () <- traceTc "pushLevelAndCaptureConstraints }" (ppr tclvl') ]

pushTcLevelM_ :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => f a -> f a
pushTcLevelM_ = locally (env_lclL . tcl_tclvlL) pushTcLevel

pushTcLevelM :: (Applicative f, IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => f a -> f (TcLevel, a)
-- See Note [TcLevel assignment] in GHC.Tc.Utils.TcType
pushTcLevelM = pushTcLevelM_ . liftA2 (,) getTcLevel

getTcLevel :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f TcLevel
getTcLevel = view (env_lclL . tcl_tclvlL)

setTcLevel :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => TcLevel -> f a -> f a
setTcLevel = locally (env_lclL . tcl_tclvlL) . pure

isTouchableTcM :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => TcTyVar -> f Bool
isTouchableTcM tv = getTcLevel <₪> \ lvl -> isTouchableMetaTyVar lvl tv

getLclTypeEnv :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f TcTypeEnv
getLclTypeEnv = view (env_lclL . tcl_envL)

traceTcConstraints :: (IsReader m, MonadIO m, HasDynFlags m, EnvType m ~ Env TcGblEnv TcLclEnv) => String -> m ()
traceTcConstraints msg
  = do { lie_var <- getConstraintVar
       ; lie     <- readMutVar lie_var
       ; traceOptTcRn Opt_D_dump_tc_trace $
         hang (text (msg ++ ": LIE:")) 2 (ppr lie)
       }

emitAnonTypeHole :: (IsReader m, HasDynFlags m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => TcTyVar -> m ()
emitAnonTypeHole tv
  = do { ct_loc <- getCtLocM (TypeHoleOrigin occ) Nothing
       ; let hole = Hole { hole_sort = TypeHole
                         , hole_occ  = occ
                         , hole_ty   = mkTyVarTy tv
                         , hole_loc  = ct_loc }
       ; emitHole hole }
  where
    occ = mkTyVarOcc "_"

emitNamedTypeHole :: (IsLocal m m, HasDynFlags m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => (Name, TcTyVar) -> m ()
emitNamedTypeHole (name, tv)
  = do { ct_loc <- setSrcSpan (nameSrcSpan name) $
                   getCtLocM (TypeHoleOrigin occ) Nothing
       ; let hole = Hole { hole_sort = TypeHole
                         , hole_occ  = occ
                         , hole_ty   = mkTyVarTy tv
                         , hole_loc  = ct_loc }
       ; emitHole hole }
  where
    occ       = nameOccName name

{- Note [Constraints and errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#12124):

  foo :: Maybe Int
  foo = return (case Left 3 of
                  Left -> 1  -- Hard error here!
                  _    -> 0)

The call to 'return' will generate a (Monad m) wanted constraint; but
then there'll be "hard error" (i.e. an exception in the TcM monad), from
the unsaturated Left constructor pattern.

We'll recover in tcPolyBinds, using recoverM.  But then the final
tcSimplifyTop will see that (Monad m) constraint, with 'm' utterly
un-filled-in, and will emit a misleading error message.

The underlying problem is that an exception interrupts the constraint
gathering process. Bottom line: if we have an exception, it's best
simply to discard any gathered constraints.  Hence in 'attemptM' we
capture the constraints in a fresh variable, and only emit them into
the surrounding context if we exit normally.  If an exception is
raised, simply discard the collected constraints... we have a hard
error to report.  So this capture-the-emit dance isn't as stupid as it
looks :-).

However suppose we throw an exception inside an invocation of
captureConstraints, and discard all the constraints. Some of those
constraints might be "variable out of scope" Hole constraints, and that
might have been the actual original cause of the exception!  For
example (#12529):
   f = p @ Int
Here 'p' is out of scope, so we get an insoluble Hole constraint. But
the visible type application fails in the monad (throws an exception).
We must not discard the out-of-scope error.

So we /retain the insoluble constraints/ if there is an exception.
Hence:
  - insolublesOnly in tryCaptureConstraints
  - emitConstraints in the Left case of captureConstraints

However note that freshly-generated constraints like (Int ~ Bool), or
((a -> b) ~ Int) are all CNonCanonical, and hence won't be flagged as
insoluble.  The constraint solver does that.  So they'll be discarded.
That's probably ok; but see th/5358 as a not-so-good example:
   t1 :: Int
   t1 x = x   -- Manifestly wrong

   foo = $(...raises exception...)
We report the exception, but not the bug in t1.  Oh well.  Possible
solution: make GHC.Tc.Utils.Unify.uType spot manifestly-insoluble constraints.


************************************************************************
*                                                                      *
             Template Haskell context
*                                                                      *
************************************************************************
-}

recordThUse :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => m ()
recordThUse = do { env <- getGblEnv; writeMutVar (tcg_th_used env) True }

recordThSpliceUse :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => m ()
recordThSpliceUse = do { env <- getGblEnv; writeMutVar (tcg_th_splice_used env) True }

keepAlive :: (IsReader m, HasDynFlags m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => Name -> m ()     -- Record the name in the keep-alive set
keepAlive name
  = do { env <- getGblEnv
       ; traceRn "keep alive" (ppr name)
       ; updMutVar (tcg_keep env) (`extendNameSet` name) }

getStage :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f ThStage
getStage = tcl_th_ctxt <$> getLclEnv

getStageAndBindLevel :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => Name -> f (Maybe (TopLevelFlag, ThLevel, ThStage))
getStageAndBindLevel name = getLclEnv <₪> \ env ->
    lookupNameEnv (tcl_th_bndrs env) name <₪> \ (top_lvl, bind_lvl) ->
    (top_lvl, bind_lvl, tcl_th_ctxt env)

setStage :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => ThStage -> f a -> f a
setStage = locally (env_lclL . tcl_th_ctxtL) . pure

-- | Adds the given modFinalizers to the global environment and set them to use
-- the current local environment.
addModFinalizersWithLclEnv :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv TcLclEnv) => ThModFinalizers -> m ()
addModFinalizersWithLclEnv mod_finalizers
  = do lcl_env <- getLclEnv
       th_modfinalizers_var <- tcg_th_modfinalizers <$> getGblEnv
       updMutVar th_modfinalizers_var $ (:) (lcl_env, mod_finalizers)

{-
************************************************************************
*                                                                      *
             Safe Haskell context
*                                                                      *
************************************************************************
-}

-- | Mark that safe inference has failed
-- See Note [Safe Haskell Overlapping Instances Implementation]
-- although this is used for more than just that failure case.
recordUnsafeInfer :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => WarningMessages -> m ()
recordUnsafeInfer warns = getGblEnv >>= \env -> writeMutVar (tcg_safeInfer env) (False, warns)

-- | Figure out the final correct safe haskell mode
finalSafeMode :: DynFlags -> TcGblEnv -> IO SafeHaskellMode
finalSafeMode dflags tcg_env =
  [ case safeHaskell dflags of
        Sf_None | safeInferOn dflags && safeInf -> Sf_SafeInferred
                | otherwise                     -> Sf_None
        s -> s
  | safeInf <- fst <$> readIORef (tcg_safeInfer tcg_env) ]

-- | Switch instances to safe instances if we're in Safe mode.
fixSafeInstances :: Functor f => SafeHaskellMode -> f ClsInst -> f ClsInst
fixSafeInstances sfMode | sfMode /= Sf_Safe && sfMode /= Sf_SafeInferred = id
fixSafeInstances _ = fmap $ set (is_flagL . isSafeOverlapL) True

{-
************************************************************************
*                                                                      *
             Stuff for the renamer's local env
*                                                                      *
************************************************************************
-}

getLocalRdrEnv :: (IsReader f, EnvType f ~ Env gbl TcLclEnv) => f LocalRdrEnv
getLocalRdrEnv = view (env_lclL . tcl_rdrL)

setLocalRdrEnv :: (IsLocal f f, EnvType f ~ Env gbl TcLclEnv) => LocalRdrEnv -> f a -> f a
setLocalRdrEnv = locally (env_lclL . tcl_rdrL) . pure

{-
************************************************************************
*                                                                      *
             Stuff for interface decls
*                                                                      *
************************************************************************
-}

mkIfLclEnv :: Module -> SDoc -> IsBootInterface -> IfLclEnv
mkIfLclEnv mod loc boot
                   = IfLclEnv { if_mod     = mod,
                                if_loc     = loc,
                                if_boot    = boot,
                                if_nsubst  = Nothing,
                                if_implicits_env = Nothing,
                                if_tv_env  = emptyFsEnv,
                                if_id_env  = emptyFsEnv }

-- | Run an 'IfG' (top-level interface monad) computation inside an existing
-- 'TcRn' (typecheck-renaming monad) computation by initializing an 'IfGblEnv'
-- based on 'TcGblEnv'.
initIfaceTcRn :: (Monad m, HasDynFlags m, IsLocal f m, EnvType f ~ Env IfGblEnv (), EnvType m ~ Env TcGblEnv lcl) => f a -> m a
initIfaceTcRn thing_inside
  = do  { tcg_env <- getGblEnv
        ; dflags <- getDynFlags
        ; let !mod = tcg_semantic_mod tcg_env
              -- When we are instantiating a signature, we DEFINITELY
              -- do not want to knot tie.
              is_instantiate = homeUnitIsDefinite dflags &&
                               not (null (homeUnitInstantiations dflags))
        ; let { if_env = IfGblEnv {
                            if_doc = text "initIfaceTcRn",
                            if_rec_types = (mod, get_type_env) <$ guard (not is_instantiate)
                         }
              ; get_type_env = readMutVar (tcg_type_env_var tcg_env) }
        ; setEnvs (if_env, ()) thing_inside }

-- Used when sucking in a ModIface into a ModDetails to put in
-- the HPT.  Notably, unlike initIfaceCheck, this does NOT use
-- hsc_type_env_var (since we're not actually going to typecheck,
-- so this variable will never get updated!)
initIfaceLoad :: HscEnv -> IfG a -> IO a
initIfaceLoad hsc_env do_this
 = do let gbl_env = IfGblEnv {
                        if_doc = text "initIfaceLoad",
                        if_rec_types = Nothing
                    }
      initTcRnIf 'i' hsc_env gbl_env () do_this

initIfaceCheck :: SDoc -> HscEnv -> IfG a -> IO a
-- Used when checking the up-to-date-ness of the old Iface
-- Initialise the environment with no useful info at all
initIfaceCheck doc hsc_env do_this
 = do let rec_types = case hsc_type_env_var hsc_env of
                         Just (mod,var) -> Just (mod, readMutVar var)
                         Nothing        -> Nothing
          gbl_env = IfGblEnv {
                        if_doc = text "initIfaceCheck" <+> doc,
                        if_rec_types = rec_types
                    }
      initTcRnIf 'i' hsc_env gbl_env () do_this

initIfaceLcl :: (IsLocal f g, EnvType f ~ Env gbl IfLclEnv, EnvType g ~ Env gbl lcl') => Module -> SDoc -> IsBootInterface -> f a -> g a
initIfaceLcl mod loc_doc hi_boot_file = setLclEnv (mkIfLclEnv mod loc_doc hi_boot_file)

-- | Initialize interface typechecking, but with a 'NameShape'
-- to apply when typechecking top-level 'OccName's (see
-- 'lookupIfaceTop')
initIfaceLclWithSubst :: (IsLocal f g, EnvType f ~ Env gbl IfLclEnv, EnvType g ~ Env gbl lcl') => Module -> SDoc -> IsBootInterface -> NameShape -> f a -> g a
initIfaceLclWithSubst mod loc_doc hi_boot_file nsubst
  = setLclEnv (mkIfLclEnv mod loc_doc hi_boot_file) { if_nsubst = Just nsubst }

getIfModule :: (IsReader f, EnvType f ~ Env gbl IfLclEnv) => f Module
getIfModule = if_mod <$> getLclEnv

--------------------
failIfM :: (IsReader m, HasDynFlags m, MonadIO m, EnvType m ~ Env gbl IfLclEnv) => MsgDoc -> m a
-- The Iface monad doesn't have a place to accumulate errors, so we
-- just fall over fast if one happens; it "shouldn't happen".
-- We use IfL here so that we can get context info out of the local env
failIfM msg
  = do  { env <- getLclEnv
        ; let full_msg = (if_loc env <> colon) $$ nest 2 msg
        ; dflags <- getDynFlags
        ; liftIO (putLogMsg dflags NoReason SevFatal
                   noSrcSpan $ withPprStyle defaultErrStyle full_msg)
        ; failM }

--------------------
forkM_maybe :: SDoc -> TcRnIf gbl IfLclEnv a -> TcRnIf gbl IfLclEnv (Maybe a)
-- Run thing_inside in an interleaved thread.
-- It shares everything with the parent thread, so this is DANGEROUS.
--
-- It returns Nothing if the computation fails
--
-- It's used for lazily type-checking interface
-- signatures, which is pretty benign

forkM_maybe doc thing_inside
 = -- see Note [Masking exceptions in forkM_maybe]
   unsafeInterleaveM $ uninterruptibleMaskM_ $
        do { traceIf (text "Starting fork {" <+> doc)
           ; tryM (locally (env_lclL . if_locL) ($$ doc) thing_inside) >>= \ case
                Right r  -> Just r <$ traceIf (text "} ending fork" <+> doc)
                Left exn -> do {
                    -- Bleat about errors in the forked thread, if -ddump-if-trace is on
                    -- Otherwise we silently discard errors. Errors can legitimately
                    -- happen when compiling interface signatures (see tcInterfaceSigs)
                      whenDOptM Opt_D_dump_if_trace $ do
                          dflags <- getDynFlags
                          let msg = hang (text "forkM failed:" <+> doc)
                                       2 (text (show exn))
                          liftIO $ putLogMsg dflags
                                             NoReason
                                             SevFatal
                                             noSrcSpan
                                             $ withPprStyle defaultErrStyle msg

                    ; Nothing <$ traceIf (text "} ending fork (badly)" <+> doc) }}

forkM :: SDoc -> TcRnIf gbl IfLclEnv a -> TcRnIf gbl IfLclEnv a
forkM doc thing_inside = fromMaybe e <$> forkM_maybe doc thing_inside
  where
    e = pgmError "Cannot continue after interface file error"

setImplicitEnvM :: (IsLocal f f, EnvType f ~ Env gbl IfLclEnv) => TypeEnv -> f a -> f a
setImplicitEnvM = locally (env_lclL . if_implicits_envL) . pure . pure

{-
Note [Masking exceptions in forkM_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using GHC-as-API it must be possible to interrupt snippets of code
executed using runStmt (#1381). Since commit 02c4ab04 this is almost possible
by throwing an asynchronous interrupt to the GHC thread. However, there is a
subtle problem: runStmt first typechecks the code before running it, and the
exception might interrupt the type checker rather than the code. Moreover, the
typechecker might be inside an unsafeInterleaveIO (through forkM_maybe), and
more importantly might be inside an exception handler inside that
unsafeInterleaveIO. If that is the case, the exception handler will rethrow the
asynchronous exception as a synchronous exception, and the exception will end
up as the value of the unsafeInterleaveIO thunk (see #8006 for a detailed
discussion).  We don't currently know a general solution to this problem, but
we can use uninterruptibleMask_ to avoid the situation.
-}

-- | Get the next cost centre index associated with a given name.
getCCIndexM :: (IsReader m, MonadIO m, EnvType m ~ Env gbl lcl) => (gbl -> TcRef CostCentreState) -> FastString -> m CostCentreIndex
getCCIndexM get_ccs nm = do
  env <- getGblEnv
  let cc_st_ref = get_ccs env
  cc_st <- readMutVar cc_st_ref
  let (idx, cc_st') = getCCIndex nm cc_st
  idx <$ writeMutVar cc_st_ref cc_st'

-- | See 'getCCIndexM'.
getCCIndexTcM :: (IsReader m, MonadIO m, EnvType m ~ Env TcGblEnv lcl) => FastString -> m CostCentreIndex
getCCIndexTcM = getCCIndexM tcg_cc_st
