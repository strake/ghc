{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

#include "lens.h"

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
-- The native code generator's monad.
--
-- -----------------------------------------------------------------------------

module GHC.CmmToAsm.Monad (
        NcgImpl(..),
        NatM_State(..), mkNatM_State,

        NatM, -- instance Monad
        initNat,
        initConfig,
        addImportNat,
        addNodeBetweenNat,
        addImmediateSuccessorNat,
        getConfig,
        getPlatform,
        getDeltaNat,
        getThisModuleNat,
        getBlockIdNat,
        getNewLabelNat,
        getNewRegNat,
        getNewRegPairNat,
        getPicBaseMaybeNat,
        getPicBaseNat,
        getDynFlags,
        getModLoc,
        getFileId,
        getDebugBlock,

        DwarfFiles,

        natm_usL,
        natm_deltaL,
        natm_importsL,
        natm_picL,
        natm_dflagsL,
        natm_configL,
        natm_this_moduleL,
        natm_modlocL,
        natm_fileidL,
        natm_debug_mapL,
        natm_cfgL,
)

where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Reg
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.CLabel           ( CLabel )
import GHC.Cmm.DebugBlock
import GHC.Data.FastString      ( FastString )
import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Driver.Session
import GHC.Unit.Module

import GHC.Utils.Lens.Monad
import GHC.Utils.Monad.State.Lazy (State (..), runState)
import GHC.Utils.Outputable (SDoc, ppr)
import GHC.Utils.Panic      (pprPanic)
import GHC.CmmToAsm.CFG

import Data.Functor.State.Class (IsState (..), gets)

data NcgImpl statics instr jumpDest = NcgImpl {
    ncgConfig                 :: !NCGConfig,
    cmmTopCodeGen             :: RawCmmDecl -> NatM [NatCmmDecl statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmDecl statics instr),
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    canShortcut               :: instr -> Maybe jumpDest,
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    pprNatCmmDecl             :: NatCmmDecl statics instr -> SDoc,
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncgExpandTop              :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgAllocMoreStack         :: Int -> NatCmmDecl statics instr
                              -> UniqSM (NatCmmDecl statics instr, [(BlockId,BlockId)]),
    -- ^ The list of block ids records the redirected jumps to allow us to update
    -- the CFG.
    ncgMakeFarBranches        :: LabelMap RawCmmStatics -> [NatBasicBlock instr] -> [NatBasicBlock instr],
    extractUnwindPoints       :: [instr] -> [UnwindPoint],
    -- ^ given the instruction sequence of a block, produce a list of
    -- the block's 'UnwindPoint's
    -- See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock"
    -- and Note [Unwinding information in the NCG] in this module.
    invertCondBranches        :: Maybe CFG -> LabelMap RawCmmStatics -> [NatBasicBlock instr]
                              -> [NatBasicBlock instr]
    -- ^ Turn the sequence of @jcc l1; jmp l2@ into @jncc l2; \<block_l1>@
    -- when possible.
    }

data NatM_State
        = NatM_State {
                natm_us          :: UniqSupply,
                natm_delta       :: Int,
                natm_imports     :: [(CLabel)],
                natm_pic         :: Maybe Reg,
                natm_dflags      :: DynFlags,
                natm_config      :: NCGConfig,
                natm_this_module :: Module,
                natm_modloc      :: ModLocation,
                natm_fileid      :: DwarfFiles,
                natm_debug_map   :: LabelMap DebugBlock,
                natm_cfg         :: CFG
        -- ^ Having a CFG with additional information is essential for some
        -- operations. However we can't reconstruct all information once we
        -- generated instructions. So instead we update the CFG as we go.
        }

LENS_FIELD(natm_usL, natm_us)
LENS_FIELD(natm_deltaL, natm_delta)
LENS_FIELD(natm_importsL, natm_imports)
LENS_FIELD(natm_picL, natm_pic)
LENS_FIELD(natm_dflagsL, natm_dflags)
LENS_FIELD(natm_configL, natm_config)
LENS_FIELD(natm_this_moduleL, natm_this_module)
LENS_FIELD(natm_modlocL, natm_modloc)
LENS_FIELD(natm_fileidL, natm_fileid)
LENS_FIELD(natm_debug_mapL, natm_debug_map)
LENS_FIELD(natm_cfgL, natm_cfg)

type DwarfFiles = UniqFM FastString (FastString, Int)

newtype NatM result = NatM (State NatM_State result)
    deriving (Functor)
    deriving (Applicative, Monad) via State NatM_State

unNat :: NatM a -> NatM_State -> (a, NatM_State)
unNat (NatM x) = runState x

instance IsState NatM where
    type StateType NatM = NatM_State
    state = NatM . state

mkNatM_State :: UniqSupply -> Int -> DynFlags -> Module -> ModLocation ->
                DwarfFiles -> LabelMap DebugBlock -> CFG -> NatM_State
mkNatM_State us delta dflags this_mod = \loc dwf dbg cfg -> NatM_State
                        { natm_us = us
                        , natm_delta = delta
                        , natm_imports = []
                        , natm_pic = Nothing
                        , natm_dflags = dflags
                        , natm_config = initConfig dflags
                        , natm_this_module = this_mod
                        , natm_modloc = loc
                        , natm_fileid = dwf
                        , natm_debug_map = dbg
                        , natm_cfg = cfg
                        }

-- | Initialize the native code generator configuration from the DynFlags
initConfig :: DynFlags -> NCGConfig
initConfig dflags = NCGConfig
   { ncgPlatform              = targetPlatform dflags
   , ncgProcAlignment         = cmmProcAlignment dflags
   , ncgDebugLevel            = debugLevel dflags
   , ncgExternalDynamicRefs   = gopt Opt_ExternalDynamicRefs dflags
   , ncgPIC                   = positionIndependent dflags
   , ncgInlineThresholdMemcpy = fromIntegral $ maxInlineMemcpyInsns dflags
   , ncgInlineThresholdMemset = fromIntegral $ maxInlineMemsetInsns dflags
   , ncgSplitSections         = gopt Opt_SplitSections dflags
   , ncgRegsIterative         = gopt Opt_RegsIterative dflags
   , ncgAsmLinting            = gopt Opt_DoAsmLinting dflags

     -- With -O1 and greater, the cmmSink pass does constant-folding, so
     -- we don't need to do it again in the native code generator.
   , ncgDoConstantFolding     = optLevel dflags < 1

   , ncgDumpRegAllocStages    = dopt Opt_D_dump_asm_regalloc_stages dflags
   , ncgDumpAsmStats          = dopt Opt_D_dump_asm_stats dflags
   , ncgDumpAsmConflicts      = dopt Opt_D_dump_asm_conflicts dflags
   , ncgBmiVersion            = case platformArch (targetPlatform dflags) of
                                 ArchX86_64 -> bmiVersion dflags
                                 ArchX86    -> bmiVersion dflags
                                 _          -> Nothing

     -- We Assume  SSE1 and SSE2 operations are available on both
     -- x86 and x86_64. Historically we didn't default to SSE2 and
     -- SSE1 on x86, which results in defacto nondeterminism for how
     -- rounding behaves in the associated x87 floating point instructions
     -- because variations in the spill/fpu stack placement of arguments for
     -- operations would change the precision and final result of what
     -- would otherwise be the same expressions with respect to single or
     -- double precision IEEE floating point computations.
   , ncgSseVersion =
      let v | sseVersion dflags < Just SSE2 = Just SSE2
            | otherwise                     = sseVersion dflags
      in case platformArch (targetPlatform dflags) of
            ArchX86_64 -> v
            ArchX86    -> v
            _          -> Nothing
   }

initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat = flip unNat

instance MonadUnique NatM where
  getUniqueSupplyM = stating natm_usL splitUniqSupply
  getUniqueM = stating natm_usL takeUniqFromSupply

instance HasDynFlags NatM where
    getDynFlags = gets natm_dflags

getDeltaNat :: NatM Int
getDeltaNat = gets natm_delta

getThisModuleNat :: NatM Module
getThisModuleNat = gets natm_this_module

addImportNat :: CLabel -> NatM ()
addImportNat = modifying_ natm_importsL . (:)

-- | Record that we added a block between `from` and `old`.
addNodeBetweenNat :: BlockId -> BlockId -> BlockId -> NatM ()
addNodeBetweenNat from between to
 = do   df <- getDynFlags
        let jmpWeight = fromIntegral . uncondWeight . cfgWeightInfo $ df
        modifying_ natm_cfgL (updateCfg jmpWeight from between to)
  where
    -- When transforming A -> B to A -> A' -> B
    -- A -> A' keeps the old edge info while
    -- A' -> B gets the info for an unconditional
    -- jump.
    updateCfg weight from between old m
        | Just info <- getEdgeInfo from old m
        = addEdge from between info .
          addWeightEdge between old weight .
          delEdge from old $ m
        | otherwise
        = pprPanic "Failed to update cfg: Untracked edge" (ppr (from,to))


-- | Place `succ` after `block` and change any edges
--   block -> X to `succ` -> X
addImmediateSuccessorNat :: BlockId -> BlockId -> NatM ()
addImmediateSuccessorNat block succ = do
   dflags <- getDynFlags
   modifying_ natm_cfgL (addImmediateSuccessor dflags block succ)

getBlockIdNat :: NatM BlockId
getBlockIdNat = mkBlockId <$> getUniqueM

getNewLabelNat :: NatM CLabel
getNewLabelNat = blockLbl <$> getBlockIdNat

getNewRegNat :: Format -> NatM Reg
getNewRegNat rep =
  [ RegVirtual $ targetMkVirtualReg platform u rep
  | u <- getUniqueM, platform <- getPlatform ]

getNewRegPairNat :: Format -> NatM (Reg,Reg)
getNewRegPairNat rep =
  [ (lo, hi)
  | u <- getUniqueM
  , platform <- getPlatform
  , let vLo = targetMkVirtualReg platform u rep
        lo  = RegVirtual $ targetMkVirtualReg platform u rep
        hi  = RegVirtual $ getHiVirtualRegFromLo vLo
  ]

getPicBaseMaybeNat :: NatM (Maybe Reg)
getPicBaseMaybeNat = gets natm_pic

getPicBaseNat :: Format -> NatM Reg
getPicBaseNat rep = getPicBaseMaybeNat >>= \ case
                Just picBase -> return picBase
                Nothing -> do
                        reg <- getNewRegNat rep
                        state \st -> (reg, st { natm_pic = Just reg })

getModLoc :: NatM ModLocation
getModLoc = gets natm_modloc

-- | Get native code generator configuration
getConfig :: NatM NCGConfig
getConfig = gets natm_config

-- | Get target platform from native code generator configuration
getPlatform :: NatM Platform
getPlatform = ncgPlatform <$> getConfig

getFileId :: FastString -> NatM Int
getFileId f = state $ \st ->
  case lookupUFM (natm_fileid st) f of
    Just (_,n) -> (n, st)
    Nothing    -> let n = 1 + sizeUFM (natm_fileid st)
                      fids = addToUFM (natm_fileid st) f (f,n)
                  in n `seq` fids `seq` (n, st { natm_fileid = fids  })

getDebugBlock :: Label -> NatM (Maybe DebugBlock)
getDebugBlock l = gets (mapLookup l . natm_debug_map)
