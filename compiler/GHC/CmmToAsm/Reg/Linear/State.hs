{-# LANGUAGE CPP, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | State monad for the linear register allocator.

--      Here we keep all the state that the register allocator keeps track
--      of as it walks the instructions in a basic block.

module GHC.CmmToAsm.Reg.Linear.State (
        RA_State(..),
        RegM,
        runR,

        spillR,
        loadR,

        getFreeRegsR,
        setFreeRegsR,

        getAssigR,
        setAssigR,

        getBlockAssigR,
        setBlockAssigR,

        setDeltaR,
        getDeltaR,

        getConfig,
        getPlatform,

        recordSpill,
        recordFixupBlock
)
where

import GHC.Prelude

import GHC.CmmToAsm.Reg.Linear.Stats
import GHC.CmmToAsm.Reg.Linear.StackMap
import GHC.CmmToAsm.Reg.Linear.Base
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Config
import GHC.Platform.Reg
import GHC.Cmm.BlockId

import GHC.Platform
import GHC.Types.Unique
import GHC.Types.Unique.Supply

import Control.Monad (ap)

type RA_Result freeRegs a = (# RA_State freeRegs, a #)

pattern RA_Result :: a -> b -> (# a, b #)
pattern RA_Result a b = (# a, b #)
{-# COMPLETE RA_Result #-}

-- | The register allocator monad type.
newtype RegM freeRegs a
        = RegM { unReg :: RA_State freeRegs -> RA_Result freeRegs a }
        deriving (Functor)

instance Applicative (RegM freeRegs) where
      pure a  =  RegM $ \s -> RA_Result s a
      (<*>) = ap

instance Monad (RegM freeRegs) where
  m >>= k   =  RegM $ \s -> case unReg m s of { RA_Result s a -> unReg (k a) s }

-- | Get native code generator configuration
getConfig :: RegM a NCGConfig
getConfig = RegM $ \s -> RA_Result s (ra_config s)

-- | Get target platform from native code generator configuration
getPlatform :: RegM a Platform
getPlatform = ncgPlatform <$> getConfig

-- | Run a computation in the RegM register allocator monad.
runR    :: NCGConfig
        -> BlockAssignment freeRegs
        -> freeRegs
        -> RegMap Loc
        -> StackMap
        -> UniqSupply
        -> RegM freeRegs a
        -> (BlockAssignment freeRegs, StackMap, RegAllocStats, a)

runR config block_assig freeregs assig stack us thing =
  case unReg thing
        (RA_State
                { ra_blockassig = block_assig
                , ra_freeregs   = freeregs
                , ra_assig      = assig
                , ra_delta      = 0{-???-}
                , ra_stack      = stack
                , ra_us         = us
                , ra_spills     = []
                , ra_config     = config
                , ra_fixups     = [] })
   of
        RA_Result state returned_thing
         ->     (ra_blockassig state, ra_stack state, makeRAStats state, returned_thing)


-- | Make register allocator stats from its final state.
makeRAStats :: RA_State freeRegs -> RegAllocStats
makeRAStats state
        = RegAllocStats
        { ra_spillInstrs        = binSpillReasons (ra_spills state)
        , ra_fixupList          = ra_fixups state }


spillR :: Instruction instr
       => Reg -> Unique -> RegM freeRegs (instr, Int)

spillR reg temp = RegM $ \s ->
  let (stack1,slot) = getStackSlotFor (ra_stack s) temp
      instr  = mkSpillInstr (ra_config s) reg (ra_delta s) slot
  in
  RA_Result s{ra_stack=stack1} (instr,slot)


loadR :: Instruction instr
      => Reg -> Int -> RegM freeRegs instr

loadR reg slot = RegM $ \s ->
  RA_Result s (mkLoadInstr (ra_config s) reg (ra_delta s) slot)

getFreeRegsR :: RegM freeRegs freeRegs
getFreeRegsR = RegM $ \ s@RA_State{ra_freeregs = freeregs} ->
  RA_Result s freeregs

setFreeRegsR :: freeRegs -> RegM freeRegs ()
setFreeRegsR regs = RegM $ \ s ->
  RA_Result s{ra_freeregs = regs} ()

getAssigR :: RegM freeRegs (RegMap Loc)
getAssigR = RegM $ \ s@RA_State{ra_assig = assig} ->
  RA_Result s assig

setAssigR :: RegMap Loc -> RegM freeRegs ()
setAssigR assig = RegM $ \ s ->
  RA_Result s{ra_assig=assig} ()

getBlockAssigR :: RegM freeRegs (BlockAssignment freeRegs)
getBlockAssigR = RegM $ \ s@RA_State{ra_blockassig = assig} ->
  RA_Result s assig

setBlockAssigR :: BlockAssignment freeRegs -> RegM freeRegs ()
setBlockAssigR assig = RegM $ \ s ->
  RA_Result s{ra_blockassig = assig} ()

setDeltaR :: Int -> RegM freeRegs ()
setDeltaR n = RegM $ \ s ->
  RA_Result s{ra_delta = n} ()

getDeltaR :: RegM freeRegs Int
getDeltaR = RegM $ \s -> RA_Result s (ra_delta s)

instance MonadUnique (RegM freeRegs) where
  getUniqueSupplyM = RegM $ \s -> RA_Result s (ra_us s)

  getUniqueM = RegM $ \s ->
    case takeUniqFromSupply (ra_us s) of
      (uniq, us) -> RA_Result s{ra_us = us} uniq

-- | Record that a spill instruction was inserted, for profiling.
recordSpill :: SpillReason -> RegM freeRegs ()
recordSpill spill
    = RegM $ \s -> RA_Result (s { ra_spills = spill : ra_spills s }) ()

-- | Record a created fixup block
recordFixupBlock :: BlockId -> BlockId -> BlockId -> RegM freeRegs ()
recordFixupBlock from between to
    = RegM $ \s -> RA_Result (s { ra_fixups = (from,between,to) : ra_fixups s }) ()
