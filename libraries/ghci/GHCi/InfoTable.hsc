{-# LANGUAGE CPP, MagicHash, ScopedTypeVariables #-}

-- |
-- Run-time info table support.  This module provides support for
-- creating and reading info tables /in the running program/.
-- We use the RTS data structures directly via hsc2hs.
--
module GHCi.InfoTable
  ( mkConInfoTable
  , peekItbl, StgInfoTable(..)
  , conInfoPtr
  ) where

import Foreign
import Foreign.C
import GHC.Ptr
import GHC.Exts
import System.IO.Unsafe

mkConInfoTable
   :: Int     -- ptr words
   -> Int     -- non-ptr words
   -> Int     -- constr tag
   -> [Word8]  -- con desc
   -> IO (Ptr ())
      -- resulting info table is allocated with allocateExec(), and
      -- should be freed with freeExec().

mkConInfoTable ptr_words nonptr_words tag con_desc =
  castFunPtrToPtr <$> newExecConItbl itbl con_desc
  where
     entry_addr = stg_interp_constr_entry
     code' = mkJumpToAddr entry_addr
     itbl  = StgInfoTable {
                 entry = if ghciTablesNextToCode
                         then Nothing
                         else Just entry_addr,
                 ptrs  = fromIntegral ptr_words,
                 nptrs = fromIntegral nonptr_words,
                 tipe  = fromIntegral cONSTR,
                 srtlen = fromIntegral tag,
                 code  = if ghciTablesNextToCode
                         then Just code'
                         else Nothing
              }


-- -----------------------------------------------------------------------------
-- Building machine code fragments for a constructor's entry code

type ItblCodes = Either [Word8] [Word32]

funPtrToInt :: FunPtr a -> Int
funPtrToInt (FunPtr a) = I## (addr2Int## a)

data Arch = ArchSPARC | ArchPPC | ArchX86 | ArchX86_64 | ArchAlpha | ArchARM
 deriving Show

platform :: Arch
platform =
#if defined(sparc_HOST_ARCH)
       ArchSparc
#elif defined(ppc_HOST_ARCH)
       ArchPPC
#elif defined(x86_HOST_ARCH)
       ArchX86
#elif defined(x86_64_HOST_ARCH)
       ArchX86_64
#elif defined(alpha_HOST_ARCH)
       ArchAlpha
#elif defined(arm_HOST_ARCH)
       ArchARM
#endif

mkJumpToAddr :: EntryFunPtr -> ItblCodes
mkJumpToAddr a = case platform of
    ArchSPARC ->
        -- After some consideration, we'll try this, where
        -- 0x55555555 stands in for the address to jump to.
        -- According to includes/rts/MachRegs.h, %g3 is very
        -- likely indeed to be baggable.
        --
        --   0000 07155555              sethi   %hi(0x55555555), %g3
        --   0004 8610E155              or      %g3, %lo(0x55555555), %g3
        --   0008 81C0C000              jmp     %g3
        --   000c 01000000              nop

        let w32 = fromIntegral (funPtrToInt a)

            hi22, lo10 :: Word32 -> Word32
            lo10 x = x .&. 0x3FF
            hi22 x = (x `shiftR` 10) .&. 0x3FFFF

        in Right [ 0x07000000 .|. (hi22 w32),
                   0x8610E000 .|. (lo10 w32),
                   0x81C0C000,
                   0x01000000 ]

    ArchPPC ->
        -- We'll use r12, for no particular reason.
        -- 0xDEADBEEF stands for the address:
        -- 3D80DEAD lis r12,0xDEAD
        -- 618CBEEF ori r12,r12,0xBEEF
        -- 7D8903A6 mtctr r12
        -- 4E800420 bctr

        let w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
        in Right [ 0x3D800000 .|. hi16 w32,
                   0x618C0000 .|. lo16 w32,
                   0x7D8903A6, 0x4E800420 ]

    ArchX86 ->
        -- Let the address to jump to be 0xWWXXYYZZ.
        -- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
        -- which is
        -- B8 ZZ YY XX WW FF E0

        let w32 = fromIntegral (funPtrToInt a) :: Word32
            insnBytes :: [Word8]
            insnBytes
               = [0xB8, byte0 w32, byte1 w32,
                        byte2 w32, byte3 w32,
                  0xFF, 0xE0]
        in
            Left insnBytes

    ArchX86_64 ->
        -- Generates:
        --      jmpq *.L1(%rip)
        --      .align 8
        -- .L1:
        --      .quad <addr>
        --
        -- which looks like:
        --     8:   ff 25 02 00 00 00     jmpq   *0x2(%rip)      # 10 <f+0x10>
        -- with addr at 10.
        --
        -- We need a full 64-bit pointer (we can't assume the info table is
        -- allocated in low memory).  Assuming the info pointer is aligned to
        -- an 8-byte boundary, the addr will also be aligned.

        let w64 = fromIntegral (funPtrToInt a) :: Word64
            insnBytes :: [Word8]
            insnBytes
               = [0xff, 0x25, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
                  byte0 w64, byte1 w64, byte2 w64, byte3 w64,
                  byte4 w64, byte5 w64, byte6 w64, byte7 w64]
        in
            Left insnBytes

    ArchAlpha ->
        let w64 = fromIntegral (funPtrToInt a) :: Word64
        in Right [ 0xc3800000      -- br   at, .+4
                 , 0xa79c000c      -- ldq  at, 12(at)
                 , 0x6bfc0000      -- jmp  (at)    # with zero hint -- oh well
                 , 0x47ff041f      -- nop
                 , fromIntegral (w64 .&. 0x0000FFFF)
                 , fromIntegral ((w64 `shiftR` 32) .&. 0x0000FFFF) ]

    ArchARM { } ->
        -- Generates Arm sequence,
        --      ldr r1, [pc, #0]
        --      bx r1
        --
        -- which looks like:
        --     00000000 <.addr-0x8>:
        --     0:       00109fe5    ldr    r1, [pc]      ; 8 <.addr>
        --     4:       11ff2fe1    bx     r1
        let w32 = fromIntegral (funPtrToInt a) :: Word32
        in Left [ 0x00, 0x10, 0x9f, 0xe5
                , 0x11, 0xff, 0x2f, 0xe1
                , byte0 w32, byte1 w32, byte2 w32, byte3 w32]


byte0 :: (Integral w) => w -> Word8
byte0 w = fromIntegral w

byte1, byte2, byte3, byte4, byte5, byte6, byte7
       :: (Integral w, Bits w) => w -> Word8
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)


-- -----------------------------------------------------------------------------
-- read & write intfo tables

-- Get definitions for the structs, constants & config etc.
#include "Rts.h"

-- entry point for direct returns for created constr itbls
foreign import ccall "&stg_interp_constr_entry"
    stg_interp_constr_entry :: EntryFunPtr

-- Ultra-minimalist version specially for constructors
#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#elif SIZEOF_VOID_P == 4
type HalfWord = Word16
#else
#error Uknown SIZEOF_VOID_P
#endif

data StgConInfoTable = StgConInfoTable {
   conDesc   :: Ptr Word8,
   infoTable :: StgInfoTable
}

type EntryFunPtr = FunPtr (Ptr () -> IO (Ptr ()))

data StgInfoTable = StgInfoTable {
   entry  :: Maybe EntryFunPtr, -- Just <=> not ghciTablesNextToCode
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: HalfWord,
   srtlen :: HalfWord,
   code   :: Maybe ItblCodes -- Just <=> ghciTablesNextToCode
  }

pokeConItbl
  :: Ptr StgConInfoTable -> Ptr StgConInfoTable -> StgConInfoTable
  -> IO ()
pokeConItbl wr_ptr ex_ptr itbl = do
  let _con_desc = conDesc itbl `minusPtr` (ex_ptr `plusPtr` conInfoTableSizeB)
#if defined(TABLES_NEXT_TO_CODE)
  (#poke StgConInfoTable, con_desc) wr_ptr _con_desc
#else
  (#poke StgConInfoTable, con_desc) wr_ptr (conDesc itbl)
#endif
  pokeItbl (wr_ptr `plusPtr` (#offset StgConInfoTable, i)) (infoTable itbl)

sizeOfEntryCode :: Int
sizeOfEntryCode
  | not ghciTablesNextToCode = 0
  | otherwise =
     case mkJumpToAddr undefined of
       Left  xs -> sizeOf (head xs) * length xs
       Right xs -> sizeOf (head xs) * length xs

pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl a0 itbl = do
#if !defined(TABLES_NEXT_TO_CODE)
  (#poke StgInfoTable, entry) a0 (fromJust (entry itbl))
#endif
  (#poke StgInfoTable, layout.payload.ptrs) a0 (ptrs itbl)
  (#poke StgInfoTable, layout.payload.nptrs) a0 (nptrs itbl)
  (#poke StgInfoTable, type) a0 (tipe itbl)
  (#poke StgInfoTable, srt_bitmap) a0 (srtlen itbl)
#if defined(TABLES_NEXT_TO_CODE)
  let code_offset = (a0 `plusPtr` (#offset StgInfoTable, code))
  case code itbl of
    Nothing -> return ()
    Just (Left xs) -> pokeArray code_offset xs
    Just (Right xs) -> pokeArray code_offset xs
#endif

peekItbl :: Ptr StgInfoTable -> IO StgInfoTable
peekItbl a0 = do
#if defined(TABLES_NEXT_TO_CODE)
  let entry' = Nothing
#else
  entry' <- Just <$> (#peek StgInfoTable, entry) a0
#endif
  ptrs' <- (#peek StgInfoTable, layout.payload.ptrs) a0
  nptrs' <- (#peek StgInfoTable, layout.payload.nptrs) a0
  tipe' <- (#peek StgInfoTable, type) a0
  srtlen' <- (#peek StgInfoTable, srt_bitmap) a0
  return StgInfoTable
    { entry  = entry'
    , ptrs   = ptrs'
    , nptrs  = nptrs'
    , tipe   = tipe'
    , srtlen = srtlen'
    , code   = Nothing
    }

newExecConItbl :: StgInfoTable -> [Word8] -> IO (FunPtr ())
newExecConItbl obj con_desc
   = alloca $ \pcode -> do
        let lcon_desc = length con_desc + 1{- null terminator -}
            sz = fromIntegral ((#size StgConInfoTable) + sizeOfEntryCode)
               -- Note: we need to allocate the conDesc string next to the info
               -- table, because on a 64-bit platform we reference this string
               -- with a 32-bit offset relative to the info table, so if we
               -- allocated the string separately it might be out of range.
        wr_ptr <- _allocateExec (sz + fromIntegral lcon_desc) pcode
        ex_ptr <- peek pcode
        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz
                                    , infoTable = obj }
        pokeConItbl wr_ptr ex_ptr cinfo
        pokeArray0 0 (castPtr wr_ptr `plusPtr` fromIntegral sz) con_desc
        _flushExec sz ex_ptr -- Cache flush (if needed)
        return (castPtrToFunPtr ex_ptr)

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)

foreign import ccall unsafe "flushExec"
  _flushExec :: CUInt -> Ptr a -> IO ()

-- | Convert a pointer to an StgConInfo into an info pointer that can be
-- used in the header of a closure.
conInfoPtr :: Ptr () -> Ptr ()
conInfoPtr ptr
 | ghciTablesNextToCode = ptr `plusPtr` (#size StgConInfoTable)
 | otherwise            = ptr

-- -----------------------------------------------------------------------------
-- Constants and config

wORD_SIZE :: Int
wORD_SIZE = (#const SIZEOF_HSINT)

fixedInfoTableSizeB :: Int
fixedInfoTableSizeB = 2 * wORD_SIZE

profInfoTableSizeB :: Int
profInfoTableSizeB = (#size StgProfInfo)

stdInfoTableSizeB :: Int
stdInfoTableSizeB
  = (if ghciTablesNextToCode then 0 else wORD_SIZE)
  + (if rtsIsProfiled then profInfoTableSizeB else 0)
  + fixedInfoTableSizeB

conInfoTableSizeB :: Int
conInfoTableSizeB = stdInfoTableSizeB + wORD_SIZE

foreign import ccall unsafe "rts_isProfiled" rtsIsProfiledIO :: IO CInt

rtsIsProfiled :: Bool
rtsIsProfiled = unsafeDupablePerformIO rtsIsProfiledIO /= 0

cONSTR :: Int   -- Defined in ClosureTypes.h
cONSTR = (#const CONSTR)

ghciTablesNextToCode :: Bool
#ifdef TABLES_NEXT_TO_CODE
ghciTablesNextToCode = True
#else
ghciTablesNextToCode = False
#endif
