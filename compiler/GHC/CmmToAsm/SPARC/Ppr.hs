{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.CmmToAsm.SPARC.Ppr (
        pprNatCmmDecl,
        pprBasicBlock,
        pprData,
        pprInstr,
        pprFormat,
        pprImm,
        pprDataItem
)

where

import GHC.Prelude

import GHC.CmmToAsm.SPARC.Regs
import GHC.CmmToAsm.SPARC.Instr
import GHC.CmmToAsm.SPARC.Cond
import GHC.CmmToAsm.SPARC.Imm
import GHC.CmmToAsm.SPARC.AddrMode
import GHC.CmmToAsm.SPARC.Base
import GHC.Platform.Reg
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.Ppr() -- For Outputable instances
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.Data.Collections

import GHC.Types.Unique ( pprUniqueAlways )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Data.FastString

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section
  $$ pprDatas (ncgPlatform config) dats

pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config in
  case topInfoTable proc of
    Nothing ->
        -- special case for code without info table:
        pprSectionAlign config (Section Text lbl) $$
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock platform top_info) blocks)

    Just (CmmStaticsRaw info_lbl _) ->
      (mwhen (platformHasSubsectionsViaSymbols platform) $
               pprSectionAlign config dspSection $$
               pdoc platform (mkDeadStripPreventer info_lbl) <> char ':') $$
      vcat (map (pprBasicBlock platform top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (mwhen (platformHasSubsectionsViaSymbols platform) $
       -- See Note [Subsections Via Symbols] in X86/Ppr.hs
                text "\t.long "
            <+> pdoc platform info_lbl
            <+> char '-'
            <+> pdoc platform (mkDeadStripPreventer info_lbl))

dspSection :: Section
dspSection = Section Text $
    panic "subsections-via-symbols doesn't combine with split-sections"

pprBasicBlock :: Platform -> LabelMap RawCmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock platform info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel platform (blockLbl blockid) $$
    vcat (map (pprInstr platform) instrs)
  where
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> mempty
       Just (CmmStaticsRaw info_lbl info) ->
           pprAlignForSection Text $$
           vcat (map (pprData platform) info) $$
           pprLabel platform info_lbl


pprDatas :: Platform -> RawCmmStatics -> SDoc
-- See note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas platform (CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl platform alias
    $$ text ".equiv" <+> pdoc platform alias <> comma <> pdoc platform (CmmLabel ind')
pprDatas platform (CmmStaticsRaw lbl dats) = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: Platform -> CmmStatic -> SDoc
pprData platform d = case d of
   CmmString str          -> pprString str
   CmmFileEmbed path      -> pprFileEmbed path
   CmmUninitialised bytes -> text ".skip " <> int bytes
   CmmStaticLit lit       -> pprDataItem platform lit

pprGloblDecl :: Platform -> CLabel -> SDoc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = mempty
  | otherwise = text ".global " <> pdoc platform lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
    = mwhen (platformOS platform == OSLinux && externallyVisibleCLabel lbl) $
      text ".type " <> pdoc platform lbl <> ptext (sLit ", @object")

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeAndSizeDecl platform lbl
   $$ (pdoc platform lbl <> char ':')

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance OutputableP Platform Instr where
    pdoc = pprInstr


-- | Pretty print a register.
pprReg :: Reg -> SDoc
pprReg reg
 = case reg of
        RegVirtual vr
         -> case vr of
                VirtualRegI   u -> text "%vI_"   <> pprUniqueAlways u
                VirtualRegHi  u -> text "%vHi_"  <> pprUniqueAlways u
                VirtualRegF   u -> text "%vF_"   <> pprUniqueAlways u
                VirtualRegD   u -> text "%vD_"   <> pprUniqueAlways u


        RegReal rr
         -> case rr of
                RealRegSingle r1
                 -> pprReg_ofRegNo r1

                RealRegPair r1 r2
                 -> text "(" <> pprReg_ofRegNo r1
                 <> vbar     <> pprReg_ofRegNo r2
                 <> text ")"



-- | Pretty print a register name, based on this register number.
--   The definition has been unfolded so we get a jump-table in the
--   object code. This function is called quite a lot when emitting
--   the asm file..
--
pprReg_ofRegNo :: Int -> SDoc
pprReg_ofRegNo i
 = ptext
    (case i of {
         0 -> sLit "%g0";   1 -> sLit "%g1";
         2 -> sLit "%g2";   3 -> sLit "%g3";
         4 -> sLit "%g4";   5 -> sLit "%g5";
         6 -> sLit "%g6";   7 -> sLit "%g7";
         8 -> sLit "%o0";   9 -> sLit "%o1";
        10 -> sLit "%o2";  11 -> sLit "%o3";
        12 -> sLit "%o4";  13 -> sLit "%o5";
        14 -> sLit "%o6";  15 -> sLit "%o7";
        16 -> sLit "%l0";  17 -> sLit "%l1";
        18 -> sLit "%l2";  19 -> sLit "%l3";
        20 -> sLit "%l4";  21 -> sLit "%l5";
        22 -> sLit "%l6";  23 -> sLit "%l7";
        24 -> sLit "%i0";  25 -> sLit "%i1";
        26 -> sLit "%i2";  27 -> sLit "%i3";
        28 -> sLit "%i4";  29 -> sLit "%i5";
        30 -> sLit "%i6";  31 -> sLit "%i7";
        32 -> sLit "%f0";  33 -> sLit "%f1";
        34 -> sLit "%f2";  35 -> sLit "%f3";
        36 -> sLit "%f4";  37 -> sLit "%f5";
        38 -> sLit "%f6";  39 -> sLit "%f7";
        40 -> sLit "%f8";  41 -> sLit "%f9";
        42 -> sLit "%f10"; 43 -> sLit "%f11";
        44 -> sLit "%f12"; 45 -> sLit "%f13";
        46 -> sLit "%f14"; 47 -> sLit "%f15";
        48 -> sLit "%f16"; 49 -> sLit "%f17";
        50 -> sLit "%f18"; 51 -> sLit "%f19";
        52 -> sLit "%f20"; 53 -> sLit "%f21";
        54 -> sLit "%f22"; 55 -> sLit "%f23";
        56 -> sLit "%f24"; 57 -> sLit "%f25";
        58 -> sLit "%f26"; 59 -> sLit "%f27";
        60 -> sLit "%f28"; 61 -> sLit "%f29";
        62 -> sLit "%f30"; 63 -> sLit "%f31";
        _  -> sLit "very naughty sparc register" })


-- | Pretty print a format for an instruction suffix.
pprFormat :: Format -> SDoc
pprFormat x
 = ptext
    (case x of
        II8     -> sLit "ub"
        II16    -> sLit "uh"
        II32    -> sLit ""
        II64    -> sLit "d"
        FF32    -> sLit ""
        FF64    -> sLit "d")


-- | Pretty print a format for an instruction suffix.
--      eg LD is 32bit on sparc, but LDD is 64 bit.
pprStFormat :: Format -> SDoc
pprStFormat x
 = ptext
    (case x of
        II8   -> sLit "b"
        II16  -> sLit "h"
        II32  -> sLit ""
        II64  -> sLit "x"
        FF32  -> sLit ""
        FF64  -> sLit "d")



-- | Pretty print a condition code.
pprCond :: Cond -> SDoc
pprCond c
 = ptext
    (case c of
        ALWAYS  -> sLit ""
        NEVER   -> sLit "n"
        GEU     -> sLit "geu"
        LU      -> sLit "lu"
        EQQ     -> sLit "e"
        GTT     -> sLit "g"
        GE      -> sLit "ge"
        GU      -> sLit "gu"
        LTT     -> sLit "l"
        LE      -> sLit "le"
        LEU     -> sLit "leu"
        NE      -> sLit "ne"
        NEG     -> sLit "neg"
        POS     -> sLit "pos"
        VC      -> sLit "vc"
        VS      -> sLit "vs")


-- | Pretty print an address mode.
pprAddr :: Platform -> AddrMode -> SDoc
pprAddr platform am
 = case am of
        AddrRegReg r1 (RegReal (RealRegSingle 0))
         -> pprReg r1

        AddrRegReg r1 r2
         -> hcat [ pprReg r1, char '+', pprReg r2 ]

        AddrRegImm r1 (ImmInt i)
         | i == 0               -> pprReg r1
         | not (fits13Bits i)   -> largeOffsetError i
         | otherwise            -> hcat [ pprReg r1, pp_sign, int i ]
         where
                pp_sign = mwhen (i > 0) (char '+')

        AddrRegImm r1 (ImmInteger i)
         | i == 0               -> pprReg r1
         | not (fits13Bits i)   -> largeOffsetError i
         | otherwise            -> hcat [ pprReg r1, pp_sign, integer i ]
         where
                pp_sign = mwhen (i > 0) (char '+')

        AddrRegImm r1 imm
         -> hcat [ pprReg r1, char '+', pprImm platform imm ]


-- | Pretty print an immediate value.
pprImm :: Platform -> Imm -> SDoc
pprImm platform imm
 = case imm of
        ImmInt i        -> int i
        ImmInteger i    -> integer i
        ImmCLbl l       -> pdoc platform l
        ImmIndex l i    -> pdoc platform l <> char '+' <> int i
        ImmLit s        -> s

        ImmConstantSum a b
         -> pprImm platform a <> char '+' <> pprImm platform b

        ImmConstantDiff a b
         -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen

        LO i
         -> hcat [ text "%lo(", pprImm platform i, rparen ]

        HI i
         -> hcat [ text "%hi(", pprImm platform i, rparen ]

        -- these should have been converted to bytes and placed
        --      in the data section.
        ImmFloat _      -> text "naughty float immediate"
        ImmDouble _     -> text "naughty double immediate"


-- | Pretty print a section \/ segment header.
--      On SPARC all the data sections must be at least 8 byte aligned
--      incase we store doubles in them.
--
pprSectionAlign :: NCGConfig -> Section -> SDoc
pprSectionAlign config sec@(Section seg _) =
    pprSectionHeader config sec $$
    pprAlignForSection seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: SectionType -> SDoc
pprAlignForSection seg =
    ptext (case seg of
      Text              -> sLit ".align 4"
      Data              -> sLit ".align 8"
      ReadOnlyData      -> sLit ".align 8"
      RelocatableReadOnlyData
                        -> sLit ".align 8"
      UninitialisedData -> sLit ".align 8"
      ReadOnlyData16    -> sLit ".align 16"
      -- TODO: This is copied from the ReadOnlyData case, but it can likely be
      -- made more efficient.
      CString           -> sLit ".align 8"
      OtherSection _    -> panic "PprMach.pprSectionHeader: unknown section")

-- | Pretty print a data item.
pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        imm = litToImm lit

        ppr_item II8   _        = [text "\t.byte\t" <> pprImm platform imm]
        ppr_item II32  _        = [text "\t.long\t" <> pprImm platform imm]

        ppr_item FF32  (CmmFloat r _)
         = let bs = floatToBytes (fromRational r)
           in  map (\b -> text "\t.byte\t" <> pprImm platform (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
         = let bs = doubleToBytes (fromRational r)
           in  map (\b -> text "\t.byte\t" <> pprImm platform (ImmInt b)) bs

        ppr_item II16  _        = [text "\t.short\t" <> pprImm platform imm]
        ppr_item II64  _        = [text "\t.quad\t"  <> pprImm platform imm]
        ppr_item _ _            = panic "SPARC.Ppr.pprDataItem: no match"


-- | Pretty print an instruction.
pprInstr :: Platform -> Instr -> SDoc
pprInstr platform = \case
   COMMENT _ -> mempty -- nuke comments.
   DELTA d   -> pprInstr platform (COMMENT (mkFastString ("\tdelta = " ++ show d)))

   -- Newblocks and LData should have been slurped out before producing the .s file.
   NEWBLOCK _ -> panic "X86.Ppr.pprInstr: NEWBLOCK"
   LDATA _ _  -> panic "PprMach.pprInstr: LDATA"

   -- 64 bit FP loads are expanded into individual instructions in CodeGen.Expand
   LD FF64 _ reg
        | RegReal (RealRegSingle{})     <- reg
        -> panic "SPARC.Ppr: not emitting potentially misaligned LD FF64 instr"

   LD format addr reg
        -> hcat [
               text "\tld",
               pprFormat format,
               char '\t',
               lbrack,
               pprAddr platform addr,
               pp_rbracket_comma,
               pprReg reg
            ]

   -- 64 bit FP stores are expanded into individual instructions in CodeGen.Expand
   ST FF64 reg _
        | RegReal (RealRegSingle{}) <- reg
        -> panic "SPARC.Ppr: not emitting potentially misaligned ST FF64 instr"

   -- no distinction is made between signed and unsigned bytes on stores for the
   -- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
   -- so we call a special-purpose pprFormat for ST..
   ST format reg addr
        -> hcat [
               text "\tst",
               pprStFormat format,
               char '\t',
               pprReg reg,
               pp_comma_lbracket,
               pprAddr platform addr,
               rbrack
            ]


   ADD x cc reg1 ri reg2
        | not x && not cc && riZero ri
        -> hcat [ text "\tmov\t", pprReg reg1, comma, pprReg reg2 ]

        | otherwise
        -> pprRegRIReg platform (if x then sLit "addx" else sLit "add") cc reg1 ri reg2


   SUB x cc reg1 ri reg2
        | not x && cc && reg2 == g0
        -> hcat [ text "\tcmp\t", pprReg reg1, comma, pprRI platform ri ]

        | not x && not cc && riZero ri
        -> hcat [ text "\tmov\t", pprReg reg1, comma, pprReg reg2 ]

        | otherwise
        -> pprRegRIReg platform (if x then sLit "subx" else sLit "sub") cc reg1 ri reg2

   AND  b reg1 ri reg2 -> pprRegRIReg platform (sLit "and")  b reg1 ri reg2

   ANDN b reg1 ri reg2 -> pprRegRIReg platform (sLit "andn") b reg1 ri reg2

   OR b reg1 ri reg2
        | not b && reg1 == g0
        -> let doit = hcat [ text "\tmov\t", pprRI platform ri, comma, pprReg reg2 ]
           in  case ri of
                   RIReg rrr | rrr == reg2 -> mempty
                   _                       -> doit

        | otherwise
        -> pprRegRIReg platform (sLit "or") b reg1 ri reg2

   ORN b reg1 ri reg2 -> pprRegRIReg platform (sLit "orn") b reg1 ri reg2

   XOR  b reg1 ri reg2 -> pprRegRIReg platform (sLit "xor")  b reg1 ri reg2
   XNOR b reg1 ri reg2 -> pprRegRIReg platform (sLit "xnor") b reg1 ri reg2

   SLL reg1 ri reg2 -> pprRegRIReg platform (sLit "sll") False reg1 ri reg2
   SRL reg1 ri reg2 -> pprRegRIReg platform (sLit "srl") False reg1 ri reg2
   SRA reg1 ri reg2 -> pprRegRIReg platform (sLit "sra") False reg1 ri reg2

   RDY rd -> text "\trd\t%y," <> pprReg rd
   WRY reg1 reg2
        -> text "\twr\t"
                <> pprReg reg1
                <> char ','
                <> pprReg reg2
                <> char ','
                <> text "%y"

   SMUL b reg1 ri reg2 -> pprRegRIReg platform (sLit "smul")  b reg1 ri reg2
   UMUL b reg1 ri reg2 -> pprRegRIReg platform (sLit "umul")  b reg1 ri reg2
   SDIV b reg1 ri reg2 -> pprRegRIReg platform (sLit "sdiv")  b reg1 ri reg2
   UDIV b reg1 ri reg2 -> pprRegRIReg platform (sLit "udiv")  b reg1 ri reg2

   SETHI imm reg
      -> hcat [
            text "\tsethi\t",
            pprImm platform imm,
            comma,
            pprReg reg
         ]

   NOP -> text "\tnop"

   FABS format reg1 reg2
        -> pprFormatRegReg (sLit "fabs") format reg1 reg2

   FADD format reg1 reg2 reg3
        -> pprFormatRegRegReg (sLit "fadd") format reg1 reg2 reg3

   FCMP e format reg1 reg2
        -> pprFormatRegReg (if e then sLit "fcmpe" else sLit "fcmp")
                           format reg1 reg2

   FDIV format reg1 reg2 reg3
        -> pprFormatRegRegReg (sLit "fdiv") format reg1 reg2 reg3

   FMOV format reg1 reg2
        -> pprFormatRegReg (sLit "fmov") format reg1 reg2

   FMUL format reg1 reg2 reg3
        -> pprFormatRegRegReg (sLit "fmul") format reg1 reg2 reg3

   FNEG format reg1 reg2
        -> pprFormatRegReg (sLit "fneg") format reg1 reg2

   FSQRT format reg1 reg2
        -> pprFormatRegReg (sLit "fsqrt") format reg1 reg2

   FSUB format reg1 reg2 reg3
        -> pprFormatRegRegReg (sLit "fsub") format reg1 reg2 reg3

   FxTOy format1 format2 reg1 reg2
      -> hcat [
            text "\tf",
            ptext
            (case format1 of
                II32  -> sLit "ito"
                FF32  -> sLit "sto"
                FF64  -> sLit "dto"
                _     -> panic "SPARC.Ppr.pprInstr.FxToY: no match"),
            ptext
            (case format2 of
                II32  -> sLit "i\t"
                II64  -> sLit "x\t"
                FF32  -> sLit "s\t"
                FF64  -> sLit "d\t"
                _     -> panic "SPARC.Ppr.pprInstr.FxToY: no match"),
            pprReg reg1, comma, pprReg reg2
         ]


   BI cond b blockid
      -> hcat [
            text "\tb", pprCond cond,
            mwhen b pp_comma_a,
            char '\t',
            pdoc platform (blockLbl blockid)
         ]

   BF cond b blockid
      -> hcat [
            text "\tfb", pprCond cond,
            mwhen b pp_comma_a,
            char '\t',
            pdoc platform (blockLbl blockid)
         ]

   JMP addr -> text "\tjmp\t" <> pprAddr platform addr
   JMP_TBL op _ _ -> pprInstr platform (JMP op)

   CALL (Left imm) n _
      -> hcat [ text "\tcall\t", pprImm platform imm, comma, int n ]

   CALL (Right reg) n _
      -> hcat [ text "\tcall\t", pprReg reg, comma, int n ]


-- | Pretty print a RI
pprRI :: Platform -> RI -> SDoc
pprRI platform = \case
   RIReg r -> pprReg r
   RIImm r -> pprImm platform r


-- | Pretty print a two reg instruction.
pprFormatRegReg :: PtrString -> Format -> Reg -> Reg -> SDoc
pprFormatRegReg name format reg1 reg2
  = hcat [
        char '\t',
        ptext name,
        (case format of
            FF32 -> text "s\t"
            FF64 -> text "d\t"
            _    -> panic "SPARC.Ppr.pprFormatRegReg: no match"),

        pprReg reg1,
        comma,
        pprReg reg2
    ]


-- | Pretty print a three reg instruction.
pprFormatRegRegReg :: PtrString -> Format -> Reg -> Reg -> Reg -> SDoc
pprFormatRegRegReg name format reg1 reg2 reg3
  = hcat [
        char '\t',
        ptext name,
        (case format of
            FF32  -> text "s\t"
            FF64  -> text "d\t"
            _    -> panic "SPARC.Ppr.pprFormatRegReg: no match"),
        pprReg reg1,
        comma,
        pprReg reg2,
        comma,
        pprReg reg3
    ]


-- | Pretty print an instruction of two regs and a ri.
pprRegRIReg :: Platform -> PtrString -> Bool -> Reg -> RI -> Reg -> SDoc
pprRegRIReg platform name b reg1 ri reg2
  = hcat [
        char '\t',
        ptext name,
        if b then text "cc\t" else char '\t',
        pprReg reg1,
        comma,
        pprRI platform ri,
        comma,
        pprReg reg2
    ]

{-
pprRIReg :: PtrString -> Bool -> RI -> Reg -> SDoc
pprRIReg name b ri reg1
  = hcat [
        char '\t',
        ptext name,
        if b then text "cc\t" else char '\t',
        pprRI ri,
        comma,
        pprReg reg1
    ]
-}

{-
pp_ld_lbracket :: SDoc
pp_ld_lbracket    = text "\tld\t["
-}

pp_rbracket_comma :: SDoc
pp_rbracket_comma = text "],"


pp_comma_lbracket :: SDoc
pp_comma_lbracket = text ",["


pp_comma_a :: SDoc
pp_comma_a        = text ",a"
