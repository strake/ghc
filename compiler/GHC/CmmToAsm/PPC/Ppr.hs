
-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.PPC.Ppr
   ( pprNatCmmDecl
   , pprInstr
   )
where

import GHC.Prelude

import GHC.CmmToAsm.PPC.Regs
import GHC.CmmToAsm.PPC.Instr
import GHC.CmmToAsm.PPC.Cond
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Format
import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm hiding (topInfoTable)
import GHC.Data.Collections
import GHC.Cmm.Dataflow.Label

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Ppr.Expr () -- For Outputable instances

import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Platform
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Word
import Data.Int
import Data.Bits

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
         (case platformArch platform of
            ArchPPC_64 ELF_V1 -> pprFunctionDescriptor platform lbl
            ArchPPC_64 ELF_V2 -> pprFunctionPrologue platform lbl
            _ -> pprLabel platform lbl) $$ -- blocks guaranteed not null,
                                           -- so label needed
         vcat (map (pprBasicBlock config top_info) blocks) $$
         (mwhen (ncgDebugLevel config > 0) $
          pdoc platform (mkAsmTempEndLabel lbl) <> char ':') $$
         pprSizeDecl platform lbl

    Just (CmmStaticsRaw info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      (mwhen (platformHasSubsectionsViaSymbols platform) $
       pdoc platform (mkDeadStripPreventer info_lbl) <> char ':') $$
      vcat (map (pprBasicBlock config top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (mwhen (platformHasSubsectionsViaSymbols platform) $
       -- See Note [Subsections Via Symbols] in X86/Ppr.hs
                text "\t.long "
            <+> pdoc platform info_lbl
            <+> char '-'
            <+> pdoc platform (mkDeadStripPreventer info_lbl)) $$
      pprSizeDecl platform info_lbl

-- | Output the ELF .size directive.
pprSizeDecl :: Platform -> CLabel -> SDoc
pprSizeDecl platform lbl
 = mwhen (osElfTarget (platformOS platform)) $
   text "\t.size" <+> prettyLbl <> text ", .-" <> codeLbl
  where
    prettyLbl = pdoc platform lbl
    codeLbl
      | platformArch platform == ArchPPC_64 ELF_V1 = char '.' <> prettyLbl
      | otherwise                                  = prettyLbl

pprFunctionDescriptor :: Platform -> CLabel -> SDoc
pprFunctionDescriptor platform lab = pprGloblDecl platform lab
                        $$  text "\t.section \".opd\", \"aw\""
                        $$  text "\t.align 3"
                        $$  pdoc platform lab <> char ':'
                        $$  text "\t.quad ."
                        <>  pdoc platform lab
                        <>  text ",.TOC.@tocbase,0"
                        $$  text "\t.previous"
                        $$  text "\t.type"
                        <+> pdoc platform lab
                        <>  text ", @function"
                        $$  char '.' <> pdoc platform lab <> char ':'

pprFunctionPrologue :: Platform -> CLabel ->SDoc
pprFunctionPrologue platform lab =  pprGloblDecl platform lab
                        $$  text ".type "
                        <> pdoc platform lab
                        <> text ", @function"
                        $$ pdoc platform lab <> char ':'
                        $$ text "0:\taddis\t" <> pprReg toc
                        <> text ",12,.TOC.-0b@ha"
                        $$ text "\taddi\t" <> pprReg toc
                        <> char ',' <> pprReg toc <> text ",.TOC.-0b@l"
                        $$ text "\t.localentry\t" <> pdoc platform lab
                        <> text ",.-" <> pdoc platform lab

pprBasicBlock :: NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr
              -> SDoc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) instrs) $$
    (mwhen (ncgDebugLevel config > 0) $
     pdoc platform (mkAsmTempEndLabel asmLbl) <> char ':')
  where
    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> mempty
       Just (CmmStaticsRaw info_lbl info) ->
           pprAlignForSection platform Text $$
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
   CmmUninitialised bytes -> text ".space " <> int bytes
   CmmStaticLit lit       -> pprDataItem platform lit

pprGloblDecl :: Platform -> CLabel -> SDoc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = mempty
  | otherwise = text ".globl " <> pdoc platform lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
  = mwhen (platformOS platform == OSLinux && externallyVisibleCLabel lbl) $
    text ".type " <> pdoc platform lbl <> text ", @object"

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeAndSizeDecl platform lbl
   $$ (pdoc platform lbl <> char ':')

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

pprReg :: Reg -> SDoc

pprReg r
  = case r of
      RegReal    (RealRegSingle i) -> ppr_reg_no i
      RegReal    (RealRegPair{})   -> panic "PPC.pprReg: no reg pairs on this arch"
      RegVirtual (VirtualRegI  u)  -> text "%vI_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_"  <> pprUniqueAlways u
      RegVirtual (VirtualRegF  u)  -> text "%vF_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegD  u)  -> text "%vD_"   <> pprUniqueAlways u

  where
    ppr_reg_no :: Int -> SDoc
    ppr_reg_no i
         | i <= 31   = int i      -- GPRs
         | i <= 63   = int (i-32) -- FPRs
         | otherwise = text "very naughty powerpc register"



pprFormat :: Format -> SDoc
pprFormat x
 = ptext (case x of
                II8  -> sLit "b"
                II16 -> sLit "h"
                II32 -> sLit "w"
                II64 -> sLit "d"
                FF32 -> sLit "fs"
                FF64 -> sLit "fd")


pprCond :: Cond -> SDoc
pprCond c
 = ptext (case c of {
                ALWAYS  -> sLit "";
                EQQ     -> sLit "eq";  NE    -> sLit "ne";
                LTT     -> sLit "lt";  GE    -> sLit "ge";
                GTT     -> sLit "gt";  LE    -> sLit "le";
                LU      -> sLit "lt";  GEU   -> sLit "ge";
                GU      -> sLit "gt";  LEU   -> sLit "le"; })


pprImm :: Platform -> Imm -> SDoc
pprImm platform = \case
   ImmInt i       -> int i
   ImmInteger i   -> integer i
   ImmCLbl l      -> pdoc platform l
   ImmIndex l i   -> pdoc platform l <> char '+' <> int i
   ImmLit s       -> s
   ImmFloat f     -> float $ fromRational f
   ImmDouble d    -> double $ fromRational d
   ImmConstantSum a b   -> pprImm platform a <> char '+' <> pprImm platform b
   ImmConstantDiff a b  -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen
   LO (ImmInt i)        -> pprImm platform (LO (ImmInteger (toInteger i)))
   LO (ImmInteger i)    -> pprImm platform (ImmInteger (toInteger lo16))
        where
          lo16 = fromInteger (i .&. 0xffff) :: Int16

   LO i              -> pprImm platform i <> text "@l"
   HI i              -> pprImm platform i <> text "@h"
   HA (ImmInt i)     -> pprImm platform (HA (ImmInteger (toInteger i)))
   HA (ImmInteger i) -> pprImm platform (ImmInteger ha16)
        where
          ha16 = if lo16 >= 0x8000 then hi16+1 else hi16
          hi16 = (i `shiftR` 16)
          lo16 = i .&. 0xffff

   HA i        -> pprImm platform i <> text "@ha"
   HIGHERA i   -> pprImm platform i <> text "@highera"
   HIGHESTA i  -> pprImm platform i <> text "@highesta"


pprAddr :: Platform -> AddrMode -> SDoc
pprAddr platform = \case
   AddrRegReg r1 r2             -> pprReg r1 <> char ',' <+> pprReg r2
   AddrRegImm r1 (ImmInt i)     -> hcat [ int i, char '(', pprReg r1, char ')' ]
   AddrRegImm r1 (ImmInteger i) -> hcat [ integer i, char '(', pprReg r1, char ')' ]
   AddrRegImm r1 imm            -> hcat [ pprImm platform imm, char '(', pprReg r1, char ')' ]


pprSectionAlign :: NCGConfig -> Section -> SDoc
pprSectionAlign config sec@(Section seg _) =
   pprSectionHeader config sec $$
   pprAlignForSection (ncgPlatform config) seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: Platform -> SectionType -> SDoc
pprAlignForSection platform seg =
 let ppc64    = not $ target32Bit platform
 in ptext $ case seg of
       Text              -> sLit ".align 2"
       Data
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       ReadOnlyData
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       RelocatableReadOnlyData
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       UninitialisedData
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       ReadOnlyData16    -> sLit ".align 4"
       -- TODO: This is copied from the ReadOnlyData case, but it can likely be
       -- made more efficient.
       CString
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       OtherSection _    -> panic "PprMach.pprSectionAlign: unknown section"

pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        imm = litToImm lit
        archPPC_64 = not $ target32Bit platform

        ppr_item II8  _ = [text "\t.byte\t"  <> pprImm platform imm]
        ppr_item II16 _ = [text "\t.short\t" <> pprImm platform imm]
        ppr_item II32 _ = [text "\t.long\t"  <> pprImm platform imm]
        ppr_item II64 _
           | archPPC_64 = [text "\t.quad\t"  <> pprImm platform imm]

        ppr_item II64 (CmmInt x _)
           | not archPPC_64 =
                [text "\t.long\t"
                    <> int (fromIntegral
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 text "\t.long\t"
                    <> int (fromIntegral (fromIntegral x :: Word32))]


        ppr_item FF32 _ = [text "\t.float\t" <> pprImm platform imm]
        ppr_item FF64 _ = [text "\t.double\t" <> pprImm platform imm]

        ppr_item _ _
                = panic "PPC.Ppr.pprDataItem: no match"


pprInstr :: Platform -> Instr -> SDoc
pprInstr platform instr = case instr of

   COMMENT _
      -> mempty -- nuke 'em

   -- COMMENT s
   --    -> if platformOS platform == OSLinux
   --          then text "# " <> ftext s
   --          else text "; " <> ftext s

   LOCATION file line col _name
      -> text "\t.loc" <+> ppr file <+> ppr line <+> ppr col

   DELTA d
      -> pprInstr platform (COMMENT (mkFastString ("\tdelta = " ++ show d)))

   NEWBLOCK _
      -> panic "PprMach.pprInstr: NEWBLOCK"

   LDATA _ _
      -> panic "PprMach.pprInstr: LDATA"

{-
   SPILL reg slot
      -> hcat [
              text "\tSPILL",
           char '\t',
           pprReg reg,
           comma,
           text "SLOT" <> parens (int slot)]

   RELOAD slot reg
      -> hcat [
              text "\tRELOAD",
           char '\t',
           text "SLOT" <> parens (int slot),
           comma,
           pprReg reg]
-}

   LD fmt reg addr
      -> hcat [
           char '\t',
           text "l",
           ptext (case fmt of
               II8  -> sLit "bz"
               II16 -> sLit "hz"
               II32 -> sLit "wz"
               II64 -> sLit "d"
               FF32 -> sLit "fs"
               FF64 -> sLit "fd"
               ),
           case addr of AddrRegImm _ _ -> mempty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
       ]

   LDFAR fmt reg (AddrRegImm source off)
      -> vcat
            [ pprInstr platform (ADDIS (tmpReg platform) source (HA off))
            , pprInstr platform (LD fmt reg (AddrRegImm (tmpReg platform) (LO off)))
            ]

   LDFAR _ _ _
      -> panic "PPC.Ppr.pprInstr LDFAR: no match"

   LDR fmt reg1 addr
      -> hcat [
           text "\tl",
           case fmt of
             II32 -> char 'w'
             II64 -> char 'd'
             _    -> panic "PPC.Ppr.Instr LDR: no match",
           text "arx\t",
           pprReg reg1,
           text ", ",
           pprAddr platform addr
           ]

   LA fmt reg addr
      -> hcat [
           char '\t',
           text "l",
           ptext (case fmt of
               II8  -> sLit "ba"
               II16 -> sLit "ha"
               II32 -> sLit "wa"
               II64 -> sLit "d"
               FF32 -> sLit "fs"
               FF64 -> sLit "fd"
               ),
           case addr of AddrRegImm _ _ -> mempty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
           ]

   ST fmt reg addr
      -> hcat [
           char '\t',
           text "st",
           pprFormat fmt,
           case addr of AddrRegImm _ _ -> mempty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
           ]

   STFAR fmt reg (AddrRegImm source off)
      -> vcat [ pprInstr platform (ADDIS (tmpReg platform) source (HA off))
              , pprInstr platform (ST fmt reg (AddrRegImm (tmpReg platform) (LO off)))
              ]

   STFAR _ _ _
      -> panic "PPC.Ppr.pprInstr STFAR: no match"

   STU fmt reg addr
      -> hcat [
           char '\t',
           text "st",
           pprFormat fmt,
           char 'u',
           case addr of AddrRegImm _ _ -> mempty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
           ]

   STC fmt reg1 addr
      -> hcat [
           text "\tst",
           case fmt of
             II32 -> char 'w'
             II64 -> char 'd'
             _    -> panic "PPC.Ppr.Instr STC: no match",
           text "cx.\t",
           pprReg reg1,
           text ", ",
           pprAddr platform addr
           ]

   LIS reg imm
      -> hcat [
           char '\t',
           text "lis",
           char '\t',
           pprReg reg,
           text ", ",
           pprImm platform imm
           ]

   LI reg imm
      -> hcat [
           char '\t',
           text "li",
           char '\t',
           pprReg reg,
           text ", ",
           pprImm platform imm
           ]

   MR reg1 reg2
    | reg1 == reg2 -> mempty
    | otherwise    -> hcat [
        char '\t',
        case targetClassOfReg platform reg1 of
            RcInteger -> text "mr"
            _ -> text "fmr",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
        ]

   CMP fmt reg ri
      -> hcat [
           char '\t',
           op,
           char '\t',
           pprReg reg,
           text ", ",
           pprRI platform ri
           ]
         where
           op = hcat [
                   text "cmp",
                   pprFormat fmt,
                   case ri of
                       RIReg _ -> mempty
                       RIImm _ -> char 'i'
               ]

   CMPL fmt reg ri
      -> hcat [
           char '\t',
           op,
           char '\t',
           pprReg reg,
           text ", ",
           pprRI platform ri
           ]
          where
              op = hcat [
                      text "cmpl",
                      pprFormat fmt,
                      case ri of
                          RIReg _ -> mempty
                          RIImm _ -> char 'i'
                  ]

   BCC cond blockid prediction
      -> hcat [
           char '\t',
           text "b",
           pprCond cond,
           pprPrediction prediction,
           char '\t',
           pdoc platform lbl
           ]
         where lbl = mkLocalBlockLabel (getUnique blockid)
               pprPrediction p = case p of
                 Nothing    -> mempty
                 Just True  -> char '+'
                 Just False -> char '-'

   BCCFAR cond blockid prediction
      -> vcat [
           hcat [
               text "\tb",
               pprCond (condNegate cond),
               neg_prediction,
               text "\t$+8"
           ],
           hcat [
               text "\tb\t",
               pdoc platform lbl
           ]
          ]
          where lbl = mkLocalBlockLabel (getUnique blockid)
                neg_prediction = case prediction of
                  Nothing    -> mempty
                  Just True  -> char '-'
                  Just False -> char '+'

   JMP lbl _
     -- We never jump to ForeignLabels; if we ever do, c.f. handling for "BL"
     | isForeignLabel lbl -> panic "PPC.Ppr.pprInstr: JMP to ForeignLabel"
     | otherwise ->
       hcat [ -- an alias for b that takes a CLabel
           char '\t',
           text "b",
           char '\t',
           pdoc platform lbl
       ]

   MTCTR reg
      -> hcat [
           char '\t',
           text "mtctr",
           char '\t',
           pprReg reg
        ]

   BCTR _ _ _
      -> hcat [
           char '\t',
           text "bctr"
         ]

   BL lbl _
      -> case platformOS platform of
           OSAIX ->
             -- On AIX, "printf" denotes a function-descriptor (for use
             -- by function pointers), whereas the actual entry-code
             -- address is denoted by the dot-prefixed ".printf" label.
             -- Moreover, the PPC NCG only ever emits a BL instruction
             -- for calling C ABI functions. Most of the time these calls
             -- originate from FFI imports and have a 'ForeignLabel',
             -- but when profiling the codegen inserts calls via
             -- 'emitRtsCallGen' which are 'CmmLabel's even though
             -- they'd technically be more like 'ForeignLabel's.
             hcat [
               text "\tbl\t.",
               pdoc platform lbl
             ]
           _ ->
             hcat [
               text "\tbl\t",
               pdoc platform lbl
             ]

   BCTRL _
      -> hcat [
             char '\t',
             text "bctrl"
         ]

   ADD reg1 reg2 ri
      -> pprLogic platform (sLit "add") reg1 reg2 ri

   ADDIS reg1 reg2 imm
      -> hcat [
           char '\t',
           text "addis",
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2,
           text ", ",
           pprImm platform imm
           ]

   ADDO reg1 reg2 reg3
      -> pprLogic platform (sLit "addo") reg1 reg2 (RIReg reg3)

   ADDC reg1 reg2 reg3
      -> pprLogic platform (sLit "addc") reg1 reg2 (RIReg reg3)

   ADDE reg1 reg2 reg3
      -> pprLogic platform (sLit "adde") reg1 reg2 (RIReg reg3)

   ADDZE reg1 reg2
      -> pprUnary (sLit "addze") reg1 reg2

   SUBF reg1 reg2 reg3
      -> pprLogic platform (sLit "subf") reg1 reg2 (RIReg reg3)

   SUBFO reg1 reg2 reg3
      -> pprLogic platform (sLit "subfo") reg1 reg2 (RIReg reg3)

   SUBFC reg1 reg2 ri
      -> hcat [
           char '\t',
           text "subf",
           case ri of
               RIReg _ -> mempty
               RIImm _ -> char 'i',
           text "c\t",
           pprReg reg1,
           text ", ",
           pprReg reg2,
           text ", ",
           pprRI platform ri
           ]

   SUBFE reg1 reg2 reg3
      -> pprLogic platform (sLit "subfe") reg1 reg2 (RIReg reg3)

   MULL fmt reg1 reg2 ri
      -> pprMul platform fmt reg1 reg2 ri

   MULLO fmt reg1 reg2 reg3
      -> hcat [
             char '\t',
             text "mull",
             case fmt of
               II32 -> char 'w'
               II64 -> char 'd'
               _    -> panic "PPC: illegal format",
             text "o\t",
             pprReg reg1,
             text ", ",
             pprReg reg2,
             text ", ",
             pprReg reg3
         ]

   MFOV fmt reg
      -> vcat [
           hcat [
               char '\t',
               text "mfxer",
               char '\t',
               pprReg reg
               ],
           hcat [
               char '\t',
               text "extr",
               case fmt of
                 II32 -> char 'w'
                 II64 -> char 'd'
                 _    -> panic "PPC: illegal format",
               text "i\t",
               pprReg reg,
               text ", ",
               pprReg reg,
               text ", 1, ",
               case fmt of
                 II32 -> text "1"
                 II64 -> text "33"
                 _    -> panic "PPC: illegal format"
               ]
           ]

   MULHU fmt reg1 reg2 reg3
      -> hcat [
            char '\t',
            text "mulh",
            case fmt of
              II32 -> char 'w'
              II64 -> char 'd'
              _    -> panic "PPC: illegal format",
            text "u\t",
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprReg reg3
        ]

   DIV fmt sgn reg1 reg2 reg3
      -> pprDiv fmt sgn reg1 reg2 reg3

        -- for some reason, "andi" doesn't exist.
        -- we'll use "andi." instead.
   AND reg1 reg2 (RIImm imm)
      -> hcat [
            char '\t',
            text "andi.",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm platform imm
        ]

   AND reg1 reg2 ri
      -> pprLogic platform (sLit "and") reg1 reg2 ri

   ANDC reg1 reg2 reg3
      -> pprLogic platform (sLit "andc") reg1 reg2 (RIReg reg3)

   NAND reg1 reg2 reg3
      -> pprLogic platform (sLit "nand") reg1 reg2 (RIReg reg3)

   OR reg1 reg2 ri
      -> pprLogic platform (sLit "or") reg1 reg2 ri

   XOR reg1 reg2 ri
      -> pprLogic platform (sLit "xor") reg1 reg2 ri

   ORIS reg1 reg2 imm
      -> hcat [
            char '\t',
            text "oris",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm platform imm
        ]

   XORIS reg1 reg2 imm
      -> hcat [
            char '\t',
            text "xoris",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm platform imm
        ]

   EXTS fmt reg1 reg2
      -> hcat [
           char '\t',
           text "exts",
           pprFormat fmt,
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2
         ]

   CNTLZ fmt reg1 reg2
      -> hcat [
           char '\t',
           text "cntlz",
           case fmt of
             II32 -> char 'w'
             II64 -> char 'd'
             _    -> panic "PPC: illegal format",
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2
         ]

   NEG reg1 reg2
      -> pprUnary (sLit "neg") reg1 reg2

   NOT reg1 reg2
      -> pprUnary (sLit "not") reg1 reg2

   SR II32 reg1 reg2 (RIImm (ImmInt i))
    -- Handle the case where we are asked to shift a 32 bit register by
    -- less than zero or more than 31 bits. We convert this into a clear
    -- of the destination register.
    -- Fixes ticket https://gitlab.haskell.org/ghc/ghc/issues/5900
      | i < 0  || i > 31 -> pprInstr platform (XOR reg1 reg2 (RIReg reg2))

   SL II32 reg1 reg2 (RIImm (ImmInt i))
    -- As above for SR, but for left shifts.
    -- Fixes ticket https://gitlab.haskell.org/ghc/ghc/issues/10870
      | i < 0  || i > 31 -> pprInstr platform (XOR reg1 reg2 (RIReg reg2))

   SRA II32 reg1 reg2 (RIImm (ImmInt i))
    -- PT: I don't know what to do for negative shift amounts:
    -- For now just panic.
    --
    -- For shift amounts greater than 31 set all bit to the
    -- value of the sign bit, this also what sraw does.
      | i > 31 -> pprInstr platform (SRA II32 reg1 reg2 (RIImm (ImmInt 31)))

   SL fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> "slw"
                       II64 -> "sld"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic platform (sLit op) reg1 reg2 (limitShiftRI fmt ri)

   SR fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> "srw"
                       II64 -> "srd"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic platform (sLit op) reg1 reg2 (limitShiftRI fmt ri)

   SRA fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> "sraw"
                       II64 -> "srad"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic platform (sLit op) reg1 reg2 (limitShiftRI fmt ri)

   RLWINM reg1 reg2 sh mb me
      -> hcat [
             text "\trlwinm\t",
             pprReg reg1,
             text ", ",
             pprReg reg2,
             text ", ",
             int sh,
             text ", ",
             int mb,
             text ", ",
             int me
         ]

   CLRLI fmt reg1 reg2 n
      -> hcat [
            text "\tclrl",
            pprFormat fmt,
            text "i ",
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            int n
        ]

   CLRRI fmt reg1 reg2 n
      -> hcat [
            text "\tclrr",
            pprFormat fmt,
            text "i ",
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            int n
        ]

   FADD fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fadd") fmt reg1 reg2 reg3

   FSUB fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fsub") fmt reg1 reg2 reg3

   FMUL fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fmul") fmt reg1 reg2 reg3

   FDIV fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fdiv") fmt reg1 reg2 reg3

   FABS reg1 reg2
      -> pprUnary (sLit "fabs") reg1 reg2

   FNEG reg1 reg2
      -> pprUnary (sLit "fneg") reg1 reg2

   FCMP reg1 reg2
      -> hcat [
           char '\t',
           text "fcmpu\t0, ",
               -- Note: we're using fcmpu, not fcmpo
               -- The difference is with fcmpo, compare with NaN is an invalid operation.
               -- We don't handle invalid fp ops, so we don't care.
               -- Moreover, we use `fcmpu 0, ...` rather than `fcmpu cr0, ...` for
               -- better portability since some non-GNU assembler (such as
               -- IBM's `as`) tend not to support the symbolic register name cr0.
               -- This matches the syntax that GCC seems to emit for PPC targets.
           pprReg reg1,
           text ", ",
           pprReg reg2
         ]

   FCTIWZ reg1 reg2
      -> pprUnary (sLit "fctiwz") reg1 reg2

   FCTIDZ reg1 reg2
      -> pprUnary (sLit "fctidz") reg1 reg2

   FCFID reg1 reg2
      -> pprUnary (sLit "fcfid") reg1 reg2

   FRSP reg1 reg2
      -> pprUnary (sLit "frsp") reg1 reg2

   CRNOR dst src1 src2
      -> hcat [
           text "\tcrnor\t",
           int dst,
           text ", ",
           int src1,
           text ", ",
           int src2
         ]

   MFCR reg
      -> hcat [
             char '\t',
             text "mfcr",
             char '\t',
             pprReg reg
         ]

   MFLR reg
      -> hcat [
           char '\t',
           text "mflr",
           char '\t',
           pprReg reg
         ]

   FETCHPC reg
      -> vcat [
             text "\tbcl\t20,31,1f",
             hcat [ text "1:\tmflr\t", pprReg reg ]
         ]

   HWSYNC
      -> text "\tsync"

   ISYNC
      -> text "\tisync"

   LWSYNC
      -> text "\tlwsync"

   NOP
      -> text "\tnop"

pprLogic :: Platform -> PtrString -> Reg -> Reg -> RI -> SDoc
pprLogic platform op reg1 reg2 ri = hcat [
        char '\t',
        ptext op,
        case ri of
            RIReg _ -> mempty
            RIImm _ -> char 'i',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprRI platform ri
    ]


pprMul :: Platform -> Format -> Reg -> Reg -> RI -> SDoc
pprMul platform fmt reg1 reg2 ri = hcat [
        char '\t',
        text "mull",
        case ri of
            RIReg _ -> case fmt of
              II32 -> char 'w'
              II64 -> char 'd'
              _    -> panic "PPC: illegal format"
            RIImm _ -> char 'i',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprRI platform ri
    ]


pprDiv :: Format -> Bool -> Reg -> Reg -> Reg -> SDoc
pprDiv fmt sgn reg1 reg2 reg3 = hcat [
        char '\t',
        text "div",
        case fmt of
          II32 -> char 'w'
          II64 -> char 'd'
          _    -> panic "PPC: illegal format",
        sgn `munless` char 'u',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprReg reg3
    ]


pprUnary :: PtrString -> Reg -> Reg -> SDoc
pprUnary op reg1 reg2 = hcat [
        char '\t',
        ptext op,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]


pprBinaryF :: PtrString -> Format -> Reg -> Reg -> Reg -> SDoc
pprBinaryF op fmt reg1 reg2 reg3 = hcat [
        char '\t',
        ptext op,
        pprFFormat fmt,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprReg reg3
    ]

pprRI :: Platform -> RI -> SDoc
pprRI _        (RIReg r) = pprReg r
pprRI platform (RIImm r) = pprImm platform r


pprFFormat :: Format -> SDoc
pprFFormat FF64     = mempty
pprFFormat FF32     = char 's'
pprFFormat _        = panic "PPC.Ppr.pprFFormat: no match"

    -- limit immediate argument for shift instruction to range 0..63
    -- for 64 bit size and 0..32 otherwise
limitShiftRI :: Format -> RI -> RI
limitShiftRI II64 (RIImm (ImmInt i)) | i > 63 || i < 0 =
  panic $ "PPC.Ppr: Shift by " ++ show i ++ " bits is not allowed."
limitShiftRI II32 (RIImm (ImmInt i)) | i > 31 || i < 0 =
  panic $ "PPC.Ppr: 32 bit: Shift by " ++ show i ++ " bits is not allowed."
limitShiftRI _ x = x
