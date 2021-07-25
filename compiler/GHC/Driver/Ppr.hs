module GHC.Driver.Ppr where

import GHC.Prelude
import {-# SOURCE #-} GHC.Driver.Session (DynFlags, hasNoDebugOutput, initSDocContext, pkgState)
import {-# SOURCE #-} GHC.Unit.State
import GHC.Utils.Outputable
import GHC.Utils.Outputable.Ppr (pprDebugAndThen)
import GHC.Utils.Panic (trace)

-- | Show a SDoc as a String with the default user style
showSDoc :: DynFlags -> SDoc -> String
showSDoc dflags sdoc = renderWithStyle (initSDocContext dflags defaultUserStyle) sdoc

showPpr :: Outputable a => DynFlags -> a -> String
showPpr dflags thing = showSDoc dflags (ppr thing)

-- | If debug output is on, show some 'SDoc' on the screen
pprTraceWithFlags :: DynFlags -> String -> SDoc -> a -> a
pprTraceWithFlags dflags str doc x
  | hasNoDebugOutput dflags = x
  | otherwise               = pprDebugAndThen (initSDocContext dflags defaultDumpStyle)
        trace (text str) doc x

showSDocForUser :: DynFlags -> PrintUnqualified -> SDoc -> String
showSDocForUser dflags unqual = renderWithStyle ctx . pprWithUnitState pkgstate
  where
    pkgstate = pkgState dflags
    style = mkUserStyle unqual AllTheWay
    ctx = initSDocContext dflags style
