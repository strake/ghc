-- | Printing-related functions
module GHC.Utils.Outputable.Ppr
   ( showSDoc
   , showSDocDebug
   , showSDocDump
   , showPpr
   , showPprDefault
   , pprDebugAndThen
   , printForUser
   , printForC
   -- ** Trace
   , warnPprTrace
   , pprTrace
   , pprTraceM
   , pprTraceDebug
   , pprTraceIt
   , pprSTrace
   , pprTraceException
   )
where

import GHC.Prelude

import GHC.Utils.Constants ( debugIsOn )
import GHC.Utils.Exception
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.GlobalVars
import GHC.Utils.Ppr       ( Mode(..) )

import System.IO ( Handle )
import Control.Monad.IO.Class

-- | Show a SDoc as a String with the default user style
showSDoc :: SDocContext -> SDoc -> String
showSDoc = renderWithStyle

showPpr :: Outputable a => SDocContext -> a -> String
showPpr ctx = showSDoc ctx . ppr

showPprDefault :: Outputable a => a -> String
showPprDefault = renderWithStyle defaultSDocContext . ppr

showSDocDump :: SDocContext -> SDoc -> String
showSDocDump ctx d = renderWithStyle ctx (withPprStyle defaultDumpStyle d)

showSDocDebug :: SDocContext -> SDoc -> String
showSDocDebug ctx = showSDocDump ctx { sdocPprDebug = True }

printForUser :: SDocContext -> Handle -> SDoc -> IO ()
printForUser ctx = printSDocLn ctx PageMode

-- | Like 'printSDocLn' but specialized with 'LeftMode' and
-- @'PprCode' 'CStyle'@.  This is typically used to output C-- code.
printForC :: SDocContext -> Handle -> SDoc -> IO ()
printForC ctx handle = printSDocLn ctx LeftMode handle . withPprStyle (PprCode CStyle)

pprDebugAndThen :: SDocContext -> (String -> a) -> SDoc -> SDoc -> a
pprDebugAndThen ctx cont heading pretty_msg
 = cont (showSDocDump ctx doc)
 where
     doc = sep [heading, nest 2 pretty_msg]

-- | If debug output is on, show some 'SDoc' on the screen
pprTrace :: String -> SDoc -> a -> a
pprTrace str doc x
  | unsafeHasNoDebugOutput = x
  | otherwise              = pprDebugAndThen defaultSDocContext trace (text str) doc x

pprTraceM :: Applicative f => String -> SDoc -> f ()
pprTraceM str doc = pprTrace str doc (pure ())

pprTraceDebug :: String -> SDoc -> a -> a
pprTraceDebug str doc x
   | debugIsOn && unsafeHasPprDebug = pprTrace str doc x
   | otherwise                      = x

-- | @pprTraceWith desc f x@ is equivalent to @pprTrace desc (f x) x@.
-- This allows you to print details from the returned value as well as from
-- ambient variables.
pprTraceWith :: String -> (a -> SDoc) -> a -> a
pprTraceWith desc f x = pprTrace desc (f x) x

-- | @pprTraceIt desc x@ is equivalent to @pprTrace desc (ppr x) x@
pprTraceIt :: Outputable a => String -> a -> a
pprTraceIt desc x = pprTraceWith desc ppr x

-- | @pprTraceException desc x action@ runs action, printing a message
-- if it throws an exception.
pprTraceException :: ExceptionMonad m => String -> SDoc -> m a -> m a
pprTraceException heading doc =
    handleGhcException $ \exc -> liftIO $ do
        putStrLn $ showSDocDump defaultSDocContext (sep [text heading, nest 2 doc])
        throwGhcExceptionIO exc

-- | If debug output is on, show some 'SDoc' on the screen along
-- with a call stack when available.
pprSTrace :: HasCallStack => SDoc -> a -> a
pprSTrace doc = pprTrace "" (doc $$ callStackDoc)

warnPprTrace :: HasCallStack => Bool -> SDoc -> a -> a
-- ^ Just warn about an assertion failure, recording the given file and line number.
-- Should typically be accessed with the WARN macros
warnPprTrace _     _    x | not debugIsOn     = x
warnPprTrace _     _msg x
   | unsafeHasNoDebugOutput = x
warnPprTrace False _msg x = x
warnPprTrace True   msg x
  = pprDebugAndThen defaultSDocContext trace (text "WARNING:")
                    (msg $$ callStackDoc)
                    x
