{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's call-stack simulation
--
-- @since 4.5.0.0
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedTuples, MagicHash, NoImplicitPrelude #-}
module GHC.Stack (
    -- * Call stacks
    -- ** Simulated by the RTS
    currentCallStack,
    whoCreated,
    errorWithStackTrace,

    -- ** Explicitly created via implicit-parameters
    --
    -- @since 4.8.2.0
    CallStack,
    getCallStack,
    showCallStack,

    -- * Internals
    CostCentreStack,
    CostCentre,
    getCurrentCCS,
    getCCSOf,
    ccsCC,
    ccsParent,
    ccLabel,
    ccModule,
    ccSrcSpan,
    ccsToStrings,
    renderStack
  ) where

import Data.List ( unlines )

import Foreign
import Foreign.C

import GHC.IO
import GHC.Base
import GHC.Ptr
import GHC.Foreign as GHC
import GHC.IO.Encoding
import GHC.Exception
import GHC.List ( concatMap, null, reverse )
import GHC.Show
import GHC.SrcLoc

#define PROFILING
#include "Rts.h"

data CostCentreStack
data CostCentre

getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)
getCurrentCCS dummy = IO $ \s ->
   case getCurrentCCS## dummy s of
     (## s', addr ##) -> (## s', Ptr addr ##)

getCCSOf :: a -> IO (Ptr CostCentreStack)
getCCSOf obj = IO $ \s ->
   case getCCSOf## obj s of
     (## s', addr ##) -> (## s', Ptr addr ##)

ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = (# peek CostCentreStack, cc) p

ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = (# peek CostCentreStack, prevStack) p

ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = (# peek CostCentre, label) p

ccModule :: Ptr CostCentre -> IO CString
ccModule p = (# peek CostCentre, module) p

ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = (# peek CostCentre, srcloc) p

-- | returns a '[String]' representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintined by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-auto@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since 4.5.0.0

currentCallStack :: IO [String]
currentCallStack = ccsToStrings =<< getCurrentCCS ()

ccsToStrings :: Ptr CostCentreStack -> IO [String]
ccsToStrings ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs == nullPtr = return acc
     | otherwise = do
        cc  <- ccsCC ccs
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        mdl <- GHC.peekCString utf8 =<< ccModule cc
        loc <- GHC.peekCString utf8 =<< ccSrcSpan cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)

-- | Get the stack trace attached to an object.
--
-- @since 4.5.0.0
whoCreated :: a -> IO [String]
whoCreated obj = do
  ccs <- getCCSOf obj
  ccsToStrings ccs

renderStack :: [String] -> String
renderStack strs = "Stack trace:" ++ concatMap ("\n  "++) (reverse strs)

-- | Like the function 'error', but appends a stack trace to the error
-- message if one is available.
--
-- @since 4.7.0.0
errorWithStackTrace :: String -> a
errorWithStackTrace x = unsafeDupablePerformIO $ do
   stack <- ccsToStrings =<< getCurrentCCS x
   if null stack
      then throwIO (ErrorCall x)
      else throwIO (ErrorCall (x ++ '\n' : renderStack stack))


----------------------------------------------------------------------
-- Explicit call-stacks built via ImplicitParams
----------------------------------------------------------------------

-- | @CallStack@s are an alternate method of obtaining the call stack at a given
-- point in the program.
--
-- When an implicit-parameter of type @CallStack@ occurs in a program, GHC will
-- solve it with the current location. If another @CallStack@ implicit-parameter
-- is in-scope (e.g. as a function argument), the new location will be appended
-- to the one in-scope, creating an explicit call-stack. For example,
--
-- @
-- myerror :: (?loc :: CallStack) => String -> a
-- myerror msg = error (msg ++ "\n" ++ showCallStack ?loc)
-- @
-- ghci> myerror "die"
-- *** Exception: die
-- ?loc, called at MyError.hs:7:51 in main:MyError
--   myerror, called at <interactive>:2:1 in interactive:Ghci1
--
-- @CallStack@s do not interact with the RTS and do not require compilation with
-- @-prof@. On the other hand, as they are built up explicitly using
-- implicit-parameters, they will generally not contain as much information as
-- the simulated call-stacks maintained by the RTS.
--
-- The @CallStack@ type is abstract, but it can be converted into a
-- @[(String, SrcLoc)]@ via 'getCallStack'. The @String@ is the name of function
-- that was called, the 'SrcLoc' is the call-site. The list is ordered with the
-- most recently called function at the head.
--
-- @since 4.8.2.0
data CallStack = CallStack { getCallStack :: [(String, SrcLoc)] }
  -- See Note [Overview of implicit CallStacks]
  deriving (Show, Eq)

-- | Pretty print 'CallStack'
--
-- @since 4.8.2.0
showCallStack :: CallStack -> String
showCallStack (CallStack (root:rest))
  = unlines (showCallSite root : map (indent . showCallSite) rest)
  where
  indent l = "  " ++ l
  showCallSite (f, loc) = f ++ ", called at " ++ showSrcLoc loc
showCallStack _ = error "CallStack cannot be empty!"
