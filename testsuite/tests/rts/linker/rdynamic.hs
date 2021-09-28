-- | A test to load symbols exposed with @-rdynamic@.
--
-- Exporting 'f' from Main is important, otherwise, the corresponding symbol
-- wouldn't appear in symbol tables.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
module Main(main, f) where

import Foreign.C.String ( withCString, CString )
import GHC.Exts         ( addrToAny# )
import GHC.Ptr          ( Ptr(..), nullPtr )
import System.Info      ( os, arch )
import GHC.Utils.Encoding

main = (loadFunction Nothing "Main" "f" :: IO (Maybe String)) >>= print

f :: String
f = "works"

-- loadFunction__ taken from
-- @plugins-1.5.4.0:System.Plugins.Load.loadFunction__@
loadFunction :: Maybe String
             -> String
             -> String
           -> IO (Maybe a)
loadFunction mpkg m valsym = do
    c_initLinker
    let symbol = maybe "" (\p -> zEncodeString p ++ "_") mpkg
              ++ zEncodeString m ++ "_" ++ zEncodeString valsym
              ++ "_closure"
    ptr@(Ptr addr) <- withCString symbol c_lookupSymbol
    if (ptr == nullPtr)
    then return Nothing
    else case addrToAny# addr of
           (# hval #) -> return ( Just hval )

foreign import ccall safe "lookupSymbol" c_lookupSymbol :: CString -> IO (Ptr a)
foreign import ccall safe "initLinker" c_initLinker :: IO ()
