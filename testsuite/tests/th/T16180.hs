{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String

$(do
   addForeignSource LangAsm $ unlines
      [ ".global \"mydata\""
      , "mydata:"
      , ".ascii \"Hello world\\0\""
      ]
   return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring
