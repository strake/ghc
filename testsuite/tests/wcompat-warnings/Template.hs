{-# LANGUAGE KindSignatures #-}

module WCompatWarningsOnOff where

-- -fwarn-star-is-type
b :: (Bool :: *)
b = True
