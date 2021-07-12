-- !!! Checking that empty contexts are permitted.
module ShouldCompile where

f :: () => Int -> Int
f = (+1)


class () => Fob a where

instance () => Fob Int where
instance () => Fob Float

