{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -w #-}  -- Turn off deprecation for OverlappingInstances
-- | Unsafe as uses overlapping instances
-- Although it isn't defining any so can we mark safe
-- still?
module UnsafeInfered08_A where

g :: Int
g = 1

