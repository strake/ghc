{-# LANGUAGE ApplicativeComprehensions #-}
module Main where

import Data.Functor.Identity

f :: Identity () -> Identity [Int] -> Identity Int
f i0 i1 = [x + 42 | _ <- i0, [x] <- i1]

main :: IO ()
main = print $ f (Identity ()) (Identity [])
