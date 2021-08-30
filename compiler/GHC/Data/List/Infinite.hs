{-# LANGUAGE PatternSynonyms #-}

module GHC.Data.List.Infinite where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Identity (Identity (..))

type Infinite = Cofree Identity

infixr 5 :.
pattern (:.) :: a -> Infinite a -> Infinite a
pattern (:.) a as = Cofree a (Identity as)
{-# COMPLETE (:.) #-}
