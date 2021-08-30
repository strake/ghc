module GHC.Data.List.NonEmpty (module Data.List.NonEmpty, module GHC.Data.List.NonEmpty) where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..))

infix 5 |:
(|:) :: [a] -> a -> NonEmpty a
[] |: b = pure b
(a:as) |: b = a :| (as <|> [b])
