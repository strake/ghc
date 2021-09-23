module GHC.Data.List.NonEmpty (module Data.List.NonEmpty, module GHC.Data.List.NonEmpty) where

import Control.Applicative
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty (..))

infix 5 |:
(|:) :: [a] -> a -> NonEmpty a
[] |: b = pure b
(a:as) |: b = a :| (as <|> [b])

infixr 5 ++|
(++|) :: Foldable f => NonEmpty a -> f a -> NonEmpty a
(a:|as) ++| bs = a:|(as <|> toList bs)
