module GHC.Utils.List.NonEmpty where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List as List

infixr 5 ++
(++) :: NonEmpty a -> NonEmpty a -> NonEmpty a
(a:|as) ++ bs = a:|as List.++ toList bs
