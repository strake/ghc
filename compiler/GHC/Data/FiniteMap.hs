-- Some extra functions to extend Data.Map

module GHC.Data.FiniteMap (
        insertList,
        insertListWith,
        deleteList,
    ) where

import GHC.Prelude

import Data.Map (Map)
import qualified Data.Map as Map

insertList :: Ord key => [(key,elt)] -> Map key elt -> Map key elt
insertList = flip $ foldl' (\m (k, v) -> Map.insert k v m)

insertListWith :: Ord key
               => (elt -> elt -> elt)
               -> [(key,elt)]
               -> Map key elt
               -> Map key elt
insertListWith f = flip $ foldl' (\m (k, v) -> Map.insertWith f k v m)

deleteList :: Ord key => [key] -> Map key elt -> Map key elt
deleteList = flip $ foldl' (flip Map.delete)
