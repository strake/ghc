module GHC.Types.Name (
    module GHC.Types.Name,
    module GHC.Types.Name.Occurrence
) where

import GHC.Prelude ()
import {-# SOURCE #-} GHC.Types.Name.Occurrence
import GHC.Types.Unique
import GHC.Utils.Outputable
import Lens.Micro (Lens')

data Name

instance Uniquable Name
instance Outputable Name

class NamedThing a where
    getOccName :: a -> OccName
    getName    :: a -> Name

    getOccName n = nameOccName (getName n)

nameUnique :: Name -> Unique
nameUniqueL :: Lens' Name Unique
nameOccName :: Name -> OccName
