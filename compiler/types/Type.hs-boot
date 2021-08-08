module Type where
import {-# SOURCE #-} TypeRep( Type, Kind )
import Var

isPredTy :: Type -> Bool

typeKind :: Type -> Kind
substKiWith :: [KindVar] -> [Kind] -> Kind -> Kind
eqKind :: Kind -> Kind -> Bool
