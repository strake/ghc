{-# LANGUAGE AutoDeriveTypeable, GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}

module T10348 where

import GHC.TypeLits
import Data.Typeable

data Foo (n :: Nat) where
  Hey :: KnownNat n => Foo n

deriving instance Show (Foo n)

data T t where
  T :: (Show t, Typeable t) => t -> T t

deriving instance Show (T n)

hey :: KnownNat n => T (Foo n)
hey = T Hey

ho :: T (Foo 42)
ho = T Hey

f1 :: KnownNat a => Proxy a -> TypeRep
f1 = typeRep

g2 :: KnownSymbol a => Proxy a -> TypeRep
g2 = typeRep


