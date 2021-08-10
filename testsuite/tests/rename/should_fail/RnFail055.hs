{-# LANGUAGE MultiParamTypeClasses,RankNTypes,ExistentialQuantification #-}
module RnFail055 where

import RnFail055_aux

-- Id with different type
f1 :: Int -> Float
f1 = undefined

-- type synonym with different arity
type S1 a b = (a,b)

-- type synonym with different rhs
type S2 a b = forall a. (a,b)

-- type synonym with alpha-renaming (should be ok)
type S3 a = [a]

-- datatype with different fields
data T1 a b = T1 [b] [a]

-- different constructor name
data T2' = T2
data T2 = T2'

-- check alpha equivalence
data T3 a = T3 (forall b. a -> b)

-- different field labels
data T4 a = T4 { field5 :: a }

-- different strict marks
data T5 = T5 Int

-- different existential quantification
data T6 a = forall a . T6 a

-- extra method in the hs-boot
class C1 a b where {}

-- missing method in the hs-boot
class C2 a b where { m2 :: a -> b; m2' :: a -> b }

-- different superclasses
class (Eq a, Ord a) => C3 a where { }
