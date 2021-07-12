{-# LANGUAGE MultiParamTypeClasses,RankNTypes,ExistentialQuantification #-}
module RnFail055 where

f1 :: Float -> Int

type S1 a b c = (a,b)

type S2 a b = forall b. (a,b)

type S3 t = [t]

data T1 a b = T1 [a] [b]

data T2 = T2
data T2' = T2'

data T3 b = T3 (forall a. b -> a)

data T4 a = T4 a

data T5 = T5 !Int

data T6 a = forall b . T6 a

class C1 a b where { m1 :: a -> b }
class C2 a b where { m2 :: a -> b }
class (Ord a, Eq a) => C3 a where { }
