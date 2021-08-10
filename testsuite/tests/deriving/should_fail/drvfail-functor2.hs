{-# LANGUAGE DeriveFunctor #-}
module ShouldFail where

-- Derive Functor on a type that uses 'a' in the wrong places

newtype InFunctionArgument a = InFunctionArgument (a -> Int)
   deriving (Functor)

newtype OnSecondArg a = OnSecondArg (Either a a)
   deriving (Functor)

-- Derive Functor on a type with no arguments

newtype NoArguments = NoArguments Int
   deriving (Functor)

-- A missing Functor instance

data NoFunctor a = NoFunctor
data UseNoFunctor a = UseNoFunctor (NoFunctor a)
   deriving (Functor)
