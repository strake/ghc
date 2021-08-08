{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, ImplicitParams #-}
{-# LANGUAGE RankNTypes, TypeInType #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Err
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The "GHC.Err" module defines the code for the wired-in error functions,
-- which have a special type in the compiler (with \"open tyvars\").
--
-- We cannot define these functions in a module where they might be used
-- (e.g., "GHC.Base"), because the magical wired-in type will get confused
-- with what the typechecker figures out.
--
-----------------------------------------------------------------------------

module GHC.Err( absentErr, error, errorWithoutStackTrace, undefined ) where
import GHC.CString ()
import GHC.Types (Char, Levity)
import GHC.Stack.Types
import GHC.Prim
import GHC.Integer ()   -- Make sure Integer is compiled first
                        -- because GHC depends on it in a wired-in way
                        -- so the build system doesn't see the dependency
import {-# SOURCE #-} GHC.Exception( errorCallWithCallStackException )

-- | 'error' stops execution and displays an error message.
error :: forall (v :: Levity). forall (a :: TYPE v).
         (?callStack :: CallStack) => [Char] -> a
error s = raise# (errorCallWithCallStackException s ?callStack)

-- | A variant of 'error' that does not produce a stack trace.
--
-- @since 4.9.0.0
errorWithoutStackTrace :: forall (v :: Levity). forall (a :: TYPE v).
                          [Char] -> a
errorWithoutStackTrace s
  = let ?callStack = freezeCallStack ?callStack
    in error s
{-# NOINLINE errorWithoutStackTrace #-}

-- Note [Errors in base]
-- ~~~~~~~~~~~~~~~~~~~~~
-- As of base-4.9.0.0, `error` produces a stack trace alongside the
-- error message using the Implicit CallStack machinery. This provides
-- a partial stack trace, containing the call-site of each function
-- with a (?callStack :: CallStack) implicit parameter constraint.
--
-- In base, however, the only functions that have such constraints are
-- error and undefined, so the stack traces from partial functions in
-- base will never contain a call-site in user code. Instead we'll
-- usually just get the actual call to error. Base functions already
-- have a good habit of providing detailed error messages, including the
-- name of the offending partial function, so the partial stack-trace
-- does not provide any extra information, just noise. Thus, we export
-- the callstack-aware error, but within base we use the
-- errorWithoutStackTrace variant for more hygienic error messages.


-- | A special case of 'error'.
-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which 'undefined'
-- appears.
undefined :: forall (v :: Levity). forall (a :: TYPE v).
             (?callStack :: CallStack) => a
undefined =  error "Prelude.undefined"

-- | Used for compiler-generated error message;
-- encoding saves bytes of string junk.
absentErr :: a
absentErr = errorWithoutStackTrace "Oops! The program has entered an `absent' argument!\n"
