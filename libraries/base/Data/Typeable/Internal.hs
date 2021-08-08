{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.Internal
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- The representations of the types TyCon and TypeRep, and the
-- function mkTyCon which is used by derived instances of Typeable to
-- construct a TyCon.
--
-----------------------------------------------------------------------------

module Data.Typeable.Internal (
    Proxy (..),
    TypeRep(..),
    Fingerprint(..),
    typeOf, typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7,
    Typeable1, Typeable2, Typeable3, Typeable4, Typeable5, Typeable6, Typeable7,
    TyCon(..),
    typeRep,
    mkTyCon,
    mkTyCon3,
    mkTyConApp,
    mkAppTy,
    typeRepTyCon,
    Typeable(..),
    mkFunTy,
    splitTyConApp,
    funResultTy,
    typeRepArgs,
    showsTypeRep,
    tyConString,
    listTc, funTc
  ) where

import GHC.Base
import GHC.Word
import GHC.Show
import GHC.Read ( Read )
import Data.Proxy
import GHC.Num
import GHC.Real
-- import GHC.IORef
-- import GHC.IOArray
-- import GHC.MVar
import GHC.ST           ( ST, STret )
import GHC.STRef        ( STRef )
import GHC.Ptr          ( Ptr, FunPtr )
-- import GHC.Stable
import GHC.Arr          ( Array, STArray, Ix )
import GHC.TypeLits ( Nat, Symbol, KnownNat, KnownSymbol, natVal', symbolVal' )
import Data.Type.Coercion
import Data.Type.Equality
import Text.ParserCombinators.ReadP ( ReadP )
import Text.Read.Lex ( Lexeme, Number )
import Text.ParserCombinators.ReadPrec ( ReadPrec )
import GHC.Float ( FFFormat, RealFloat, Floating )
import Data.Bits ( Bits, FiniteBits )
import GHC.Enum ( Bounded, Enum )

import GHC.Fingerprint.Type
import {-# SOURCE #-} GHC.Fingerprint
   -- loop: GHC.Fingerprint -> Foreign.Ptr -> Data.Typeable
   -- Better to break the loop here, because we want non-SOURCE imports
   -- of Data.Typeable as much as possible so we can optimise the derived
   -- instances.

-- | A concrete representation of a (monomorphic) type.  'TypeRep'
-- supports reasonably efficient equality.
data TypeRep = TypeRep {-# UNPACK #-} !Fingerprint TyCon [TypeRep]

-- Compare keys for equality
instance Eq TypeRep where
  (TypeRep k1 _ _) == (TypeRep k2 _ _) = k1 == k2

instance Ord TypeRep where
  (TypeRep k1 _ _) <= (TypeRep k2 _ _) = k1 <= k2

-- | An abstract representation of a type constructor.  'TyCon' objects can
-- be built using 'mkTyCon'.
data TyCon = TyCon {
   tyConHash    :: {-# UNPACK #-} !Fingerprint,
   tyConPackage :: String, -- ^ /Since: 4.5.0.0/
   tyConModule  :: String, -- ^ /Since: 4.5.0.0/
   tyConName    :: String  -- ^ /Since: 4.5.0.0/
 }

instance Eq TyCon where
  (TyCon t1 _ _ _) == (TyCon t2 _ _ _) = t1 == t2

instance Ord TyCon where
  (TyCon k1 _ _ _) <= (TyCon k2 _ _ _) = k1 <= k2

----------------- Construction --------------------

#include "MachDeps.h"

-- mkTyCon is an internal function to make it easier for GHC to
-- generate derived instances.  GHC precomputes the MD5 hash for the
-- TyCon and passes it as two separate 64-bit values to mkTyCon.  The
-- TyCon for a derived Typeable instance will end up being statically
-- allocated.

#if WORD_SIZE_IN_BITS < 64
mkTyCon :: Word64# -> Word64# -> String -> String -> String -> TyCon
#else
mkTyCon :: Word#   -> Word#   -> String -> String -> String -> TyCon
#endif
mkTyCon high# low# pkg modl name
  = TyCon (Fingerprint (W64# high#) (W64# low#)) pkg modl name

-- | Applies a type constructor to a sequence of types
mkTyConApp  :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp tc@(TyCon tc_k _ _ _) []
  = TypeRep tc_k tc [] -- optimisation: all derived Typeable instances
                       -- end up here, and it helps generate smaller
                       -- code for derived Typeable.
mkTyConApp tc@(TyCon tc_k _ _ _) args
  = TypeRep (fingerprintFingerprints (tc_k : arg_ks)) tc args
  where
    arg_ks = [k | TypeRep k _ _ <- args]

-- | A special case of 'mkTyConApp', which applies the function
-- type constructor to a pair of types.
mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy f a = mkTyConApp funTc [f,a]

-- | Splits a type constructor application
splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp (TypeRep _ tc trs) = (tc,trs)

-- | Applies a type to a function type.  Returns: @'Just' u@ if the
-- first argument represents a function of type @t -> u@ and the
-- second argument represents a function of type @t@.  Otherwise,
-- returns 'Nothing'.
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy trFun trArg
  = case splitTyConApp trFun of
      (tc, [t1,t2]) | tc == funTc && t1 == trArg -> Just t2
      _ -> Nothing

-- | Adds a TypeRep argument to a TypeRep.
mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy (TypeRep _ tc trs) arg_tr = mkTyConApp tc (trs ++ [arg_tr])
   -- Notice that we call mkTyConApp to construct the fingerprint from tc and
   -- the arg fingerprints.  Simply combining the current fingerprint with
   -- the new one won't give the same answer, but of course we want to
   -- ensure that a TypeRep of the same shape has the same fingerprint!
   -- See Trac #5962

-- | Builds a 'TyCon' object representing a type constructor.  An
-- implementation of "Data.Typeable" should ensure that the following holds:
--
-- >  A==A' ^ B==B' ^ C==C' ==> mkTyCon A B C == mkTyCon A' B' C'
--

--
mkTyCon3 :: String       -- ^ package name
         -> String       -- ^ module name
         -> String       -- ^ the name of the type constructor
         -> TyCon        -- ^ A unique 'TyCon' object
mkTyCon3 pkg modl name =
  TyCon (fingerprintString (pkg ++ (' ':modl) ++ (' ':name))) pkg modl name

----------------- Observation ---------------------

-- | Observe the type constructor of a type representation
typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (TypeRep _ tc _) = tc

-- | Observe the argument types of a type representation
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (TypeRep _ _ args) = args

-- | Observe string encoding of a type representation
{-# DEPRECATED tyConString "renamed to 'tyConName'; 'tyConModule' and 'tyConPackage' are also available." #-} -- deprecated in 7.4
tyConString :: TyCon   -> String
tyConString = tyConName

-------------------------------------------------------------
--
--      The Typeable class and friends
--
-------------------------------------------------------------

-- | The class 'Typeable' allows a concrete representation of a type to
-- be calculated.
class Typeable a where
  typeRep# :: Proxy# a -> TypeRep

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
--
-- /Since: 4.7.0.0/
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeRep# (proxy# :: Proxy# a)
{-# INLINE typeRep #-}

-- Keeping backwards-compatibility
typeOf :: forall a. Typeable a => a -> TypeRep
typeOf _ = typeRep (Proxy :: Proxy a)

typeOf1 :: forall t (a :: *). Typeable t => t a -> TypeRep
typeOf1 _ = typeRep (Proxy :: Proxy t)

typeOf2 :: forall t (a :: *) (b :: *). Typeable t => t a b -> TypeRep
typeOf2 _ = typeRep (Proxy :: Proxy t)

typeOf3 :: forall t (a :: *) (b :: *) (c :: *). Typeable t
        => t a b c -> TypeRep
typeOf3 _ = typeRep (Proxy :: Proxy t)

typeOf4 :: forall t (a :: *) (b :: *) (c :: *) (d :: *). Typeable t
        => t a b c d -> TypeRep
typeOf4 _ = typeRep (Proxy :: Proxy t)

typeOf5 :: forall t (a :: *) (b :: *) (c :: *) (d :: *) (e :: *). Typeable t
        => t a b c d e -> TypeRep
typeOf5 _ = typeRep (Proxy :: Proxy t)

typeOf6 :: forall t (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *).
                Typeable t => t a b c d e f -> TypeRep
typeOf6 _ = typeRep (Proxy :: Proxy t)

typeOf7 :: forall t (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *)
                (g :: *). Typeable t => t a b c d e f g -> TypeRep
typeOf7 _ = typeRep (Proxy :: Proxy t)

type Typeable1 (a :: * -> *)                               = Typeable a
type Typeable2 (a :: * -> * -> *)                          = Typeable a
type Typeable3 (a :: * -> * -> * -> *)                     = Typeable a
type Typeable4 (a :: * -> * -> * -> * -> *)                = Typeable a
type Typeable5 (a :: * -> * -> * -> * -> * -> *)           = Typeable a
type Typeable6 (a :: * -> * -> * -> * -> * -> * -> *)      = Typeable a
type Typeable7 (a :: * -> * -> * -> * -> * -> * -> * -> *) = Typeable a

{-# DEPRECATED Typeable1 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable2 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable3 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable4 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable5 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable6 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable7 "renamed to 'Typeable'" #-} -- deprecated in 7.8

-- | Kind-polymorphic Typeable instance for type application
instance (Typeable s, Typeable a) => Typeable (s a) where
         -- See Note [The apparent incoherence of Typable]
  typeRep# = \_ -> rep                  -- Note [Memoising typeOf]
    where !ty1 = typeRep# (proxy# :: Proxy# s)
          !ty2 = typeRep# (proxy# :: Proxy# a)
          !rep = ty1 `mkAppTy` ty2

{- Note [Memoising typeOf]
~~~~~~~~~~~~~~~~~~~~~~~~~~
See #3245, #9203

IMPORTANT: we don't want to recalculate the TypeRep once per call with
the proxy argument.  This is what went wrong in #3245 and #9203. So we
help GHC by manually keeping the 'rep' *outside* the lambda.
-}

----------------- Showing TypeReps --------------------

instance Show TypeRep where
  showsPrec p (TypeRep _ tycon tys) =
    case tys of
      [] -> showsPrec p tycon
      [x]   | tycon == listTc -> showChar '[' . shows x . showChar ']'
      [a,r] | tycon == funTc  -> showParen (p > 8) $
                                 showsPrec 9 a .
                                 showString " -> " .
                                 showsPrec 8 r
      xs | isTupleTyCon tycon -> showTuple xs
         | otherwise         ->
            showParen (p > 9) $
            showsPrec p tycon .
            showChar ' '      .
            showArgs (showChar ' ') tys

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = shows

instance Show TyCon where
  showsPrec _ t = showString (tyConName t)

isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ _ _ ('(':',':_)) = True
isTupleTyCon _                         = False

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => ShowS -> [a] -> ShowS
showArgs _   []     = id
showArgs _   [a]    = showsPrec 10 a
showArgs sep (a:as) = showsPrec 10 a . sep . showArgs sep as

showTuple :: [TypeRep] -> ShowS
showTuple args = showChar '('
               . showArgs (showChar ',') args
               . showChar ')'

listTc :: TyCon
listTc = typeRepTyCon (typeOf [()])

funTc :: TyCon
funTc = typeRepTyCon (typeRep (Proxy :: Proxy (->)))

-------------------------------------------------------------
--
--      Instances of the Typeable classes for Prelude types
--
-------------------------------------------------------------

deriving instance Typeable ()
deriving instance Typeable []
deriving instance Typeable Maybe
deriving instance Typeable Ratio
deriving instance Typeable (->)
deriving instance Typeable IO

deriving instance Typeable Array

deriving instance Typeable ST
deriving instance Typeable STret
deriving instance Typeable STRef
deriving instance Typeable STArray

deriving instance Typeable (,)
deriving instance Typeable (,,)
deriving instance Typeable (,,,)
deriving instance Typeable (,,,,)
deriving instance Typeable (,,,,,)
deriving instance Typeable (,,,,,,)

deriving instance Typeable Ptr
deriving instance Typeable FunPtr

-------------------------------------------------------
--
-- Generate Typeable instances for standard datatypes
--
-------------------------------------------------------

deriving instance Typeable Bool
deriving instance Typeable Char
deriving instance Typeable Float
deriving instance Typeable Double
deriving instance Typeable Int
deriving instance Typeable Word
deriving instance Typeable Integer
deriving instance Typeable Ordering

deriving instance Typeable Word8
deriving instance Typeable Word16
deriving instance Typeable Word32
deriving instance Typeable Word64

deriving instance Typeable TyCon
deriving instance Typeable TypeRep
deriving instance Typeable Fingerprint

deriving instance Typeable RealWorld
deriving instance Typeable Proxy
deriving instance Typeable KProxy
deriving instance Typeable (:~:)
deriving instance Typeable Coercion

deriving instance Typeable ReadP
deriving instance Typeable Lexeme
deriving instance Typeable Number
deriving instance Typeable ReadPrec

deriving instance Typeable FFFormat

-------------------------------------------------------
--
-- Generate Typeable instances for standard classes
--
-------------------------------------------------------

deriving instance Typeable (~)
deriving instance Typeable Coercible
deriving instance Typeable TestEquality
deriving instance Typeable TestCoercion

deriving instance Typeable Eq
deriving instance Typeable Ord

deriving instance Typeable Bits
deriving instance Typeable FiniteBits
deriving instance Typeable Num
deriving instance Typeable Real
deriving instance Typeable Integral
deriving instance Typeable Fractional
deriving instance Typeable RealFrac
deriving instance Typeable Floating
deriving instance Typeable RealFloat

deriving instance Typeable Bounded
deriving instance Typeable Enum
deriving instance Typeable Ix

deriving instance Typeable Show
deriving instance Typeable Read

deriving instance Typeable Alternative
deriving instance Typeable Applicative
deriving instance Typeable Functor
deriving instance Typeable Monad
deriving instance Typeable MonadPlus
deriving instance Typeable Monoid

deriving instance Typeable Typeable



--------------------------------------------------------------------------------
-- Instances for type literals

{- Note [Potential Collisions in `Nat` and `Symbol` instances]

Kinds resulting from lifted types have finitely many type-constructors.
This is not the case for `Nat` and `Symbol`, which both contain *infinitely*
many type constructors (e.g., `Nat` has 0, 1, 2, 3, etc.).  One might think
that this would increase the chance of hash-collisions in the type but this
is not the case because the fingerprint stored in a `TypeRep` identifies
the whole *type* and not just the type constructor.  This is why the chance
of collisions for `Nat` and `Symbol` is not any worse than it is for other
lifted types with infinitely many inhabitants.  Indeed, `Nat` is
isomorphic to (lifted) `[()]`  and `Symbol` is isomorphic to `[Char]`.
-}

{- Note [The apparent incoherence of Typable] See Trac #9242
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The reason we have INCOHERENT on Typeable (n:Nat) and Typeable (s:Symbol)
because we also have an instance Typable (f a).  Now suppose we have
  [Wanted] Typeable (a :: Nat)
we should pick the (x::Nat) instance, even though the instance
matching rules would worry that 'a' might later be instantiated to
(f b), for some f and b. But we type theorists know that there are no
type constructors f of kind blah -> Nat, so this can never happen and
it's safe to pick the second instance. -}


instance {-# INCOHERENT #-} KnownNat n => Typeable (n :: Nat) where
  -- See Note [The apparent incoherence of Typable]
  -- See #9203 for an explanation of why this is written as `\_ -> rep`.
  typeRep# = \_ -> rep
    where
    rep = mkTyConApp tc []
    tc = TyCon
           { tyConHash     = fingerprintString (mk pack modu nm)
           , tyConPackage  = pack
           , tyConModule   = modu
           , tyConName     = nm
           }
    pack = "base"
    modu = "GHC.TypeLits"
    nm   = show (natVal' (proxy# :: Proxy# n))
    mk a b c = a ++ " " ++ b ++ " " ++ c


instance {-# INCOHERENT #-} KnownSymbol s => Typeable (s :: Symbol) where
  -- See Note [The apparent incoherence of Typable]
  -- See #9203 for an explanation of why this is written as `\_ -> rep`.
  typeRep# = \_ -> rep
    where
    rep = mkTyConApp tc []
    tc = TyCon
           { tyConHash     = fingerprintString (mk pack modu nm)
           , tyConPackage  = pack
           , tyConModule   = modu
           , tyConName     = nm
           }
    pack = "base"
    modu = "GHC.TypeLits"
    nm   = show (symbolVal' (proxy# :: Proxy# s))
    mk a b c = a ++ " " ++ b ++ " " ++ c

