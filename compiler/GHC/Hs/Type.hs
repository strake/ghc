{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow] in module GHC.Hs.Extension
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


GHC.Hs.Type: Abstract syntax: user-defined types
-}

module GHC.Hs.Type (
        HsType(..), NewHsTypeX(..), LHsType, HsKind, LHsKind,
        HsTyVarBndr(..), LHsTyVarBndr, ForallVisFlag(..),
        LHsQTyVars(..),
        HsImplicitBndrs(..),
        HsWildCardBndrs(..),
        HsPatSigType(..), HsPSRn(..),
        LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        HsContext, LHsContext, noLHsContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsArg(..), numVisibleArgs,
        LHsTypeArg, lhsTypeArgSrcSpan,
        OutputableBndrFlag,

        LBangType, BangType,
        HsSrcBang(..), HsImplBang(..),
        SrcStrictness(..), SrcUnpackedness(..),
        getBangType, getBangStrictness,

        ConDeclField(..), LConDeclField, pprConDeclFields,

        HsConDetails(..),

        FieldOcc(..), LFieldOcc, mkFieldOcc,
        AmbiguousFieldOcc(..), mkAmbiguousFieldOcc,
        rdrNameAmbiguousFieldOcc, selectorAmbiguousFieldOcc,
        unambiguousFieldOcc, ambiguousFieldOcc,

        mkAnonWildCardTy, pprAnonWildCard,

        mkHsImplicitBndrs, mkHsWildCardBndrs, mkHsPatSigType, hsImplicitBody,
        mkEmptyImplicitBndrs, mkEmptyWildCardBndrs,
        mkHsQTvs, hsQTvExplicit, emptyLHsQTvs,
        isHsKindedTyVar, hsTvbAllKinded, isLHsForAllTy,
        hsScopedTvs, hsWcScopedTvs, dropWildCards,
        hsTyVarName, hsAllLTyVarNames, hsLTyVarLocNames,
        hsLTyVarName, hsLTyVarNames, hsLTyVarLocName, hsExplicitLTyVarNames,
        splitLHsInstDeclTy, getLHsInstDeclHead, getLHsInstDeclClass_maybe,
        splitLHsPatSynTy,
        splitLHsForAllTyInvis, splitLHsQualTy,
        splitLHsSigmaTyInvis, splitLHsGADTPrefixTy,
        splitHsFunType, hsTyGetAppHead_maybe,
        mkHsOpTy, mkHsAppTy, mkHsAppTys, mkHsAppKindTy,
        ignoreParens, hsSigType, hsSigWcType, hsPatSigType,
        hsTyKindSig,
        hsConDetailsArgs,
        setHsTyVarBndrFlag, hsTyVarBndrFlag,

        -- Printing
        pprHsType, pprHsForAll, pprHsForAllExtra, pprHsExplicitForAll,
        pprLHsContext,
        hsTypeNeedsParens, parenthesizeHsType, parenthesizeHsContext
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Hs.Expr ( HsSplice, pprSplice )

import GHC.Hs.Extension

import GHC.Types.Id ( Id )
import GHC.Types.SourceText
import GHC.Types.Name( Name, NamedThing(getName) )
import GHC.Types.Name.Reader ( RdrName )
import GHC.Core.DataCon( HsSrcBang(..), HsImplBang(..),
                         SrcStrictness(..), SrcUnpackedness(..) )
import GHC.Builtin.Types( mkTupleStr )
import GHC.Core.Type
import GHC.Hs.Doc
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Data.Maybe( isJust )
import GHC.Utils.Misc ( count )

import Data.Data hiding ( Fixity, Prefix, Infix )

{-
************************************************************************
*                                                                      *
\subsection{Bang annotations}
*                                                                      *
************************************************************************
-}

-- | Located Bang Type
type LBangType pass = Located (BangType pass)

-- | Bang Type
--
-- In the parser, strictness and packedness annotations bind more tightly
-- than docstrings. This means that when consuming a 'BangType' (and looking
-- for 'HsBangTy') we must be ready to peer behind a potential layer of
-- 'HsDocTy'. See #15206 for motivation and 'getBangType' for an example.
type BangType pass  = HsType pass       -- Bangs are in the HsType data type

getBangType :: LHsType a -> LHsType a
getBangType                 (L _ (HsBangTy _ _ lty))       = lty
getBangType (L _ (HsDocTy x (L _ (HsBangTy _ _ lty)) lds)) =
  addCLoc lty lds (HsDocTy x lty lds)
getBangType lty                                            = lty

getBangStrictness :: LHsType a -> HsSrcBang
getBangStrictness                 (L _ (HsBangTy _ s _))     = s
getBangStrictness (L _ (HsDocTy _ (L _ (HsBangTy _ s _)) _)) = s
getBangStrictness _ = (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)

{-
************************************************************************
*                                                                      *
\subsection{Data types}
*                                                                      *
************************************************************************

This is the syntax for types as seen in type signatures.

Note [HsBSig binder lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a binder (or pattern) decorated with a type or kind,
   \ (x :: a -> a). blah
   forall (a :: k -> *) (b :: k). blah
Then we use a LHsBndrSig on the binder, so that the
renamer can decorate it with the variables bound
by the pattern ('a' in the first example, 'k' in the second),
assuming that neither of them is in scope already
See also Note [Kind and type-variable binders] in GHC.Rename.HsType

Note [HsType binders]
~~~~~~~~~~~~~~~~~~~~~
The system for recording type and kind-variable binders in HsTypes
is a bit complicated.  Here's how it works.

* In a HsType,
     HsForAllTy   represents an /explicit, user-written/ 'forall'
                   e.g.   forall a b.   {...} or
                          forall a b -> {...}
     HsQualTy     represents an /explicit, user-written/ context
                   e.g.   (Eq a, Show a) => ...
                  The context can be empty if that's what the user wrote
  These constructors represent what the user wrote, no more
  and no less.

* The ForallVisFlag field of HsForAllTy represents whether a forall is
  invisible (e.g., forall a b. {...}, with a dot) or visible
  (e.g., forall a b -> {...}, with an arrow).

* HsTyVarBndr describes a quantified type variable written by the
  user.  For example
     f :: forall a (b :: *).  blah
  here 'a' and '(b::*)' are each a HsTyVarBndr.  A HsForAllTy has
  a list of LHsTyVarBndrs.

* HsImplicitBndrs is a wrapper that gives the implicitly-quantified
  kind and type variables of the wrapped thing.  It is filled in by
  the renamer. For example, if the user writes
     f :: a -> a
  the HsImplicitBinders binds the 'a' (not a HsForAllTy!).
  NB: this implicit quantification is purely lexical: we bind any
      type or kind variables that are not in scope. The type checker
      may subsequently quantify over further kind variables.

* HsWildCardBndrs is a wrapper that binds the wildcard variables
  of the wrapped thing.  It is filled in by the renamer
     f :: _a -> _
  The enclosing HsWildCardBndrs binds the wildcards _a and _.

* HsSigPatType describes types that appear in pattern signatures and
  the signatures of term-level binders in RULES. Like
  HsWildCardBndrs/HsImplicitBndrs, they track the names of wildcard
  variables and implicitly bound type variables. Unlike
  HsImplicitBndrs, however, HsSigPatTypes do not obey the
  forall-or-nothing rule. See Note [Pattern signature binders and scoping].

* The explicit presence of these wrappers specifies, in the HsSyn,
  exactly where implicit quantification is allowed, and where
  wildcards are allowed.

* LHsQTyVars is used in data/class declarations, where the user gives
  explicit *type* variable bindings, but we need to implicitly bind
  *kind* variables.  For example
      class C (a :: k -> *) where ...
  The 'k' is implicitly bound in the hsq_tvs field of LHsQTyVars

Note [The wildcard story for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types can have wildcards in them, to support partial type signatures,
like       f :: Int -> (_ , _a) -> _a

A wildcard in a type can be

  * An anonymous wildcard,
        written '_'
    In HsType this is represented by HsWildCardTy.
    The renamer leaves it untouched, and it is later given fresh meta tyvars in
    the typechecker.

  * A named wildcard,
        written '_a', '_foo', etc
    In HsType this is represented by (HsTyVar "_a")
    i.e. a perfectly ordinary type variable that happens
         to start with an underscore

Note carefully:

* When NamedWildCards is off, type variables that start with an
  underscore really /are/ ordinary type variables.  And indeed, even
  when NamedWildCards is on you can bind _a explicitly as an ordinary
  type variable:
        data T _a _b = MkT _b _a
  Or even:
        f :: forall _a. _a -> _b
  Here _a is an ordinary forall'd binder, but (With NamedWildCards)
  _b is a named wildcard.  (See the comments in #10982)

* Named wildcards are bound by the HsWildCardBndrs (for types that obey the
  forall-or-nothing rule) and HsPatSigType (for type signatures in patterns
  and term-level binders in RULES), which wrap types that are allowed to have
  wildcards. Unnamed wildcards, however are left unchanged until typechecking,
  where we give them fresh wild tyvars and determine whether or not to emit
  hole constraints on each wildcard (we don't if it's a visible type/kind
  argument or a type family pattern). See related notes
  Note [Wildcards in visible kind application] and
  Note [Wildcards in visible type application] in GHC.Tc.Gen.HsType.

* After type checking is done, we report what types the wildcards
  got unified with.

Note [Ordering of implicit variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the advent of -XTypeApplications, GHC makes promises about the ordering
of implicit variable quantification. Specifically, we offer that implicitly
quantified variables (such as those in const :: a -> b -> a, without a `forall`)
will occur in left-to-right order of first occurrence. Here are a few examples:

  const :: a -> b -> a       -- forall a b. ...
  f :: Eq a => b -> a -> a   -- forall a b. ...  contexts are included

  type a <-< b = b -> a
  g :: a <-< b               -- forall a b. ...  type synonyms matter

  class Functor f where
    fmap :: (a -> b) -> f a -> f b   -- forall f a b. ...
    -- The f is quantified by the class, so only a and b are considered in fmap

This simple story is complicated by the possibility of dependency: all variables
must come after any variables mentioned in their kinds.

  typeRep :: Typeable a => TypeRep (a :: k)   -- forall k a. ...

The k comes first because a depends on k, even though the k appears later than
the a in the code. Thus, GHC does a *stable topological sort* on the variables.
By "stable", we mean that any two variables who do not depend on each other
preserve their existing left-to-right ordering.

Implicitly bound variables are collected by the extract- family of functions
(extractHsTysRdrTyVars, extractHsTyVarBndrsKVs, etc.) in GHC.Rename.HsType.
These functions thus promise to keep left-to-right ordering.
Look for pointers to this note to see the places where the action happens.

Note that we also maintain this ordering in kind signatures. Even though
there's no visible kind application (yet), having implicit variables be
quantified in left-to-right order in kind signatures is nice since:

* It's consistent with the treatment for type signatures.
* It can affect how types are displayed with -fprint-explicit-kinds (see
  #15568 for an example), which is a situation where knowing the order in
  which implicit variables are quantified can be useful.
* In the event that visible kind application is implemented, the order in
  which we would expect implicit variables to be ordered in kinds will have
  already been established.
-}

-- | Located Haskell Context
type LHsContext pass = Located (HsContext pass)
      -- ^ 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnUnit'
      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

noLHsContext :: LHsContext pass
-- Use this when there is no context in the original program
-- It would really be more kosher to use a Maybe, to distinguish
--     class () => C a where ...
-- from
--     class C a where ...
noLHsContext = noLoc []

-- | Haskell Context
type HsContext pass = [LHsType pass]

-- | Located Haskell Type
type LHsType pass = Located (HsType pass)
      -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

-- | Haskell Kind
type HsKind pass = HsType pass

-- | Located Haskell Kind
type LHsKind pass = Located (HsKind pass)
      -- ^ 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

--------------------------------------------------
--             LHsQTyVars
--  The explicitly-quantified binders in a data/type declaration

-- | Located Haskell Type Variable Binder
type LHsTyVarBndr flag pass = Located (HsTyVarBndr flag pass)
                         -- See Note [HsType binders]

-- | Located Haskell Quantified Type Variables
data LHsQTyVars pass   -- See Note [HsType binders]
  = HsQTvs { hsq_ext :: XHsQTvs pass

           , hsq_explicit :: [LHsTyVarBndr () pass]
                -- Explicit variables, written by the user
    }
  | XLHsQTyVars !(XXLHsQTyVars pass)

type HsQTvsRn = [Name]  -- Implicit variables
  -- For example, in   data T (a :: k1 -> k2) = ...
  -- the 'a' is explicit while 'k1', 'k2' are implicit

type instance XHsQTvs GhcPs = NoExtField
type instance XHsQTvs GhcRn = HsQTvsRn
type instance XHsQTvs GhcTc = HsQTvsRn

type instance XXLHsQTyVars  (GhcPass _) = NoExtCon

mkHsQTvs :: [LHsTyVarBndr () GhcPs] -> LHsQTyVars GhcPs
mkHsQTvs tvs = HsQTvs { hsq_ext = noExtField, hsq_explicit = tvs }

hsQTvExplicit :: LHsQTyVars pass -> [LHsTyVarBndr () pass]
hsQTvExplicit = hsq_explicit

emptyLHsQTvs :: LHsQTyVars GhcRn
emptyLHsQTvs = HsQTvs { hsq_ext = [], hsq_explicit = [] }

------------------------------------------------
--            HsImplicitBndrs
-- Used to quantify the implicit binders of a type
--    * Implicit binders of a type signature (LHsSigType/LHsSigWcType)
--    * Patterns in a type/data family instance (HsTyPats)

-- | Haskell Implicit Binders
data HsImplicitBndrs pass thing   -- See Note [HsType binders]
  = HsIB { hsib_ext  :: XHsIB pass thing -- after renamer: [Name]
                                         -- Implicitly-bound kind & type vars
                                         -- Order is important; see
                                         -- Note [Ordering of implicit variables]
                                         -- in GHC.Rename.HsType

         , hsib_body :: thing            -- Main payload (type or list of types)
    }
  | XHsImplicitBndrs !(XXHsImplicitBndrs pass thing)

type instance XHsIB              GhcPs _ = NoExtField
type instance XHsIB              GhcRn _ = [Name]
type instance XHsIB              GhcTc _ = [Name]

type instance XXHsImplicitBndrs  (GhcPass _) _ = NoExtCon

-- | Haskell Wildcard Binders
data HsWildCardBndrs pass thing
    -- See Note [HsType binders]
    -- See Note [The wildcard story for types]
  = HsWC { hswc_ext :: XHsWC pass thing
                -- after the renamer
                -- Wild cards, only named
                -- See Note [Wildcards in visible kind application]

         , hswc_body :: thing
                -- Main payload (type or list of types)
                -- If there is an extra-constraints wildcard,
                -- it's still there in the hsc_body.
    }
  | XHsWildCardBndrs !(XXHsWildCardBndrs pass thing)

type instance XHsWC              GhcPs b = NoExtField
type instance XHsWC              GhcRn b = [Name]
type instance XHsWC              GhcTc b = [Name]

type instance XXHsWildCardBndrs  (GhcPass _) b = NoExtCon

-- | Types that can appear in pattern signatures, as well as the signatures for
-- term-level binders in RULES.
-- See @Note [Pattern signature binders and scoping]@.
--
-- This is very similar to 'HsSigWcType', but with
-- slightly different semantics: see @Note [HsType binders]@.
-- See also @Note [The wildcard story for types]@.
data HsPatSigType pass
  = HsPS { hsps_ext  :: XHsPS pass   -- ^ After renamer: 'HsPSRn'
         , hsps_body :: LHsType pass -- ^ Main payload (the type itself)
    }
  | XHsPatSigType !(XXHsPatSigType pass)

-- | The extension field for 'HsPatSigType', which is only used in the
-- renamer onwards. See @Note [Pattern signature binders and scoping]@.
data HsPSRn = HsPSRn
  { hsps_nwcs    :: [Name] -- ^ Wildcard names
  , hsps_imp_tvs :: [Name] -- ^ Implicitly bound variable names
  }
  deriving Data

type instance XHsPS GhcPs = NoExtField
type instance XHsPS GhcRn = HsPSRn
type instance XHsPS GhcTc = HsPSRn

type instance XXHsPatSigType (GhcPass _) = NoExtCon

-- | Located Haskell Signature Type
type LHsSigType   pass = HsImplicitBndrs pass (LHsType pass)    -- Implicit only

-- | Located Haskell Wildcard Type
type LHsWcType    pass = HsWildCardBndrs pass (LHsType pass)    -- Wildcard only

-- | Located Haskell Signature Wildcard Type
type LHsSigWcType pass = HsWildCardBndrs pass (LHsSigType pass) -- Both

-- See Note [Representing type signatures]

hsImplicitBody :: HsImplicitBndrs (GhcPass p) thing -> thing
hsImplicitBody (HsIB { hsib_body = body }) = body

hsSigType :: LHsSigType (GhcPass p) -> LHsType (GhcPass p)
hsSigType = hsImplicitBody

hsSigWcType :: LHsSigWcType pass -> LHsType pass
hsSigWcType sig_ty = hsib_body (hswc_body sig_ty)

hsPatSigType :: HsPatSigType pass -> LHsType pass
hsPatSigType = hsps_body

dropWildCards :: LHsSigWcType pass -> LHsSigType pass
-- Drop the wildcard part of a LHsSigWcType
dropWildCards sig_ty = hswc_body sig_ty

{- Note [Representing type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsSigType is used to represent an explicit user type signature
such as   f :: a -> a
     or   g (x :: a -> a) = x

A HsSigType is just a HsImplicitBndrs wrapping a LHsType.
 * The HsImplicitBndrs binds the /implicitly/ quantified tyvars
 * The LHsType binds the /explicitly/ quantified tyvars

E.g. For a signature like
   f :: forall (a::k). blah
we get
   HsIB { hsib_vars = [k]
        , hsib_body = HsForAllTy { hst_bndrs = [(a::*)]
                                 , hst_body = blah }
The implicit kind variable 'k' is bound by the HsIB;
the explicitly forall'd tyvar 'a' is bound by the HsForAllTy

Note [Pattern signature binders and scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the pattern signatures like those on `t` and `g` in:

   f = let h = \(t :: (b, b) ->
               \(g :: forall a. a -> b) ->
               ...(t :: (Int,Int))...
       in woggle

* The `b` in t's pattern signature is implicitly bound and scopes over
  the signature and the body of the lambda.  It stands for a type (any type);
  indeed we subsequently discover that b=Int.
  (See Note [TyVarTv] in GHC.Tc.Utils.TcMType for more on this point.)
* The `b` in g's pattern signature is an /occurrence/ of the `b` bound by
  t's pattern signature.
* The `a` in `forall a` scopes only over the type `a -> b`, not over the body
  of the lambda.
* There is no forall-or-nothing rule for pattern signatures, which is why the
  type `forall a. a -> b` is permitted in `g`'s pattern signature, even though
  `b` is not explicitly bound.
  See Note [forall-or-nothing rule] in GHC.Rename.HsType.

Similar scoping rules apply to term variable binders in RULES, like in the
following example:

   {-# RULES "h" forall (t :: (b, b)) (g :: forall a. a -> b). h t g = ... #-}

Just like in pattern signatures, the `b` in t's signature is implicitly bound
and scopes over the remainder of the RULE. As a result, the `b` in g's
signature is an occurrence. Moreover, the `a` in `forall a` scopes only over
the type `a -> b`, and the forall-or-nothing rule does not apply.

While quite similar, RULE term binder signatures behave slightly differently
from pattern signatures in two ways:

1. Unlike in pattern signatures, where type variables can stand for any type,
   type variables in RULE term binder signatures are skolems.
   See Note [Typechecking pattern signature binders] in GHC.Tc.Gen.HsType for
   more on this point.

   In this sense, type variables in pattern signatures are quite similar to
   named wildcards, as both can refer to arbitrary types. The main difference
   lies in error reporting: if a named wildcard `_a` in a pattern signature
   stands for Int, then by default GHC will emit a warning stating as much.
   Changing `_a` to `a`, on the other hand, will cause it not to be reported.
2. In the `h` RULE above, only term variables are explicitly bound, so any free
   type variables in the term variables' signatures are implicitly bound.
   This is just like how the free type variables in pattern signatures are
   implicitly bound. If a RULE explicitly binds both term and type variables,
   however, then free type variables in term signatures are /not/ implicitly
   bound. For example, this RULE would be ill scoped:

     {-# RULES "h2" forall b. forall (t :: (b, c)) (g :: forall a. a -> b).
                    h2 t g = ... #-}

   This is because `b` and `c` occur free in the signature for `t`, but only
   `b` was explicitly bound, leaving `c` out of scope. If the RULE had started
   with `forall b c.`, then it would have been accepted.

The types in pattern signatures and RULE term binder signatures are represented
in the AST by HsSigPatType. From the renamer onward, the hsps_ext field (of
type HsPSRn) tracks the names of named wildcards and implicitly bound type
variables so that they can be brought into scope during renaming and
typechecking.
-}

mkHsImplicitBndrs :: thing -> HsImplicitBndrs GhcPs thing
mkHsImplicitBndrs x = HsIB { hsib_ext  = noExtField
                           , hsib_body = x }

mkHsWildCardBndrs :: thing -> HsWildCardBndrs GhcPs thing
mkHsWildCardBndrs x = HsWC { hswc_body = x
                           , hswc_ext  = noExtField }

mkHsPatSigType :: LHsType GhcPs -> HsPatSigType GhcPs
mkHsPatSigType x = HsPS { hsps_ext  = noExtField
                        , hsps_body = x }

-- Add empty binders.  This is a bit suspicious; what if
-- the wrapped thing had free type variables?
mkEmptyImplicitBndrs :: thing -> HsImplicitBndrs GhcRn thing
mkEmptyImplicitBndrs x = HsIB { hsib_ext = []
                              , hsib_body = x }

mkEmptyWildCardBndrs :: thing -> HsWildCardBndrs GhcRn thing
mkEmptyWildCardBndrs x = HsWC { hswc_body = x
                              , hswc_ext  = [] }


--------------------------------------------------
-- | These names are used early on to store the names of implicit
-- parameters.  They completely disappear after type-checking.
newtype HsIPName = HsIPName FastString
  deriving( Eq, Data )

hsIPNameFS :: HsIPName -> FastString
hsIPNameFS (HsIPName n) = n

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _   = ppr         -- Simple for now
    pprInfixOcc  = ppr
    pprPrefixOcc = ppr

--------------------------------------------------

-- | Haskell Type Variable Binder
-- The flag annotates the binder. It is 'Specificity' in places where
-- explicit specificity is allowed (e.g. x :: forall {a} b. ...) or
-- '()' in other places.
data HsTyVarBndr flag pass
  = UserTyVar        -- no explicit kinding
         (XUserTyVar pass)
         flag
         (Located (IdP pass))
        -- See Note [Located RdrNames] in GHC.Hs.Expr
  | KindedTyVar
         (XKindedTyVar pass)
         flag
         (Located (IdP pass))
         (LHsKind pass)  -- The user-supplied kind signature
        -- ^
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --          'GHC.Parser.Annotation.AnnDcolon', 'GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | XTyVarBndr
      !(XXTyVarBndr pass)

type instance XUserTyVar    (GhcPass _) = NoExtField
type instance XKindedTyVar  (GhcPass _) = NoExtField

type instance XXTyVarBndr   (GhcPass _) = NoExtCon

-- | Return the attached flag
hsTyVarBndrFlag :: HsTyVarBndr flag (GhcPass pass) -> flag
hsTyVarBndrFlag (UserTyVar _ fl _)     = fl
hsTyVarBndrFlag (KindedTyVar _ fl _ _) = fl

-- | Set the attached flag
setHsTyVarBndrFlag :: flag -> HsTyVarBndr flag' (GhcPass pass)
  -> HsTyVarBndr flag (GhcPass pass)
setHsTyVarBndrFlag f (UserTyVar x _ l)     = UserTyVar x f l
setHsTyVarBndrFlag f (KindedTyVar x _ l k) = KindedTyVar x f l k

-- | Does this 'HsTyVarBndr' come with an explicit kind annotation?
isHsKindedTyVar :: HsTyVarBndr flag pass -> Bool
isHsKindedTyVar (UserTyVar {})   = False
isHsKindedTyVar (KindedTyVar {}) = True
isHsKindedTyVar (XTyVarBndr {})  = False

-- | Do all type variables in this 'LHsQTyVars' come with kind annotations?
hsTvbAllKinded :: LHsQTyVars pass -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvExplicit

instance NamedThing (HsTyVarBndr flag GhcRn) where
  getName (UserTyVar _ _ v) = unLoc v
  getName (KindedTyVar _ _ v _) = unLoc v

{- Note [Specificity in HsForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All type variables in a `HsForAllTy` type are annotated with their
`Specificity`. The meaning of this `Specificity` depends on the visibility of
the binder `hst_fvf`:

* In an invisible forall type, the `Specificity` denotes whether type variables
  are `Specified` (`forall a. ...`) or `Inferred` (`forall {a}. ...`). For more
  information, see Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility]
  in GHC.Core.TyCo.Rep.

* In a visible forall type, the `Specificity` has no particular meaning. We
  uphold the convention that all visible forall types use `Specified` binders.
-}

-- | Haskell Type
data HsType pass
  = HsForAllTy   -- See Note [HsType binders]
      { hst_xforall :: XForAllTy pass
      , hst_fvf     :: ForallVisFlag -- Is this `forall a -> {...}` or
                                     --         `forall a. {...}`?
      , hst_bndrs   :: [LHsTyVarBndr Specificity pass]
                                     -- Explicit, user-supplied 'forall a {b} c'
                                     -- see Note [Specificity in HsForAllTy]
      , hst_body    :: LHsType pass  -- body type
      }
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnForall',
      --         'GHC.Parser.Annotation.AnnDot','GHC.Parser.Annotation.AnnDarrow'
      -- For details on above see note [Api annotations] in "GHC.Parser.Annotation"

  | HsQualTy   -- See Note [HsType binders]
      { hst_xqual :: XQualTy pass
      , hst_ctxt  :: LHsContext pass       -- Context C => blah
      , hst_body  :: LHsType pass }

  | HsTyVar  (XTyVar pass)
              PromotionFlag    -- Whether explicitly promoted,
                               -- for the pretty printer
             (Located (IdP pass))
                  -- Type variable, type constructor, or data constructor
                  -- see Note [Promotions (HsTyVar)]
                  -- See Note [Located RdrNames] in GHC.Hs.Expr
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsAppTy             (XAppTy pass)
                        (LHsType pass)
                        (LHsType pass)
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsAppKindTy         (XAppKindTy pass) -- type level type app
                        (LHsType pass)
                        (LHsKind pass)

  | HsFunTy             (XFunTy pass)
                        (LHsType pass)   -- function type
                        (LHsType pass)
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnRarrow',

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsListTy            (XListTy pass)
                        (LHsType pass)  -- Element type
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'['@,
      --         'GHC.Parser.Annotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsTupleTy           (XTupleTy pass)
                        HsTupleSort
                        [LHsType pass]  -- Element types (length gives arity)
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'(' or '(#'@,
    --         'GHC.Parser.Annotation.AnnClose' @')' or '#)'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsSumTy             (XSumTy pass)
                        [LHsType pass]  -- Element types (length gives arity)
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'(#'@,
    --         'GHC.Parser.Annotation.AnnClose' '#)'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsOpTy              (XOpTy pass)
                        (LHsType pass) (Located (IdP pass)) (LHsType pass)
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsParTy             (XParTy pass)
                        (LHsType pass)   -- See Note [Parens in HsSyn] in GHC.Hs.Expr
        -- Parenthesis preserved for the precedence re-arrangement in
        -- GHC.Rename.HsType
        -- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
      --         'GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsIParamTy          (XIParamTy pass)
                        (Located HsIPName) -- (?x :: ty)
                        (LHsType pass)   -- Implicit parameters as they occur in
                                         -- contexts
      -- ^
      -- > (?x :: ty)
      --
      -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsStarTy            (XStarTy pass)
                        Bool             -- Is this the Unicode variant?
                                         -- Note [HsStarTy]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

  | HsKindSig           (XKindSig pass)
                        (LHsType pass)  -- (ty :: kind)
                        (LHsKind pass)  -- A type with a kind signature
      -- ^
      -- > (ty :: kind)
      --
      -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
      --         'GHC.Parser.Annotation.AnnDcolon','GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsSpliceTy          (XSpliceTy pass)
                        (HsSplice pass)   -- Includes quasi-quotes
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'$('@,
      --         'GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsDocTy             (XDocTy pass)
                        (LHsType pass) LHsDocString -- A documented type
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsBangTy    (XBangTy pass)
                HsSrcBang (LHsType pass)   -- Bang-style type annotations
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' :
      --         'GHC.Parser.Annotation.AnnOpen' @'{-\# UNPACK' or '{-\# NOUNPACK'@,
      --         'GHC.Parser.Annotation.AnnClose' @'#-}'@
      --         'GHC.Parser.Annotation.AnnBang' @\'!\'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsRecTy     (XRecTy pass)
                [LConDeclField pass]    -- Only in data type declarations
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'{'@,
      --         'GHC.Parser.Annotation.AnnClose' @'}'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  -- | HsCoreTy (XCoreTy pass) Type -- An escape hatch for tunnelling a *closed*
  --                                -- Core Type through HsSyn.
  --     -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsExplicitListTy       -- A promoted explicit list
        (XExplicitListTy pass)
        PromotionFlag      -- whether explicitly promoted, for pretty printer
        [LHsType pass]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @"'["@,
      --         'GHC.Parser.Annotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsExplicitTupleTy      -- A promoted explicit tuple
        (XExplicitTupleTy pass)
        [LHsType pass]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @"'("@,
      --         'GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsTyLit (XTyLit pass) HsTyLit      -- A promoted numeric literal.
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsWildCardTy (XWildCardTy pass)  -- A type wildcard
      -- See Note [The wildcard story for types]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  -- For adding new constructors via Trees that Grow
  | XHsType
      (XXType pass)

data NewHsTypeX
  = NHsCoreTy Type -- An escape hatch for tunnelling a *closed*
                   -- Core Type through HsSyn.
    deriving Data
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

instance Outputable NewHsTypeX where
  ppr (NHsCoreTy ty) = ppr ty

type instance XForAllTy        (GhcPass _) = NoExtField
type instance XQualTy          (GhcPass _) = NoExtField
type instance XTyVar           (GhcPass _) = NoExtField
type instance XAppTy           (GhcPass _) = NoExtField
type instance XFunTy           (GhcPass _) = NoExtField
type instance XListTy          (GhcPass _) = NoExtField
type instance XTupleTy         (GhcPass _) = NoExtField
type instance XSumTy           (GhcPass _) = NoExtField
type instance XOpTy            (GhcPass _) = NoExtField
type instance XParTy           (GhcPass _) = NoExtField
type instance XIParamTy        (GhcPass _) = NoExtField
type instance XStarTy          (GhcPass _) = NoExtField
type instance XKindSig         (GhcPass _) = NoExtField

type instance XAppKindTy       (GhcPass _) = SrcSpan -- Where the `@` lives

type instance XSpliceTy        GhcPs = NoExtField
type instance XSpliceTy        GhcRn = NoExtField
type instance XSpliceTy        GhcTc = Kind

type instance XDocTy           (GhcPass _) = NoExtField
type instance XBangTy          (GhcPass _) = NoExtField
type instance XRecTy           (GhcPass _) = NoExtField

type instance XExplicitListTy  GhcPs = NoExtField
type instance XExplicitListTy  GhcRn = NoExtField
type instance XExplicitListTy  GhcTc = Kind

type instance XExplicitTupleTy GhcPs = NoExtField
type instance XExplicitTupleTy GhcRn = NoExtField
type instance XExplicitTupleTy GhcTc = [Kind]

type instance XTyLit           (GhcPass _) = NoExtField

type instance XWildCardTy      (GhcPass _) = NoExtField

type instance XXType         (GhcPass _) = NewHsTypeX


-- Note [Literal source text] in GHC.Types.Basic for SourceText fields in
-- the following
-- | Haskell Type Literal
data HsTyLit
  = HsNumTy SourceText Integer
  | HsStrTy SourceText FastString
    deriving Data


{-
Note [Unit tuples]
~~~~~~~~~~~~~~~~~~
Consider the type
    type instance F Int = ()
We want to parse that "()"
    as HsTupleTy HsBoxedOrConstraintTuple [],
NOT as HsTyVar unitTyCon

Why? Because F might have kind (* -> Constraint), so we when parsing we
don't know if that tuple is going to be a constraint tuple or an ordinary
unit tuple.  The HsTupleSort flag is specifically designed to deal with
that, but it has to work for unit tuples too.

Note [Promotions (HsTyVar)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsTyVar: A name in a type or kind.
  Here are the allowed namespaces for the name.
    In a type:
      Var: not allowed
      Data: promoted data constructor
      Tv: type variable
      TcCls before renamer: type constructor, class constructor, or promoted data constructor
      TcCls after renamer: type constructor or class constructor
    In a kind:
      Var, Data: not allowed
      Tv: kind variable
      TcCls: kind constructor or promoted type constructor

  The 'Promoted' field in an HsTyVar captures whether the type was promoted in
  the source code by prefixing an apostrophe.

Note [HsStarTy]
~~~~~~~~~~~~~~~
When the StarIsType extension is enabled, we want to treat '*' and its Unicode
variant identically to 'Data.Kind.Type'. Unfortunately, doing so in the parser
would mean that when we pretty-print it back, we don't know whether the user
wrote '*' or 'Type', and lose the parse/ppr roundtrip property.

As a workaround, we parse '*' as HsStarTy (if it stands for 'Data.Kind.Type')
and then desugar it to 'Data.Kind.Type' in the typechecker (see tc_hs_type).
When '*' is a regular type operator (StarIsType is disabled), HsStarTy is not
involved.


Note [Promoted lists and tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice the difference between
   HsListTy    HsExplicitListTy
   HsTupleTy   HsExplicitListTupleTy

E.g.    f :: [Int]                      HsListTy

        g3  :: T '[]                   All these use
        g2  :: T '[True]                  HsExplicitListTy
        g1  :: T '[True,False]
        g1a :: T [True,False]             (can omit ' where unambiguous)

  kind of T :: [Bool] -> *        This kind uses HsListTy!

E.g.    h :: (Int,Bool)                 HsTupleTy; f is a pair
        k :: S '(True,False)            HsExplicitTypleTy; S is indexed by
                                           a type-level pair of booleans
        kind of S :: (Bool,Bool) -> *   This kind uses HsExplicitTupleTy

Note [Distinguishing tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Apart from promotion, tuples can have one of three different kinds:

        x :: (Int, Bool)                -- Regular boxed tuples
        f :: Int# -> (# Int#, Int# #)   -- Unboxed tuples
        g :: (Eq a, Ord a) => a         -- Constraint tuples

For convenience, internally we use a single constructor for all of these,
namely HsTupleTy, but keep track of the tuple kind (in the first argument to
HsTupleTy, a HsTupleSort). We can tell if a tuple is unboxed while parsing,
because of the #. However, with -XConstraintKinds we can only distinguish
between constraint and boxed tuples during type checking, in general. Hence the
four constructors of HsTupleSort:

        HsUnboxedTuple                  -> Produced by the parser
        HsBoxedTuple                    -> Certainly a boxed tuple
        HsConstraintTuple               -> Certainly a constraint tuple
        HsBoxedOrConstraintTuple        -> Could be a boxed or a constraint
                                        tuple. Produced by the parser only,
                                        disappears after type checking
-}

-- | Haskell Tuple Sort
data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple
                 deriving Data

-- | Located Constructor Declaration Field
type LConDeclField pass = Located (ConDeclField pass)
      -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation

-- | Constructor Declaration Field
data ConDeclField pass  -- Record fields have Haddock docs on them
  = ConDeclField { cd_fld_ext  :: XConDeclField pass,
                   cd_fld_names :: [LFieldOcc pass],
                                   -- ^ See Note [ConDeclField passs]
                   cd_fld_type :: LBangType pass,
                   cd_fld_doc  :: Maybe LHsDocString }
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

      -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | XConDeclField !(XXConDeclField pass)

type instance XConDeclField  (GhcPass _) = NoExtField
type instance XXConDeclField (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (ConDeclField (GhcPass p)) where
  ppr (ConDeclField _ fld_n fld_ty _) = ppr fld_n <+> dcolon <+> ppr fld_ty

-- HsConDetails is used for patterns/expressions *and* for data type
-- declarations
-- | Haskell Constructor Details
data HsConDetails arg rec
  = PrefixCon [arg]             -- C p1 p2 p3
  | RecCon    rec               -- C { x = p1, y = p2 }
  | InfixCon  arg arg           -- p1 `C` p2
  deriving Data

instance (Outputable arg, Outputable rec)
         => Outputable (HsConDetails arg rec) where
  ppr (PrefixCon args) = text "PrefixCon" <+> ppr args
  ppr (RecCon rec)     = text "RecCon:" <+> ppr rec
  ppr (InfixCon l r)   = text "InfixCon:" <+> ppr [l, r]

hsConDetailsArgs ::
     HsConDetails (LHsType a) (Located [LConDeclField a])
  -> [LHsType a]
hsConDetailsArgs details = case details of
  InfixCon a b -> [a,b]
  PrefixCon xs -> xs
  RecCon r -> map (cd_fld_type . unLoc) (unLoc r)

{-
Note [ConDeclField passs]
~~~~~~~~~~~~~~~~~~~~~~~~~

A ConDeclField contains a list of field occurrences: these always
include the field label as the user wrote it.  After the renamer, it
will additionally contain the identity of the selector function in the
second component.

Due to DuplicateRecordFields, the OccName of the selector function
may have been mangled, which is why we keep the original field label
separately.  For example, when DuplicateRecordFields is enabled

    data T = MkT { x :: Int }

gives

    ConDeclField { cd_fld_names = [L _ (FieldOcc "x" $sel:x:MkT)], ... }.
-}

-----------------------
-- A valid type must have a for-all at the top of the type, or of the fn arg
-- types

---------------------
hsWcScopedTvs :: LHsSigWcType GhcRn -> [Name]
-- Get the lexically-scoped type variables of a HsSigType
--  - the explicitly-given forall'd type variables
--  - the named wildcards; see Note [Scoping of named wildcards]
-- because they scope in the same way
hsWcScopedTvs sig_ty
  | HsWC { hswc_ext = nwcs, hswc_body = sig_ty1 }  <- sig_ty
  , HsIB { hsib_ext = vars
         , hsib_body = sig_ty2 } <- sig_ty1
  = case sig_ty2 of
      L _ (HsForAllTy { hst_fvf = ForallInvis -- See Note [hsScopedTvs vis_flag]
                      , hst_bndrs = tvs }) ->
        vars ++ nwcs ++ hsLTyVarNames tvs
      _                                    -> nwcs

hsScopedTvs :: LHsSigType GhcRn -> [Name]
-- Same as hsWcScopedTvs, but for a LHsSigType
hsScopedTvs sig_ty
  | HsIB { hsib_ext = vars
         , hsib_body = sig_ty2 } <- sig_ty
  , L _ (HsForAllTy { hst_fvf = ForallInvis -- See Note [hsScopedTvs vis_flag]
                    , hst_bndrs = tvs }) <- sig_ty2
  = vars ++ hsLTyVarNames tvs
  | otherwise
  = []

{- Note [Scoping of named wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: _a -> _a
  f x = let g :: _a -> _a
            g = ...
        in ...

Currently, for better or worse, the "_a" variables are all the same. So
although there is no explicit forall, the "_a" scopes over the definition.
I don't know if this is a good idea, but there it is.
-}

{- Note [hsScopedTvs vis_flag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-XScopedTypeVariables can be defined in terms of a desugaring to
-XTypeAbstractions (GHC Proposal #50):

    fn :: forall a b c. tau(a,b,c)            fn :: forall a b c. tau(a,b,c)
    fn = defn(a,b,c)                   ==>    fn @x @y @z = defn(x,y,z)

That is, for every type variable of the leading 'forall' in the type signature,
we add an invisible binder at term level.

This model does not extend to visible forall, as discussed here:

* https://gitlab.haskell.org/ghc/ghc/issues/16734#note_203412
* https://github.com/ghc-proposals/ghc-proposals/pull/238

The conclusion of these discussions can be summarized as follows:

  > Assuming support for visible 'forall' in terms, consider this example:
  >
  >     vfn :: forall x y -> tau(x,y)
  >     vfn = \a b -> ...
  >
  > The user has written their own binders 'a' and 'b' to stand for 'x' and
  > 'y', and we definitely should not desugar this into:
  >
  >     vfn :: forall x y -> tau(x,y)
  >     vfn x y = \a b -> ...         -- bad!

We cement this design by pattern-matching on ForallInvis in hsScopedTvs:

    hsScopedTvs (HsForAllTy { hst_fvf = ForallInvis, ... }) = ...

At the moment, GHC does not support visible 'forall' in terms. Nevertheless,
it is still possible to write erroneous programs that use visible 'forall's in
terms, such as this example:

    x :: forall a -> a -> a
    x = x

If we do not pattern-match on ForallInvis in hsScopedTvs, then `a` would
erroneously be brought into scope over the body of `x` when renaming it.
Although the typechecker would later reject this (see `GHC.Tc.Validity.vdqAllowed`),
it is still possible for this to wreak havoc in the renamer before it gets to
that point (see #17687 for an example of this).
Bottom line: nip problems in the bud by matching on ForallInvis from the start.
-}

---------------------
hsTyVarName :: HsTyVarBndr flag (GhcPass p) -> IdP (GhcPass p)
hsTyVarName (UserTyVar _ _ (L _ n))     = n
hsTyVarName (KindedTyVar _ _ (L _ n) _) = n

hsLTyVarName :: LHsTyVarBndr flag (GhcPass p) -> IdP (GhcPass p)
hsLTyVarName = hsTyVarName . unLoc

hsLTyVarNames :: [LHsTyVarBndr flag (GhcPass p)] -> [IdP (GhcPass p)]
hsLTyVarNames = map hsLTyVarName

hsExplicitLTyVarNames :: LHsQTyVars (GhcPass p) -> [IdP (GhcPass p)]
-- Explicit variables only
hsExplicitLTyVarNames qtvs = map hsLTyVarName (hsQTvExplicit qtvs)

hsAllLTyVarNames :: LHsQTyVars GhcRn -> [Name]
-- All variables
hsAllLTyVarNames (HsQTvs { hsq_ext = kvs
                         , hsq_explicit = tvs })
  = kvs ++ hsLTyVarNames tvs

hsLTyVarLocName :: LHsTyVarBndr flag (GhcPass p) -> Located (IdP (GhcPass p))
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: LHsQTyVars (GhcPass p) -> [Located (IdP (GhcPass p))]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvExplicit qtvs)

-- | Get the kind signature of a type, ignoring parentheses:
--
--   hsTyKindSig   `Maybe                    `   =   Nothing
--   hsTyKindSig   `Maybe ::   Type -> Type  `   =   Just  `Type -> Type`
--   hsTyKindSig   `Maybe :: ((Type -> Type))`   =   Just  `Type -> Type`
--
-- This is used to extract the result kind of type synonyms with a CUSK:
--
--  type S = (F :: res_kind)
--                 ^^^^^^^^
--
hsTyKindSig :: LHsType pass -> Maybe (LHsKind pass)
hsTyKindSig lty =
  case unLoc lty of
    HsParTy _ lty'    -> hsTyKindSig lty'
    HsKindSig _ _ k   -> Just k
    _                 -> Nothing

---------------------
ignoreParens :: LHsType pass -> LHsType pass
ignoreParens (L _ (HsParTy _ ty)) = ignoreParens ty
ignoreParens ty                   = ty

isLHsForAllTy :: LHsType p -> Bool
isLHsForAllTy (L _ (HsForAllTy {})) = True
isLHsForAllTy _                     = False

{-
************************************************************************
*                                                                      *
                Building types
*                                                                      *
************************************************************************
-}

mkAnonWildCardTy :: HsType GhcPs
mkAnonWildCardTy = HsWildCardTy noExtField

mkHsOpTy :: LHsType (GhcPass p) -> Located (IdP (GhcPass p))
         -> LHsType (GhcPass p) -> HsType (GhcPass p)
mkHsOpTy ty1 op ty2 = HsOpTy noExtField ty1 op ty2

mkHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
mkHsAppTy t1 t2
  = addCLoc t1 t2 (HsAppTy noExtField t1 (parenthesizeHsType appPrec t2))

mkHsAppTys :: LHsType (GhcPass p) -> [LHsType (GhcPass p)]
           -> LHsType (GhcPass p)
mkHsAppTys = foldl' mkHsAppTy

mkHsAppKindTy :: XAppKindTy (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
              -> LHsType (GhcPass p)
mkHsAppKindTy ext ty k
  = addCLoc ty k (HsAppKindTy ext ty k)

{-
************************************************************************
*                                                                      *
                Decomposing HsTypes
*                                                                      *
************************************************************************
-}

---------------------------------
-- splitHsFunType decomposes a type (t1 -> t2 ... -> tn)
-- Breaks up any parens in the result type:
--      splitHsFunType (a -> (b -> c)) = ([a,b], c)
splitHsFunType :: LHsType GhcRn -> ([LHsType GhcRn], LHsType GhcRn)
splitHsFunType (L _ (HsParTy _ ty))
  = splitHsFunType ty

splitHsFunType (L _ (HsFunTy _ x y))
  | (args, res) <- splitHsFunType y
  = (x:args, res)

splitHsFunType other = ([], other)

-- retrieve the name of the "head" of a nested type application
-- somewhat like splitHsAppTys, but a little more thorough
-- used to examine the result of a GADT-like datacon, so it doesn't handle
-- *all* cases (like lists, tuples, (~), etc.)
hsTyGetAppHead_maybe :: LHsType (GhcPass p)
                     -> Maybe (Located (IdP (GhcPass p)))
hsTyGetAppHead_maybe = go
  where
    go (L _ (HsTyVar _ _ ln))          = Just ln
    go (L _ (HsAppTy _ l _))           = go l
    go (L _ (HsAppKindTy _ t _))       = go t
    go (L _ (HsOpTy _ _ (L loc n) _))  = Just (L loc n)
    go (L _ (HsParTy _ t))             = go t
    go (L _ (HsKindSig _ t _))         = go t
    go _                               = Nothing

------------------------------------------------------------
-- Arguments in an expression/type after splitting
data HsArg tm ty
  = HsValArg tm   -- Argument is an ordinary expression     (f arg)
  | HsTypeArg SrcSpan ty -- Argument is a visible type application (f @ty)
                         -- SrcSpan is location of the `@`
  | HsArgPar SrcSpan -- See Note [HsArgPar]

numVisibleArgs :: [HsArg tm ty] -> Arity
numVisibleArgs = count is_vis
  where is_vis (HsValArg _) = True
        is_vis _            = False

-- type level equivalent
type LHsTypeArg p = HsArg (LHsType p) (LHsKind p)

-- | Compute the 'SrcSpan' associated with an 'LHsTypeArg'.
lhsTypeArgSrcSpan :: LHsTypeArg pass -> SrcSpan
lhsTypeArgSrcSpan = \ case
  HsValArg  tm    -> getLoc tm
  HsTypeArg at ty -> at <> getLoc ty
  HsArgPar  sp    -> sp

instance (Outputable tm, Outputable ty) => Outputable (HsArg tm ty) where
  ppr (HsValArg tm)    = ppr tm
  ppr (HsTypeArg _ ty) = char '@' <> ppr ty
  ppr (HsArgPar sp)    = text "HsArgPar"  <+> ppr sp
{-
Note [HsArgPar]
A HsArgPar indicates that everything to the left of this in the argument list is
enclosed in parentheses together with the function itself. It is necessary so
that we can recreate the parenthesis structure in the original source after
typechecking the arguments.

The SrcSpan is the span of the original HsPar

((f arg1) arg2 arg3) results in an input argument list of
[HsValArg arg1, HsArgPar span1, HsValArg arg2, HsValArg arg3, HsArgPar span2]

-}

--------------------------------

-- | Decompose a pattern synonym type signature into its constituent parts.
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall a. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsPatSynTy :: LHsType pass
                 -> ( [LHsTyVarBndr Specificity pass]    -- universals
                    , LHsContext pass        -- required constraints
                    , [LHsTyVarBndr Specificity pass]    -- existentials
                    , LHsContext pass        -- provided constraints
                    , LHsType pass)          -- body type
splitLHsPatSynTy ty = (univs, reqs, exis, provs, ty4)
  where
    (univs, ty1) = splitLHsForAllTyInvis ty
    (reqs,  ty2) = splitLHsQualTy ty1
    (exis,  ty3) = splitLHsForAllTyInvis ty2
    (provs, ty4) = splitLHsQualTy ty3

-- | Decompose a sigma type (of the form @forall <tvs>. context => body@)
-- into its constituent parts.
-- Only splits type variable binders that were
-- quantified invisibly (e.g., @forall a.@, with a dot).
--
-- This function is used to split apart certain types, such as instance
-- declaration types, which disallow visible @forall@s. For instance, if GHC
-- split apart the @forall@ in @instance forall a -> Show (Blah a)@, then that
-- declaration would mistakenly be accepted!
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall a. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsSigmaTyInvis :: LHsType pass
                     -> ([LHsTyVarBndr Specificity pass], LHsContext pass, LHsType pass)
splitLHsSigmaTyInvis ty
  | (tvs,  ty1) <- splitLHsForAllTyInvis ty
  , (ctxt, ty2) <- splitLHsQualTy ty1
  = (tvs, ctxt, ty2)

-- | Decompose a prefix GADT type into its constituent parts.
-- Returns @(mb_tvbs, mb_ctxt, body)@, where:
--
-- * @mb_tvbs@ are @Just@ the leading @forall@s, if they are provided.
--   Otherwise, they are @Nothing@.
--
-- * @mb_ctxt@ is @Just@ the context, if it is provided.
--   Otherwise, it is @Nothing@.
--
-- * @body@ is the body of the type after the optional @forall@s and context.
--
-- This function is careful not to look through parentheses.
-- See @Note [GADT abstract syntax] (Wrinkle: No nested foralls or contexts)@
-- "GHC.Hs.Decls" for why this is important.
splitLHsGADTPrefixTy ::
     LHsType pass
  -> (Maybe [LHsTyVarBndr Specificity pass], Maybe (LHsContext pass), LHsType pass)
splitLHsGADTPrefixTy ty
  | (mb_tvbs, rho) <- split_forall ty
  , (mb_ctxt, tau) <- split_ctxt rho
  = (mb_tvbs, mb_ctxt, tau)
  where
    -- NB: We do not use splitLHsForAllTyInvis below, since that looks through
    -- parentheses...
    split_forall (L _ (HsForAllTy { hst_fvf = ForallInvis, hst_bndrs = bndrs
                                  , hst_body = rho }))
      = (Just bndrs, rho)
    split_forall sigma
      = (Nothing, sigma)

    -- ...similarly, we do not use splitLHsQualTy below, since that also looks
    -- through parentheses.
    split_ctxt (L _ (HsQualTy { hst_ctxt = cxt, hst_body = tau }))
      = (Just cxt, tau)
    split_ctxt tau
      = (Nothing, tau)

-- | Decompose a type of the form @forall <tvs>. body@ into its constituent
-- parts. Only splits type variable binders that
-- were quantified invisibly (e.g., @forall a.@, with a dot).
--
-- This function is used to split apart certain types, such as instance
-- declaration types, which disallow visible @forall@s. For instance, if GHC
-- split apart the @forall@ in @instance forall a -> Show (Blah a)@, then that
-- declaration would mistakenly be accepted!
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall a. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsForAllTyInvis :: LHsType pass -> ([LHsTyVarBndr Specificity pass], LHsType pass)
splitLHsForAllTyInvis lty@(L _ ty) =
  case ty of
    HsParTy _ ty' -> splitLHsForAllTyInvis ty'
    HsForAllTy { hst_fvf = fvf', hst_bndrs = tvs', hst_body = body' }
      |  fvf' == ForallInvis
      -> (tvs', body')
    _ -> ([], lty)

-- | Decompose a type of the form @context => body@ into its constituent parts.
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(context => <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsQualTy :: LHsType pass -> (LHsContext pass, LHsType pass)
splitLHsQualTy (L _ (HsParTy _ ty)) = splitLHsQualTy ty
splitLHsQualTy (L _ (HsQualTy { hst_ctxt = ctxt, hst_body = body })) = (ctxt,     body)
splitLHsQualTy body              = (noLHsContext, body)

-- | Decompose a type class instance type (of the form
-- @forall <tvs>. context => instance_head@) into its constituent parts.
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall <tvs>. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsInstDeclTy :: LHsSigType GhcRn
                   -> ([Name], LHsContext GhcRn, LHsType GhcRn)
-- Split up an instance decl type, returning the pieces
splitLHsInstDeclTy (HsIB { hsib_ext = itkvs
                         , hsib_body = inst_ty })
  | (tvs, cxt, body_ty) <- splitLHsSigmaTyInvis inst_ty
  = (itkvs ++ hsLTyVarNames tvs, cxt, body_ty)
         -- Return implicitly bound type and kind vars
         -- For an instance decl, all of them are in scope

getLHsInstDeclHead :: LHsSigType (GhcPass p) -> LHsType (GhcPass p)
getLHsInstDeclHead inst_ty
  | (_tvs, _cxt, body_ty) <- splitLHsSigmaTyInvis (hsSigType inst_ty)
  = body_ty

getLHsInstDeclClass_maybe :: LHsSigType (GhcPass p)
                          -> Maybe (Located (IdP (GhcPass p)))
-- Works on (HsSigType RdrName)
getLHsInstDeclClass_maybe = hsTyGetAppHead_maybe . getLHsInstDeclHead

{-
************************************************************************
*                                                                      *
                FieldOcc
*                                                                      *
************************************************************************
-}

-- | Located Field Occurrence
type LFieldOcc pass = Located (FieldOcc pass)

-- | Field Occurrence
--
-- Represents an *occurrence* of an unambiguous field.  We store
-- both the 'RdrName' the user originally wrote, and after the
-- renamer, the selector function.
data FieldOcc pass = FieldOcc { extFieldOcc     :: XCFieldOcc pass
                              , rdrNameFieldOcc :: Located RdrName
                                 -- ^ See Note [Located RdrNames] in "GHC.Hs.Expr"
                              }

  | XFieldOcc
      !(XXFieldOcc pass)
deriving instance Eq  (XCFieldOcc (GhcPass p)) => Eq  (FieldOcc (GhcPass p))

type instance XCFieldOcc GhcPs = NoExtField
type instance XCFieldOcc GhcRn = Name
type instance XCFieldOcc GhcTc = Id

type instance XXFieldOcc (GhcPass _) = NoExtCon

instance Outputable (FieldOcc pass) where
  ppr = ppr . rdrNameFieldOcc

mkFieldOcc :: Located RdrName -> FieldOcc GhcPs
mkFieldOcc rdr = FieldOcc noExtField rdr


-- | Ambiguous Field Occurrence
--
-- Represents an *occurrence* of a field that is potentially
-- ambiguous after the renamer, with the ambiguity resolved by the
-- typechecker.  We always store the 'RdrName' that the user
-- originally wrote, and store the selector function after the renamer
-- (for unambiguous occurrences) or the typechecker (for ambiguous
-- occurrences).
--
-- See Note [HsRecField and HsRecUpdField] in "GHC.Hs.Pat" and
-- Note [Disambiguating record fields] in "GHC.Tc.Gen.Expr".
-- See Note [Located RdrNames] in "GHC.Hs.Expr"
data AmbiguousFieldOcc pass
  = Unambiguous (XUnambiguous pass) (Located RdrName)
  | Ambiguous   (XAmbiguous pass)   (Located RdrName)
  | XAmbiguousFieldOcc !(XXAmbiguousFieldOcc pass)

type instance XUnambiguous GhcPs = NoExtField
type instance XUnambiguous GhcRn = Name
type instance XUnambiguous GhcTc = Id

type instance XAmbiguous GhcPs = NoExtField
type instance XAmbiguous GhcRn = NoExtField
type instance XAmbiguous GhcTc = Id

type instance XXAmbiguousFieldOcc (GhcPass _) = NoExtCon

instance Outputable (AmbiguousFieldOcc (GhcPass p)) where
  ppr = ppr . rdrNameAmbiguousFieldOcc

instance OutputableBndr (AmbiguousFieldOcc (GhcPass p)) where
  pprInfixOcc  = pprInfixOcc . rdrNameAmbiguousFieldOcc
  pprPrefixOcc = pprPrefixOcc . rdrNameAmbiguousFieldOcc

mkAmbiguousFieldOcc :: Located RdrName -> AmbiguousFieldOcc GhcPs
mkAmbiguousFieldOcc rdr = Unambiguous noExtField rdr

rdrNameAmbiguousFieldOcc :: AmbiguousFieldOcc (GhcPass p) -> RdrName
rdrNameAmbiguousFieldOcc (Unambiguous _ (L _ rdr)) = rdr
rdrNameAmbiguousFieldOcc (Ambiguous   _ (L _ rdr)) = rdr

selectorAmbiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> Id
selectorAmbiguousFieldOcc (Unambiguous sel _) = sel
selectorAmbiguousFieldOcc (Ambiguous   sel _) = sel

unambiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> FieldOcc GhcTc
unambiguousFieldOcc (Unambiguous rdr sel) = FieldOcc rdr sel
unambiguousFieldOcc (Ambiguous   rdr sel) = FieldOcc rdr sel

ambiguousFieldOcc :: FieldOcc GhcTc -> AmbiguousFieldOcc GhcTc
ambiguousFieldOcc (FieldOcc sel rdr) = Unambiguous sel rdr

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

class OutputableBndrFlag flag where
    pprTyVarBndr :: OutputableBndrId p => HsTyVarBndr flag (GhcPass p) -> SDoc

instance OutputableBndrFlag () where
    pprTyVarBndr (UserTyVar _ _ n)     = ppr n
    pprTyVarBndr (KindedTyVar _ _ n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance OutputableBndrFlag Specificity where
    pprTyVarBndr (UserTyVar _ SpecifiedSpec n)     = ppr n
    pprTyVarBndr (UserTyVar _ InferredSpec n)      = braces $ ppr n
    pprTyVarBndr (KindedTyVar _ SpecifiedSpec n k) = parens $ hsep [ppr n, dcolon, ppr k]
    pprTyVarBndr (KindedTyVar _ InferredSpec n k)  = braces $ hsep [ppr n, dcolon, ppr k]

instance OutputableBndrId p => Outputable (HsType (GhcPass p)) where
    ppr = pprHsType

instance Outputable HsTyLit where
    ppr = ppr_tylit

instance OutputableBndrId p
       => Outputable (LHsQTyVars (GhcPass p)) where
    ppr (HsQTvs { hsq_explicit = tvs }) = interppSP tvs

instance (OutputableBndrId p, OutputableBndrFlag flag)
       => Outputable (HsTyVarBndr flag (GhcPass p)) where
    ppr = pprTyVarBndr

instance Outputable thing
       => Outputable (HsImplicitBndrs (GhcPass p) thing) where
    ppr (HsIB { hsib_body = ty }) = ppr ty

instance Outputable thing
       => Outputable (HsWildCardBndrs (GhcPass p) thing) where
    ppr (HsWC { hswc_body = ty }) = ppr ty

instance OutputableBndrId p
       => Outputable (HsPatSigType (GhcPass p)) where
    ppr (HsPS { hsps_body = ty }) = ppr ty

pprAnonWildCard :: SDoc
pprAnonWildCard = char '_'

-- | Prints a forall; When passed an empty list, prints @forall .@/@forall ->@
-- only when @-dppr-debug@ is enabled.
pprHsForAll :: (OutputableBndrId p, OutputableBndrFlag flag)
            => ForallVisFlag -> [LHsTyVarBndr flag (GhcPass p)]
            -> LHsContext (GhcPass p) -> SDoc
pprHsForAll = pprHsForAllExtra Nothing

-- | Version of 'pprHsForAll' that can also print an extra-constraints
-- wildcard, e.g. @_ => a -> Bool@ or @(Show a, _) => a -> String@. This
-- underscore will be printed when the 'Maybe SrcSpan' argument is a 'Just'
-- containing the location of the extra-constraints wildcard. A special
-- function for this is needed, as the extra-constraints wildcard is removed
-- from the actual context and type, and stored in a separate field, thus just
-- printing the type will not print the extra-constraints wildcard.
pprHsForAllExtra :: (OutputableBndrId p, OutputableBndrFlag flag)
                 => Maybe SrcSpan -> ForallVisFlag
                 -> [LHsTyVarBndr flag (GhcPass p)]
                 -> LHsContext (GhcPass p) -> SDoc
pprHsForAllExtra extra fvf qtvs cxt
  = pp_forall <+> pprLHsContextExtra (isJust extra) cxt
  where
    pp_forall | null qtvs = whenPprDebug (forAllLit <> separator)
              | otherwise = forAllLit <+> interppSP qtvs <> separator

    separator = ppr_forall_separator fvf

-- | Version of 'pprHsForAll' or 'pprHsForAllExtra' that will always print
-- @forall.@ when passed @Just []@. Prints nothing if passed 'Nothing'
pprHsExplicitForAll :: (OutputableBndrId p)
                    => ForallVisFlag
                    -> Maybe [LHsTyVarBndr () (GhcPass p)] -> SDoc
pprHsExplicitForAll fvf (Just qtvs) = forAllLit <+> interppSP qtvs
                                                 <> ppr_forall_separator fvf
pprHsExplicitForAll _   Nothing     = mempty

-- | Prints an arrow for visible @forall@s (e.g., @forall a ->@) and a dot for
-- invisible @forall@s (e.g., @forall a.@).
ppr_forall_separator :: ForallVisFlag -> SDoc
ppr_forall_separator ForallVis   = space <> arrow
ppr_forall_separator ForallInvis = dot

pprLHsContext :: (OutputableBndrId p)
              => LHsContext (GhcPass p) -> SDoc
pprLHsContext lctxt
  | null (unLoc lctxt) = mempty
  | otherwise          = pprLHsContextAlways lctxt

-- For use in a HsQualTy, which always gets printed if it exists.
pprLHsContextAlways :: (OutputableBndrId p)
                    => LHsContext (GhcPass p) -> SDoc
pprLHsContextAlways (L _ ctxt)
  = case ctxt of
      []       -> parens mempty            <+> darrow
      [L _ ty] -> ppr_mono_ty ty           <+> darrow
      _        -> parens (interpp'SP ctxt) <+> darrow

-- True <=> print an extra-constraints wildcard, e.g. @(Show a, _) =>@
pprLHsContextExtra :: (OutputableBndrId p)
                   => Bool -> LHsContext (GhcPass p) -> SDoc
pprLHsContextExtra show_extra lctxt@(L _ ctxt)
  | not show_extra = pprLHsContext lctxt
  | null ctxt      = char '_' <+> darrow
  | otherwise      = parens (sep (punctuate comma ctxt')) <+> darrow
  where
    ctxt' = map ppr ctxt ++ [char '_']

pprConDeclFields :: (OutputableBndrId p)
                 => [LConDeclField (GhcPass p)] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns, cd_fld_type = ty,
                                 cd_fld_doc = doc }))
        = ppr_names ns <+> dcolon <+> ppr ty <+> foldMap ppr doc
    ppr_fld (L _ (XConDeclField x)) = ppr x
    ppr_names [n] = ppr n
    ppr_names ns = sep (punctuate comma (map ppr ns))

{-
Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)
-}

-- Printing works more-or-less as for Types

pprHsType :: (OutputableBndrId p) => HsType (GhcPass p) -> SDoc
pprHsType ty = ppr_mono_ty ty

ppr_mono_lty :: (OutputableBndrId p) => LHsType (GhcPass p) -> SDoc
ppr_mono_lty ty = ppr_mono_ty (unLoc ty)

ppr_mono_ty :: (OutputableBndrId p) => HsType (GhcPass p) -> SDoc
ppr_mono_ty (HsForAllTy { hst_fvf = fvf, hst_bndrs = tvs, hst_body = ty })
  = sep [pprHsForAll fvf tvs noLHsContext, ppr_mono_lty ty]

ppr_mono_ty (HsQualTy { hst_ctxt = ctxt, hst_body = ty })
  = sep [pprLHsContextAlways ctxt, ppr_mono_lty ty]

ppr_mono_ty (HsBangTy _ b ty)   = ppr b <> ppr_mono_lty ty
ppr_mono_ty (HsRecTy _ flds)      = pprConDeclFields flds
ppr_mono_ty (HsTyVar _ prom (L _ name))
  | isPromoted prom = quote (pprPrefixOcc name)
  | otherwise       = pprPrefixOcc name
ppr_mono_ty (HsFunTy _ ty1 ty2)   = ppr_fun_ty ty1 ty2
ppr_mono_ty (HsTupleTy _ con tys)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `Solo x`, not `(x)`
  | [ty] <- tys
  , BoxedTuple <- std_con
  = sep [text (mkTupleStr Boxed 1), ppr_mono_lty ty]
  | otherwise
  = tupleParens std_con (pprWithCommas ppr tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty (HsSumTy _ tys)
  = tupleParens UnboxedTuple (pprWithBars ppr tys)
ppr_mono_ty (HsKindSig _ ty kind)
  = ppr_mono_lty ty <+> dcolon <+> ppr kind
ppr_mono_ty (HsListTy _ ty)       = brackets (ppr_mono_lty ty)
ppr_mono_ty (HsIParamTy _ n ty)   = (ppr n <+> dcolon <+> ppr_mono_lty ty)
ppr_mono_ty (HsSpliceTy _ s)      = pprSplice s
ppr_mono_ty (HsExplicitListTy _ prom tys)
  | isPromoted prom = quote $ brackets (maybeAddSpace tys $ interpp'SP tys)
  | otherwise       = brackets (interpp'SP tys)
ppr_mono_ty (HsExplicitTupleTy _ tys)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `'Solo x`, not `'(x)`
  | [ty] <- tys
  = quote $ sep [text (mkTupleStr Boxed 1), ppr_mono_lty ty]
  | otherwise
  = quote $ parens (maybeAddSpace tys $ interpp'SP tys)
ppr_mono_ty (HsTyLit _ t)       = ppr_tylit t
ppr_mono_ty (HsWildCardTy {})   = char '_'

ppr_mono_ty (HsStarTy _ isUni)  = char (if isUni then '???' else '*')

ppr_mono_ty (HsAppTy _ fun_ty arg_ty)
  = hsep [ppr_mono_lty fun_ty, ppr_mono_lty arg_ty]
ppr_mono_ty (HsAppKindTy _ ty k)
  = ppr_mono_lty ty <+> char '@' <> ppr_mono_lty k
ppr_mono_ty (HsOpTy _ ty1 (L _ op) ty2)
  = sep [ ppr_mono_lty ty1
        , sep [pprInfixOcc op, ppr_mono_lty ty2 ] ]

ppr_mono_ty (HsParTy _ ty)
  = parens (ppr_mono_lty ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty (HsDocTy _ ty doc)
  -- AZ: Should we add parens?  Should we introduce "-- ^"?
  = ppr_mono_lty ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

ppr_mono_ty (XHsType t) = ppr t

--------------------------
ppr_fun_ty :: (OutputableBndrId p)
           => LHsType (GhcPass p) -> LHsType (GhcPass p) -> SDoc
ppr_fun_ty ty1 ty2
  = let p1 = ppr_mono_lty ty1
        p2 = ppr_mono_lty ty2
    in
    sep [p1, arrow <+> p2]

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)


-- | @'hsTypeNeedsParens' p t@ returns 'True' if the type @t@ needs parentheses
-- under precedence @p@.
hsTypeNeedsParens :: PprPrec -> HsType pass -> Bool
hsTypeNeedsParens p = go
  where
    go (HsForAllTy{})        = p >= funPrec
    go (HsQualTy{})          = p >= funPrec
    go (HsBangTy{})          = p > topPrec
    go (HsRecTy{})           = False
    go (HsTyVar{})           = False
    go (HsFunTy{})           = p >= funPrec
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsKindSig{})         = p >= sigPrec
    go (HsListTy{})          = False
    go (HsIParamTy{})        = p > topPrec
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy{})  = False
    go (HsExplicitTupleTy{}) = False
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsStarTy{})          = p >= starPrec
    go (HsAppTy{})           = p >= appPrec
    go (HsAppKindTy{})       = p >= appPrec
    go (HsOpTy{})            = p >= opPrec
    go (HsParTy{})           = False
    go (HsDocTy _ (L _ t) _) = go t
    go (XHsType{})           = False

maybeAddSpace :: [LHsType pass] -> SDoc -> SDoc
-- See Note [Printing promoted type constructors]
-- in GHC.Iface.Type.  This code implements the same
-- logic for printing HsType
maybeAddSpace tys doc
  | (ty : _) <- tys
  , lhsTypeHasLeadingPromotionQuote ty = space <> doc
  | otherwise                          = doc

lhsTypeHasLeadingPromotionQuote :: LHsType pass -> Bool
lhsTypeHasLeadingPromotionQuote = goL
  where
    goL = go . unLoc

    go (HsForAllTy{})        = False
    go (HsQualTy{ hst_ctxt = ctxt, hst_body = body})
      | L _ (c:_) <- ctxt    = goL c
      | otherwise            = goL body
    go (HsBangTy{})          = False
    go (HsRecTy{})           = False
    go (HsTyVar _ p _)       = isPromoted p
    go (HsFunTy _ arg _)     = goL arg
    go (HsListTy{})          = False
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsOpTy _ t1 _ _)     = goL t1
    go (HsKindSig _ t _)     = goL t
    go (HsIParamTy{})        = False
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy _ p _) = isPromoted p
    go (HsExplicitTupleTy{}) = True
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsStarTy{})          = False
    go (HsAppTy _ t _)       = goL t
    go (HsAppKindTy _ t _)   = goL t
    go (HsParTy{})           = False
    go (HsDocTy _ t _)       = goL t
    go (XHsType{})           = False

-- | @'parenthesizeHsType' p ty@ checks if @'hsTypeNeedsParens' p ty@ is
-- true, and if so, surrounds @ty@ with an 'HsParTy'. Otherwise, it simply
-- returns @ty@.
parenthesizeHsType :: PprPrec -> LHsType (GhcPass p) -> LHsType (GhcPass p)
parenthesizeHsType p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy noExtField lty)
  | otherwise              = lty

-- | @'parenthesizeHsContext' p ctxt@ checks if @ctxt@ is a single constraint
-- @c@ such that @'hsTypeNeedsParens' p c@ is true, and if so, surrounds @c@
-- with an 'HsParTy' to form a parenthesized @ctxt@. Otherwise, it simply
-- returns @ctxt@ unchanged.
parenthesizeHsContext :: PprPrec
                      -> LHsContext (GhcPass p) -> LHsContext (GhcPass p)
parenthesizeHsContext p lctxt@(L loc ctxt) =
  case ctxt of
    [c] -> L loc [parenthesizeHsType p c]
    _   -> lctxt -- Other contexts are already "parenthesized" by virtue of
                 -- being tuples.
