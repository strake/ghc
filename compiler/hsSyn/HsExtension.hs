{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder

module HsExtension where

-- This module captures the type families to precisely identify the extension
-- points for HsSyn

import GhcPrelude

import GHC.Exts (Constraint)
import Data.Data hiding ( Fixity )
import PlaceHolder
import BasicTypes
import ConLike
import NameSet
import Name
import RdrName
import Var
import Type       ( Type )
import Outputable
import SrcLoc (Located)
import Coercion
import TcEvidence

{-
Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

See https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow

The hsSyn AST is reused across multiple compiler passes. We also have the
Template Haskell AST, and the haskell-src-exts one (outside of GHC)

Supporting multiple passes means the AST has various warts on it to cope with
the specifics for the phases, such as the 'ValBindsOut', 'ConPatOut',
'SigPatOut' etc.

The growable AST will allow each of these variants to be captured explicitly,
such that they only exist in the given compiler pass AST, as selected by the
type parameter to the AST.

In addition it will allow tool writers to define their own extensions to capture
additional information for the tool, in a natural way.

A further goal is to provide a means to harmonise the Template Haskell and
haskell-src-exts ASTs as well.

-}

-- | Used when constructing a term with an unused extension point.
noExt :: PlaceHolder
noExt = PlaceHolder

-- | Used as a data type index for the hsSyn AST
data GhcPass (c :: Pass)
deriving instance Eq (GhcPass c)
deriving instance Typeable c => Data (GhcPass c)

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Old 'RdrName' type param
type GhcRn   = GhcPass 'Renamed     -- Old 'Name' type param
type GhcTc   = GhcPass 'Typechecked -- Old 'Id' type para,
type GhcTcId = GhcTc                -- Old 'TcId' type param


-- | Types that are not defined until after type checking
type family PostTc x ty -- Note [Pass sensitive types] in PlaceHolder
type instance PostTc GhcPs ty = PlaceHolder
type instance PostTc GhcRn ty = PlaceHolder
type instance PostTc GhcTc ty = ty

-- deriving instance (Data ty) => Data (PostTc (GhcPass 'Parsed) ty)

-- | Types that are not defined until after renaming
type family PostRn x ty  -- Note [Pass sensitive types] in PlaceHolder
type instance PostRn GhcPs ty = PlaceHolder
type instance PostRn GhcRn ty = ty
type instance PostRn GhcTc ty = ty

-- | Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GhcPs = RdrName
type instance IdP GhcRn = Name
type instance IdP GhcTc = Id
-- type instance IdP (GHC x) = IdP x

type LIdP p = Located (IdP p)

-- ---------------------------------------------------------------------
-- type families for the Pat extension points
type family XWildPat   x
type family XVarPat    x
type family XLazyPat   x
type family XAsPat     x
type family XParPat    x
type family XBangPat   x
type family XListPat   x
type family XTuplePat  x
type family XSumPat    x
type family XPArrPat   x
type family XConPat    x
type family XViewPat   x
type family XSplicePat x
type family XLitPat    x
type family XNPat      x
type family XNPlusKPat x
type family XSigPat    x
type family XCoPat     x
type family XXPat      x


type ForallXPat (c :: * -> Constraint) (x :: *) =
       ( c (XWildPat   x)
       , c (XVarPat    x)
       , c (XLazyPat   x)
       , c (XAsPat     x)
       , c (XParPat    x)
       , c (XBangPat   x)
       , c (XListPat   x)
       , c (XTuplePat  x)
       , c (XSumPat    x)
       , c (XPArrPat   x)
       , c (XViewPat   x)
       , c (XSplicePat x)
       , c (XLitPat    x)
       , c (XNPat      x)
       , c (XNPlusKPat x)
       , c (XSigPat    x)
       , c (XCoPat     x)
       , c (XXPat      x)
       )
-- ---------------------------------------------------------------------
-- ValBindsLR type families

type family XValBinds    x x'
type family XXValBindsLR x x'

type ForallXValBindsLR (c :: * -> Constraint) (x :: *) (x' :: *)=
       ( c (XValBinds    x x')
       , c (XXValBindsLR x x')
       )




-- We define a type family for each HsLit extension point. This is based on
-- prepending 'X' to the constructor name, for ease of reference.
type family XHsChar       x
type family XHsCharPrim   x
type family XHsString     x
type family XHsStringPrim x
type family XHsInt        x
type family XHsIntPrim    x
type family XHsWordPrim   x
type family XHsInt64Prim  x
type family XHsWord64Prim x
type family XHsInteger    x
type family XHsRat        x
type family XHsFloatPrim  x
type family XHsDoublePrim x
type family XXLit         x

-- | Helper to apply a constraint to all HsLit extension points. It has one
-- entry per extension point type family.
type ForallXHsLit (c :: * -> Constraint) (x :: *) =
  ( c (XHsChar       x)
  , c (XHsCharPrim   x)
  , c (XHsString     x)
  , c (XHsStringPrim x)
  , c (XHsInt        x)
  , c (XHsIntPrim    x)
  , c (XHsWordPrim   x)
  , c (XHsInt64Prim  x)
  , c (XHsWord64Prim x)
  , c (XHsInteger    x)
  , c (XHsRat        x)
  , c (XHsFloatPrim  x)
  , c (XHsDoublePrim x)
  , c (XXLit         x)
  )


type family XOverLit  x
type family XXOverLit x

type ForallXOverLit (c :: * -> Constraint) (x :: *) =
       ( c (XOverLit  x)
       , c (XXOverLit x)
       )

-- ---------------------------------------------------------------------
-- Type families for the Type type families

type family XForAllTy        x
type family XQualTy          x
type family XTyVar           x
type family XAppsTy          x
type family XAppTy           x
type family XFunTy           x
type family XListTy          x
type family XPArrTy          x
type family XTupleTy         x
type family XSumTy           x
type family XOpTy            x
type family XParTy           x
type family XIParamTy        x
type family XEqTy            x
type family XKindSig         x
type family XSpliceTy        x
type family XDocTy           x
type family XBangTy          x
type family XRecTy           x
type family XExplicitListTy  x
type family XExplicitTupleTy x
type family XTyLit           x
type family XWildCardTy      x
type family XXType           x

-- | Helper to apply a constraint to all extension points. It has one
-- entry per extension point type family.
type ForallXType (c :: * -> Constraint) (x :: *) =
       ( c (XForAllTy        x)
       , c (XQualTy          x)
       , c (XTyVar           x)
       , c (XAppsTy          x)
       , c (XAppTy           x)
       , c (XFunTy           x)
       , c (XListTy          x)
       , c (XPArrTy          x)
       , c (XTupleTy         x)
       , c (XSumTy           x)
       , c (XOpTy            x)
       , c (XParTy           x)
       , c (XIParamTy        x)
       , c (XEqTy            x)
       , c (XKindSig         x)
       , c (XSpliceTy        x)
       , c (XDocTy           x)
       , c (XBangTy          x)
       , c (XRecTy           x)
       , c (XExplicitListTy  x)
       , c (XExplicitTupleTy x)
       , c (XTyLit           x)
       , c (XWildCardTy      x)
       , c (XXType           x)
       )

-- ---------------------------------------------------------------------

type family XUserTyVar   x
type family XKindedTyVar x
type family XXTyVarBndr  x

type ForallXTyVarBndr (c :: * -> Constraint) (x :: *) =
       ( c (XUserTyVar      x)
       , c (XKindedTyVar    x)
       , c (XXTyVarBndr     x)
       )

-- ---------------------------------------------------------------------

type family XAppInfix  x
type family XAppPrefix x
type family XXAppType  x

type ForallXAppType (c :: * -> Constraint) (x :: *) =
       ( c (XAppInfix   x)
       , c (XAppPrefix  x)
       , c (XXAppType   x)
       )

-- ---------------------------------------------------------------------

type family XFieldOcc  x
type family XXFieldOcc x

type ForallXFieldOcc (c :: * -> Constraint) (x :: *) =
       ( c (XFieldOcc  x)
       , c (XXFieldOcc x)
       )

-- ---------------------------------------------------------------------

type family XUnambiguous        x
type family XAmbiguous          x
type family XXAmbiguousFieldOcc x

type ForallXAmbiguousFieldOcc (c :: * -> Constraint) (x :: *) =
       ( c (XUnambiguous        x)
       , c (XAmbiguous          x)
       , c (XXAmbiguousFieldOcc x)
       )

-- ---------------------------------------------------------------------

-- | The 'SourceText' fields have been moved into the extension fields, thus
-- placing a requirement in the extension field to contain a 'SourceText' so
-- that the pretty printing and round tripping of source can continue to
-- operate.
--
-- The 'HasSourceText' class captures this requirement for the relevant fields.
class HasSourceText a where
  -- Provide setters to mimic existing constructors
  noSourceText  :: a
  sourceText    :: String -> a

  setSourceText :: SourceText -> a
  getSourceText :: a -> SourceText

-- | Provide a summary constraint that lists all the extension points requiring
-- the 'HasSourceText' class, so that it can be changed in one place as the
-- named extensions change throughout the AST.
type SourceTextX x =
  ( HasSourceText (XHsChar x)
  , HasSourceText (XHsCharPrim x)
  , HasSourceText (XHsString x)
  , HasSourceText (XHsStringPrim x)
  , HasSourceText (XHsIntPrim x)
  , HasSourceText (XHsWordPrim x)
  , HasSourceText (XHsInt64Prim x)
  , HasSourceText (XHsWord64Prim x)
  , HasSourceText (XHsInteger x)
  )


-- |  'SourceText' trivially implements 'HasSourceText'
instance HasSourceText SourceText where
  noSourceText    = NoSourceText
  sourceText s    = SourceText s

  setSourceText s = s
  getSourceText a = a


-- ----------------------------------------------------------------------
-- | Conversion of annotations from one type index to another. This is required
-- where the AST is converted from one pass to another, and the extension values
-- need to be brought along if possible. So for example a 'SourceText' is
-- converted via 'id', but needs a type signature to keep the type checker
-- happy.
class Convertable a b  | a -> b where
  convert :: a -> b

instance Convertable a a where
  convert = id

-- | A constraint capturing all the extension points that can be converted via
-- @instance Convertable a a@
type ConvertIdX a b =
  (XHsDoublePrim a ~ XHsDoublePrim b,
   XHsFloatPrim a ~ XHsFloatPrim b,
   XHsRat a ~ XHsRat b,
   XHsInteger a ~ XHsInteger b,
   XHsWord64Prim a ~ XHsWord64Prim b,
   XHsInt64Prim a ~ XHsInt64Prim b,
   XHsWordPrim a ~ XHsWordPrim b,
   XHsIntPrim a ~ XHsIntPrim b,
   XHsInt a ~ XHsInt b,
   XHsStringPrim a ~ XHsStringPrim b,
   XHsString a ~ XHsString b,
   XHsCharPrim a ~ XHsCharPrim b,
   XHsChar a ~ XHsChar b,
   XXLit a ~ XXLit b)

-- ----------------------------------------------------------------------

-- | Provide a summary constraint that gives all am Outputable constraint to
-- extension points needing one
type OutputableX p =
  ( Outputable (XXPat p)
  , Outputable (XXPat GhcRn)
  , Outputable (XSigPat p)
  , Outputable (XSigPat GhcRn)
  , Outputable (XXLit p)
  , Outputable (XXOverLit p)
  , Outputable (XXType p)
  )
-- TODO: Should OutputableX be included in OutputableBndrId?

-- ----------------------------------------------------------------------

--
type DataId p =
  ( Data p

  , ForallXHsLit Data p
  , ForallXPat   Data p

  -- AZ: The following ForAllXXXX shoulbe be unnecessary? Driven by ValBindsOut
  -- , ForallXPat Data (GhcPass 'Parsed)
  , ForallXPat Data (GhcPass 'Renamed)
  -- , ForallXPat Data (GhcPass 'Typechecked)
  , ForallXType Data (GhcPass 'Renamed)

  , ForallXOverLit           Data p
  , ForallXType              Data p
  , ForallXTyVarBndr         Data p
  , ForallXAppType           Data p
  , ForallXFieldOcc          Data p
  , ForallXAmbiguousFieldOcc Data p

  , Data (NameOrRdrName (IdP p))

  , Data (IdP p)
  , Data (PostRn p (IdP p))
  , Data (PostRn p (Located Name))
  , Data (PostRn p Bool)
  , Data (PostRn p Fixity)
  , Data (PostRn p NameSet)
  , Data (PostRn p [Name])

  , Data (PostTc p (IdP p))
  , Data (PostTc p Coercion)
  , Data (PostTc p ConLike)
  , Data (PostTc p HsWrapper)
  , Data (PostTc p Type)
  , Data (PostTc p [ConLike])
  , Data (PostTc p [Type])
  )

type DataIdLR pL pR =
  ( DataId pL
  , DataId pR
  , ForallXValBindsLR Data pL pR
  )

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  ( OutputableBndr (NameOrRdrName (IdP id))
  , OutputableBndr (IdP id)
  , OutputableX id
  )
