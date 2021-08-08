%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

module HsLit where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr( SyntaxExpr, pprExpr )
import BasicTypes ( FractionalLit(..) )
import Type       ( Type )
import Outputable
import FastString
import PlaceHolder ( PostTc,PostRn,DataId )
import Lexer       ( SourceText )

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )
\end{code}





%************************************************************************
%*                                                                      *
\subsection[HsLit]{Literals}
%*                                                                      *
%************************************************************************


\begin{code}
-- Note [literal source text] for SourceText fields in the following
data HsLit
  = HsChar          SourceText Char        -- Character
  | HsCharPrim      SourceText Char        -- Unboxed character
  | HsString        SourceText FastString  -- String
  | HsStringPrim    SourceText ByteString  -- Packed bytes
  | HsInt           SourceText Integer     -- Genuinely an Int; arises from
                                       --     TcGenDeriv, and from TRANSLATION
  | HsIntPrim       SourceText Integer     -- literal Int#
  | HsWordPrim      SourceText Integer     -- literal Word#
  | HsInt64Prim     SourceText Integer     -- literal Int64#
  | HsWord64Prim    SourceText Integer     -- literal Word64#
  | HsInteger       SourceText Integer Type -- Genuinely an integer; arises only
                                          --   from TRANSLATION (overloaded
                                          --   literals are done with HsOverLit)
  | HsRat           FractionalLit Type -- Genuinely a rational; arises only from
                                       --   TRANSLATION (overloaded literals are
                                       --   done with HsOverLit)
  | HsFloatPrim     FractionalLit      -- Unboxed Float
  | HsDoublePrim    FractionalLit      -- Unboxed Double
  deriving (Data, Typeable)

instance Eq HsLit where
  (HsChar _ x1)       == (HsChar _ x2)       = x1==x2
  (HsCharPrim _ x1)   == (HsCharPrim _ x2)   = x1==x2
  (HsString _ x1)     == (HsString _ x2)     = x1==x2
  (HsStringPrim _ x1) == (HsStringPrim _ x2) = x1==x2
  (HsInt _ x1)        == (HsInt _ x2)        = x1==x2
  (HsIntPrim _ x1)    == (HsIntPrim _ x2)    = x1==x2
  (HsWordPrim _ x1)   == (HsWordPrim _ x2)   = x1==x2
  (HsInt64Prim _ x1)  == (HsInt64Prim _ x2)  = x1==x2
  (HsWord64Prim _ x1) == (HsWord64Prim _ x2) = x1==x2
  (HsInteger _ x1 _)  == (HsInteger _ x2 _)  = x1==x2
  (HsRat x1 _)        == (HsRat x2 _)        = x1==x2
  (HsFloatPrim x1)    == (HsFloatPrim x2)    = x1==x2
  (HsDoublePrim x1)   == (HsDoublePrim x2)   = x1==x2
  _                   == _                   = False

data HsOverLit id       -- An overloaded literal
  = OverLit {
        ol_val :: OverLitVal,
        ol_rebindable :: PostRn id Bool, -- Note [ol_rebindable]
        ol_witness :: SyntaxExpr id,     -- Note [Overloaded literal witnesses]
        ol_type :: PostTc id Type }
  deriving (Typeable)
deriving instance (DataId id) => Data (HsOverLit id)

-- Note [literal source text] for SourceText fields in the following
data OverLitVal
  = HsIntegral   !SourceText !Integer    -- Integer-looking literals;
  | HsFractional !FractionalLit          -- Frac-looking literals
  | HsIsString   !SourceText !FastString -- String-looking literals
  deriving (Data, Typeable)

overLitType :: HsOverLit a -> PostTc a Type
overLitType = ol_type
\end{code}

Note [literal source text]
~~~~~~~~~~~~~~~~~~~~~~~~~~

The lexer/parser converts literals from their original source text
versions to an appropriate internal representation. This is a problem
for tools doing source to source conversions, so the original source
text is stored in literals where this can occur.

Motivating examples for HsLit

  HsChar          '\n', '\x20`
  HsCharPrim      '\x41`#
  HsString        "\x20\x41" == " A"
  HsStringPrim    "\x20"#
  HsInt           001
  HsIntPrim       002#
  HsWordPrim      003##
  HsInt64Prim     004##
  HsWord64Prim    005##
  HsInteger       006

For OverLitVal

  HsIntegral      003,0x001
  HsIsString      "\x41nd"





Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

  False iff ol_witness is the standard one
  True  iff ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one

Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Before* type checking, the SyntaxExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
        (fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desguarar made the application.

The PostTcType in each branch records the type the overload literal is
found to have.

\begin{code}
-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit id) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral _ i1)   == (HsIntegral _ i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

instance Ord (HsOverLit id) where
  compare (OverLit {ol_val=val1}) (OverLit {ol_val=val2}) = val1 `compare` val2

instance Ord OverLitVal where
  compare (HsIntegral _ i1)   (HsIntegral _ i2)   = i1 `compare` i2
  compare (HsIntegral _ _)    (HsFractional _)    = LT
  compare (HsIntegral _ _)    (HsIsString _ _)    = LT
  compare (HsFractional f1)   (HsFractional f2)   = f1 `compare` f2
  compare (HsFractional _)    (HsIntegral _ _)    = GT
  compare (HsFractional _)    (HsIsString _ _)    = LT
  compare (HsIsString _ s1)   (HsIsString _ s2)   = s1 `compare` s2
  compare (HsIsString _ _)    (HsIntegral _ _)    = GT
  compare (HsIsString _ _)    (HsFractional _)    = GT
\end{code}

\begin{code}
instance Outputable HsLit where
        -- Use "show" because it puts in appropriate escapes
    ppr (HsChar _ c)       = pprHsChar c
    ppr (HsCharPrim _ c)   = pprHsChar c <> char '#'
    ppr (HsString _ s)     = pprHsString s
    ppr (HsStringPrim _ s) = pprHsBytes s <> char '#'
    ppr (HsInt _ i)        = integer i
    ppr (HsInteger _ i _)  = integer i
    ppr (HsRat f _)        = ppr f
    ppr (HsFloatPrim f)    = ppr f <> char '#'
    ppr (HsDoublePrim d)   = ppr d <> text "##"
    ppr (HsIntPrim _ i)    = integer i  <> char '#'
    ppr (HsWordPrim _ w)   = integer w  <> text "##"
    ppr (HsInt64Prim _ i)  = integer i  <> text "L#"
    ppr (HsWord64Prim _ w) = integer w  <> text "L##"

-- in debug mode, print the expression that it's resolved to, too
instance OutputableBndr id => Outputable (HsOverLit id) where
  ppr (OverLit {ol_val=val, ol_witness=witness})
        = ppr val <+> (ifPprDebug (parens (pprExpr witness)))

instance Outputable OverLitVal where
  ppr (HsIntegral _ i)   = integer i
  ppr (HsFractional f)   = ppr f
  ppr (HsIsString _ s)   = pprHsString s
\end{code}
