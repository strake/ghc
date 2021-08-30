{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

#include "lens.h"

module GHC.Core.Map (
   -- * Maps over Core expressions
   CoreMap,
   -- * Maps over 'Type's
   TypeMap,
   LooseTypeMap,
   -- ** With explicit scoping
   CmEnv, lookupCME, extendTypeMapWithScope, lookupTypeMapWithScope,
   mkDeBruijnContext,
   -- * Maps over 'Maybe' values
   MaybeMap,
   -- * Maps over 'List' values
   ListMap,
   -- * Maps over 'Literal's
   LiteralMap,
   -- * Map for compressing leaves. See Note [Compressed TrieMap]
   GenMap,
   -- * 'TrieMap' class
   TrieMap(..), insertTM, deleteTM,
   lkDFreeVar, xtDFreeVar,
   lkDNamed, xtDNamed,
   (|>>),
 ) where

import GHC.Prelude

import GHC.Data.TrieMap
import GHC.Core
import GHC.Core.Coercion
import GHC.Types.Name
import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Types.Var
import GHC.Data.FastString(FastString)

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import GHC.Types.Var.Env
import GHC.Types.Name.Env
import Control.Category( (>>>) )
import Control.Monad( (>=>) )
import Data.Foldable( toList )

{-
This module implements TrieMaps over Core related data structures
like CoreExpr or Type. It is built on the Tries from the TrieMap
module.

The code is very regular and boilerplate-like, but there is
some neat handling of *binders*.  In effect they are deBruijn
numbered on the fly.


-}

----------------------
-- Recall that
--   Control.Monad.(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c

-- NB: Be careful about RULES and type families (#5821).  So we should make sure
-- to specify @Key TypeMapX@ (and not @DeBruijn Type@, the reduced form)

-- The CoreMap makes heavy use of GenMap. However the CoreMap Types are not
-- known when defining GenMap so we can only specialize them here.

{-# SPECIALIZE lkG :: Key TypeMapX     -> TypeMapG a     -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoercionMapX -> CoercionMapG a -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoreMapX     -> CoreMapG a     -> Maybe a #-}

{-# SPECIALIZE xtG :: Key TypeMapX     -> XT a -> TypeMapG a -> TypeMapG a #-}
{-# SPECIALIZE xtG :: Key CoercionMapX -> XT a -> CoercionMapG a -> CoercionMapG a #-}
{-# SPECIALIZE xtG :: Key CoreMapX     -> XT a -> CoreMapG a -> CoreMapG a #-}


{-
************************************************************************
*                                                                      *
                   CoreMap
*                                                                      *
************************************************************************
-}

lkDNamed :: NamedThing n => n -> DNameEnv a -> Maybe a
lkDNamed n env = lookupDNameEnv env (getName n)

xtDNamed :: NamedThing n => n -> XT a -> DNameEnv a -> DNameEnv a
xtDNamed tc f m = alterDNameEnv f m (getName tc)


{-
Note [Binders]
~~~~~~~~~~~~~~
 * In general we check binders as late as possible because types are
   less likely to differ than expression structure.  That's why
      cm_lam :: CoreMapG (TypeMapG a)
   rather than
      cm_lam :: TypeMapG (CoreMapG a)

 * We don't need to look at the type of some binders, notably
     - the case binder in (Case _ b _ _)
     - the binders in an alternative
   because they are totally fixed by the context

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* For a key (Case e b ty (alt:alts))  we don't need to look the return type
  'ty', because every alternative has that type.

* For a key (Case e b ty []) we MUST look at the return type 'ty', because
  otherwise (Case (error () "urk") _ Int  []) would compare equal to
            (Case (error () "urk") _ Bool [])
  which is utterly wrong (#6097)

We could compare the return type regardless, but the wildly common case
is that it's unnecessary, so we have two fields (cm_case and cm_ecase)
for the two possibilities.  Only cm_ecase looks at the type.

See also Note [Empty case alternatives] in GHC.Core.
-}

-- | @CoreMap a@ is a map from 'CoreExpr' to @a@.  If you are a client, this
-- is the type you want.
newtype CoreMap a = CoreMap (CoreMapG a)
  deriving (Foldable, Functor, Traversable)

instance TrieMap CoreMap where
    type Key CoreMap = CoreExpr
    emptyTM = CoreMap emptyTM
    lookupTM k (CoreMap m) = lookupTM (deBruijnize k) m
    alterTM k f (CoreMap m) = CoreMap (alterTM (deBruijnize k) f m)

-- | @CoreMapG a@ is a map from @DeBruijn CoreExpr@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'CoreMap'
-- inside another 'TrieMap', this is the type you want.
type CoreMapG = GenMap CoreMapX

-- | @CoreMapX a@ is the base map from @DeBruijn CoreExpr@ to @a@, but without
-- the 'GenMap' optimization.
data CoreMapX a
  = CM { cm_var   :: VarMap a
       , cm_lit   :: LiteralMap a
       , cm_co    :: CoercionMapG a
       , cm_type  :: TypeMapG a
       , cm_cast  :: CoreMapG (CoercionMapG a)
       , cm_tick  :: CoreMapG (TickishMap a)
       , cm_app   :: CoreMapG (CoreMapG a)
       , cm_lam   :: CoreMapG (BndrMap a)    -- Note [Binders]
       , cm_letn  :: CoreMapG (CoreMapG (BndrMap a))
       , cm_letr  :: ListMap CoreMapG (CoreMapG (ListMap BndrMap a))
       , cm_case  :: CoreMapG (ListMap AltMap a)
       , cm_ecase :: CoreMapG (TypeMapG a)    -- Note [Empty case alternatives]
     }
  deriving (Foldable, Functor, Traversable)

LENS_FIELD(cm_varL, cm_var)
LENS_FIELD(cm_litL, cm_lit)
LENS_FIELD(cm_coL, cm_co)
LENS_FIELD(cm_typeL, cm_type)
LENS_FIELD(cm_castL, cm_cast)
LENS_FIELD(cm_tickL, cm_tick)
LENS_FIELD(cm_appL, cm_app)
LENS_FIELD(cm_lamL, cm_lam)
LENS_FIELD(cm_letnL, cm_letn)
LENS_FIELD(cm_letrL, cm_letr)
LENS_FIELD(cm_caseL, cm_case)
LENS_FIELD(cm_ecaseL, cm_ecase)

instance Eq (DeBruijn CoreExpr) where
  D env1 e1 == D env2 e2 = go e1 e2 where
    go (Var v1) (Var v2) = case (lookupCME env1 v1, lookupCME env2 v2) of
                            (Just b1, Just b2) -> b1 == b2
                            (Nothing, Nothing) -> v1 == v2
                            _ -> False
    go (Lit lit1)    (Lit lit2)      = lit1 == lit2
    go (Type t1)    (Type t2)        = D env1 t1 == D env2 t2
    go (Coercion co1) (Coercion co2) = D env1 co1 == D env2 co2
    go (Cast e1 co1) (Cast e2 co2) = D env1 co1 == D env2 co2 && go e1 e2
    go (App f1 a1)   (App f2 a2)   = go f1 f2 && go a1 a2
    -- This seems a bit dodgy, see 'eqTickish'
    go (Tick n1 e1)  (Tick n2 e2)  = n1 == n2 && go e1 e2

    go (Lam b1 e1)  (Lam b2 e2)
      =  D env1 (varType b1) == D env2 (varType b2)
      && D (extendCME env1 b1) e1 == D (extendCME env2 b2) e2

    go (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      =  go r1 r2
      && D (extendCME env1 v1) e1 == D (extendCME env2 v2) e2

    go (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = equalLength ps1 ps2
      && D env1' rs1 == D env2' rs2
      && D env1' e1  == D env2' e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env1' = foldl' extendCME env1 bs1
        env2' = foldl' extendCME env2 bs2

    go (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives]
      = null a2 && go e1 e2 && D env1 t1 == D env2 t2
      | otherwise
      =  go e1 e2 && D (extendCME env1 b1) a1 == D (extendCME env2 b2) a2

    go _ _ = False

emptyE :: CoreMapX a
emptyE = CM { cm_var = emptyTM, cm_lit = emptyTM
            , cm_co = emptyTM, cm_type = emptyTM
            , cm_cast = emptyTM, cm_app = emptyTM
            , cm_lam = emptyTM, cm_letn = emptyTM
            , cm_letr = emptyTM, cm_case = emptyTM
            , cm_ecase = emptyTM, cm_tick = emptyTM }

instance TrieMap CoreMapX where
   type Key CoreMapX = DeBruijn CoreExpr
   emptyTM  = emptyE
   lookupTM = lkE
   alterTM  = xtE

--------------------------
instance Outputable a => Outputable (CoreMap a) where
  ppr m = text "CoreMap elts" <+> ppr (toList m)

-------------------------
-- lkE: lookup in trie for expressions
lkE :: DeBruijn CoreExpr -> CoreMapX a -> Maybe a
lkE (D env expr) = go expr
  where
    go (Var v)              = cm_var  >>> lkVar env v
    go (Lit l)              = cm_lit  >>> lookupTM l
    go (Type t)             = cm_type >>> lkG (D env t)
    go (Coercion c)         = cm_co   >>> lkG (D env c)
    go (Cast e c)           = cm_cast >>> lkG (D env e) >=> lkG (D env c)
    go (Tick tickish e)     = cm_tick >>> lkG (D env e) >=> lookupTM tickish
    go (App e1 e2)          = cm_app  >>> lkG (D env e2) >=> lkG (D env e1)
    go (Lam v e)            = cm_lam  >>> lkG (D (extendCME env v) e) >=> lkBndr env v
    go (Let (NonRec b r) e) = cm_letn >>> lkG (D env r)
                              >=> lkG (D (extendCME env b) e) >=> lkBndr env b
    go (Let (Rec prs) e)    = let (bndrs,rhss) = unzip prs
                                  env1 = foldl' extendCME env bndrs
                              in cm_letr
                                 >>> lkList (lkG . D env1) rhss
                                 >=> lkG (D env1 e)
                                 >=> lkList (lkBndr env1) bndrs
    go (Case e b ty as)     -- See Note [Empty case alternatives]
               | null as    = cm_ecase >>> lkG (D env e) >=> lkG (D env ty)
               | otherwise  = cm_case >>> lkG (D env e)
                              >=> lkList (lkA (extendCME env b)) as

xtE :: DeBruijn CoreExpr -> XT a -> CoreMapX a -> CoreMapX a
xtE (D env (Var v))              f = cm_varL `over` xtVar env v f
xtE (D env (Type t))             f = cm_typeL `over` xtG (D env t) f
xtE (D env (Coercion c))         f = cm_coL `over` xtG (D env c) f
xtE (D _   (Lit l))              f = cm_litL `over` alterTM l f
xtE (D env (Cast e c))           f = over cm_castL $ xtG (D env e) |>> xtG (D env c) f
xtE (D env (Tick t e))           f = over cm_tickL $ xtG (D env e) |>> Map.alter f t
xtE (D env (App e1 e2))          f = over cm_appL $ xtG (D env e2) |>> xtG (D env e1) f
xtE (D env (Lam v e))            f = over cm_lamL $ xtG (D (extendCME env v) e) |>> xtBndr env v f
xtE (D env (Let (NonRec b r) e)) f = over cm_letnL $ xtG (D (extendCME env b) e) |>> xtG (D env r) |>> xtBndr env b f
xtE (D env (Let (Rec prs) e))    f = let (bndrs,rhss) = unzip prs
                                         env1 = foldl' extendCME env bndrs
                                     in over cm_letrL $ xtList (xtG . D env1) rhss
                                                 |>> xtG (D env1 e)
                                                 |>> xtList (xtBndr env1) bndrs f
xtE (D env (Case e b ty as))     f
                     | null as   = over cm_ecaseL $ xtG (D env e) |>> xtG (D env ty) f
                     | otherwise = over cm_caseL $ xtG (D env e)
                                                 |>> let env1 = extendCME env b
                                                     in xtList (xtA env1) as f

-- TODO: this seems a bit dodgy, see 'eqTickish'
type TickishMap a = Map.Map (Tickish Id) a

------------------------
data AltMap a   -- A single alternative
  = AM { am_deflt :: CoreMapG a
       , am_data  :: DNameEnv (CoreMapG a)
       , am_lit   :: LiteralMap (CoreMapG a) }
  deriving (Foldable, Functor, Traversable)

LENS_FIELD(am_defltL, am_deflt)
LENS_FIELD(am_dataL, am_data)
LENS_FIELD(am_litL, am_lit)

instance TrieMap AltMap where
   type Key AltMap = CoreAlt
   emptyTM  = AM { am_deflt = emptyTM
                 , am_data = emptyDNameEnv
                 , am_lit  = emptyTM }
   lookupTM = lkA emptyCME
   alterTM  = xtA emptyCME

instance Eq (DeBruijn CoreAlt) where
  D env1 a1 == D env2 a2 = go a1 a2 where
    go (DEFAULT, _, rhs1) (DEFAULT, _, rhs2)
        = D env1 rhs1 == D env2 rhs2
    go (LitAlt lit1, _, rhs1) (LitAlt lit2, _, rhs2)
        = lit1 == lit2 && D env1 rhs1 == D env2 rhs2
    go (DataAlt dc1, bs1, rhs1) (DataAlt dc2, bs2, rhs2)
        = dc1 == dc2 &&
          D (foldl' extendCME env1 bs1) rhs1 == D (foldl' extendCME env2 bs2) rhs2
    go _ _ = False

lkA :: CmEnv -> CoreAlt -> AltMap a -> Maybe a
lkA env (DEFAULT,    _, rhs)  = am_deflt >>> lkG (D env rhs)
lkA env (LitAlt lit, _, rhs)  = am_lit >>> lookupTM lit >=> lkG (D env rhs)
lkA env (DataAlt dc, bs, rhs) = am_data >>> lkDNamed dc >=> lkG (D (foldl' extendCME env bs) rhs)

xtA :: CmEnv -> CoreAlt -> XT a -> AltMap a -> AltMap a
xtA env (DEFAULT, _, rhs)    f = over am_defltL $ xtG (D env rhs) f
xtA env (LitAlt l, _, rhs)   f = over am_litL $ alterTM l |>> xtG (D env rhs) f
xtA env (DataAlt d, bs, rhs) f = over am_dataL $ xtDNamed d |>> xtG (D (foldl' extendCME env bs) rhs) f

{-
************************************************************************
*                                                                      *
                   Coercions
*                                                                      *
************************************************************************
-}

-- We should really never care about the contents of a coercion. Instead,
-- just look up the coercion's type.
newtype CoercionMap a = CoercionMap (CoercionMapG a)
  deriving (Foldable, Functor, Traversable)

instance TrieMap CoercionMap where
   type Key CoercionMap = Coercion
   emptyTM                     = CoercionMap emptyTM
   lookupTM k  (CoercionMap m) = lookupTM (deBruijnize k) m
   alterTM k f (CoercionMap m) = CoercionMap (alterTM (deBruijnize k) f m)

type CoercionMapG = GenMap CoercionMapX
newtype CoercionMapX a = CoercionMapX (TypeMapX a)
  deriving (Foldable, Functor, Traversable)

instance TrieMap CoercionMapX where
  type Key CoercionMapX = DeBruijn Coercion
  emptyTM = CoercionMapX emptyTM
  lookupTM = lkC
  alterTM  = xtC

instance Eq (DeBruijn Coercion) where
  D env1 co1 == D env2 co2 = D env1 (coercionType co1) == D env2 (coercionType co2)

lkC :: DeBruijn Coercion -> CoercionMapX a -> Maybe a
lkC (D env co) (CoercionMapX core_tm) = lkT (D env $ coercionType co) core_tm

xtC :: DeBruijn Coercion -> XT a -> CoercionMapX a -> CoercionMapX a
xtC (D env co) f (CoercionMapX m) = CoercionMapX (xtT (D env $ coercionType co) f m)

{-
************************************************************************
*                                                                      *
                   Types
*                                                                      *
************************************************************************
-}

-- | @TypeMapG a@ is a map from @DeBruijn Type@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'TypeMap'
-- inside another 'TrieMap', this is the type you want. Note that this
-- lookup does not do a kind-check. Thus, all keys in this map must have
-- the same kind. Also note that this map respects the distinction between
-- @Type@ and @Constraint@, despite the fact that they are equivalent type
-- synonyms in Core.
type TypeMapG = GenMap TypeMapX

-- | @TypeMapX a@ is the base map from @DeBruijn Type@ to @a@, but without the
-- 'GenMap' optimization.
data TypeMapX a
  = TM { tm_var    :: VarMap a
       , tm_app    :: TypeMapG (TypeMapG a)
       , tm_tycon  :: DNameEnv a
       , tm_forall :: TypeMapG (BndrMap a) -- See Note [Binders]
       , tm_tylit  :: TyLitMap a
       , tm_coerce :: Maybe a
       }
  deriving (Foldable, Functor, Traversable)
    -- Note that there is no tyconapp case; see Note [Equality on AppTys] in GHC.Core.Type

LENS_FIELD(tm_varL, tm_var)
LENS_FIELD(tm_appL, tm_app)
LENS_FIELD(tm_tyconL, tm_tycon)
LENS_FIELD(tm_forallL, tm_forall)
LENS_FIELD(tm_tylitL, tm_tylit)
LENS_FIELD(tm_coerceL, tm_coerce)

-- | Squeeze out any synonyms, and change TyConApps to nested AppTys. Why the
-- last one? See Note [Equality on AppTys] in "GHC.Core.Type"
--
-- Note, however, that we keep Constraint and Type apart here, despite the fact
-- that they are both synonyms of TYPE 'LiftedRep (see #11715).
trieMapView :: Type -> Maybe Type
trieMapView ty
  -- First check for TyConApps that need to be expanded to
  -- AppTy chains.
  | Just (tc, tys@(_:_)) <- tcSplitTyConApp_maybe ty
  = Just $ foldl' AppTy (TyConApp tc []) tys

  -- Then resolve any remaining nullary synonyms.
  | Just ty' <- tcView ty = Just ty'
trieMapView _ = Nothing

instance TrieMap TypeMapX where
   type Key TypeMapX = DeBruijn Type
   emptyTM  = emptyT
   lookupTM = lkT
   alterTM  = xtT

instance Eq (DeBruijn Type) where
  env_t@(D env t) == env_t'@(D env' t')
    | Just new_t  <- tcView t  = D env new_t == env_t'
    | Just new_t' <- tcView t' = env_t       == D env' new_t'
    | otherwise
    = case (t, t') of
        (CastTy t1 _, _)  -> D env t1 == D env t'
        (_, CastTy t1' _) -> D env t  == D env t1'

        (TyVarTy v, TyVarTy v')
            -> case (lookupCME env v, lookupCME env' v') of
                (Just bv, Just bv') -> bv == bv'
                (Nothing, Nothing)  -> v == v'
                _ -> False
                -- See Note [Equality on AppTys] in GHC.Core.Type
        (AppTy t1 t2, s) | Just (t1', t2') <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (s, AppTy t1' t2') | Just (t1, t2) <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (FunTy _ t1 t2, FunTy _ t1' t2')
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (TyConApp tc tys, TyConApp tc' tys')
            -> tc == tc' && D env tys == D env' tys'
        (LitTy l, LitTy l')
            -> l == l'
        (ForAllTy (Bndr tv _) ty, ForAllTy (Bndr tv' _) ty')
            -> D env (varType tv)      == D env' (varType tv') &&
               D (extendCME env tv) ty == D (extendCME env' tv') ty'
        (CoercionTy {}, CoercionTy {})
            -> True
        _ -> False

instance {-# OVERLAPPING #-}
         Outputable a => Outputable (TypeMapG a) where
  ppr m = text "TypeMap elts" <+> ppr (toList m)

emptyT :: TypeMapX a
emptyT = TM { tm_var  = emptyTM
            , tm_app  = emptyTM
            , tm_tycon  = emptyDNameEnv
            , tm_forall = emptyTM
            , tm_tylit  = emptyTyLitMap
            , tm_coerce = Nothing }

-----------------
lkT :: DeBruijn Type -> TypeMapX a -> Maybe a
lkT (D env ty) m = go ty m
  where
    go ty | Just ty' <- trieMapView ty = go ty'
    go (TyVarTy v)                 = tm_var    >>> lkVar env v
    go (AppTy t1 t2)               = tm_app    >>> lkG (D env t1)
                                               >=> lkG (D env t2)
    go (TyConApp tc [])            = tm_tycon  >>> lkDNamed tc
    go ty@(TyConApp _ (_:_))       = pprPanic "lkT TyConApp" (ppr ty)
    go (LitTy l)                   = tm_tylit  >>> lkTyLit l
    go (ForAllTy (Bndr tv _) ty)   = tm_forall >>> lkG (D (extendCME env tv) ty)
                                               >=> lkBndr env tv
    go ty@(FunTy {})               = pprPanic "lkT FunTy" (ppr ty)
    go (CastTy t _)                = go t
    go (CoercionTy {})             = tm_coerce

-----------------
xtT :: DeBruijn Type -> XT a -> TypeMapX a -> TypeMapX a
xtT (D env ty) f | Just ty' <- trieMapView ty = xtT (D env ty') f

xtT (D env (TyVarTy v))       f = over tm_varL $ xtVar env v f
xtT (D env (AppTy t1 t2))     f = over tm_appL $ xtG (D env t1) |>> xtG (D env t2) f
xtT (D _   (TyConApp tc []))  f = over tm_tyconL $ xtDNamed tc f
xtT (D _   (LitTy l))         f = over tm_tylitL $ xtTyLit l f
xtT (D env (CastTy t _))      f = xtT (D env t) f
xtT (D _   (CoercionTy {}))   f = over tm_coerceL f
xtT (D env (ForAllTy (Bndr tv _) ty)) f = over tm_forallL $
    xtG (D (extendCME env tv) ty) |>> xtBndr env tv f
xtT (D _   ty@(TyConApp _ (_:_))) _ = pprPanic "xtT TyConApp" (ppr ty)
xtT (D _   ty@(FunTy {}))         _ = pprPanic "xtT FunTy" (ppr ty)

------------------------
data TyLitMap a = TLM { tlm_number :: Map.Map Integer a
                      , tlm_string :: Map.Map FastString a
                      }
  deriving (Foldable, Functor, Traversable)

instance TrieMap TyLitMap where
   type Key TyLitMap = TyLit
   emptyTM  = emptyTyLitMap
   lookupTM = lkTyLit
   alterTM  = xtTyLit

emptyTyLitMap :: TyLitMap a
emptyTyLitMap = TLM { tlm_number = Map.empty, tlm_string = Map.empty }

LENS_FIELD(tlm_numberL, tlm_number)
LENS_FIELD(tlm_stringL, tlm_string)

lkTyLit :: TyLit -> TyLitMap a -> Maybe a
lkTyLit = \ case
    NumTyLit n -> tlm_number >>> Map.lookup n
    StrTyLit n -> tlm_string >>> Map.lookup n

xtTyLit :: TyLit -> XT a -> TyLitMap a -> TyLitMap a
xtTyLit l f = case l of
    NumTyLit n -> over tlm_numberL $ Map.alter f n
    StrTyLit n -> over tlm_stringL $ Map.alter f n

-------------------------------------------------
-- | @TypeMap a@ is a map from 'Type' to @a@.  If you are a client, this
-- is the type you want. The keys in this map may have different kinds.
newtype TypeMap a = TypeMap (TypeMapG (TypeMapG a))
  deriving (Foldable, Functor, Traversable)

lkTT :: DeBruijn Type -> TypeMap a -> Maybe a
lkTT (D env ty) (TypeMap m) = lkG (D env $ typeKind ty) m >>= lkG (D env ty)

xtTT :: DeBruijn Type -> XT a -> TypeMap a -> TypeMap a
xtTT (D env ty) f (TypeMap m) = TypeMap (xtG (D env $ typeKind ty) |>> xtG (D env ty) f $ m)

-- Below are some client-oriented functions which operate on 'TypeMap'.

instance TrieMap TypeMap where
    type Key TypeMap = Type
    emptyTM = TypeMap emptyTM
    lookupTM k = lkTT (deBruijnize k)
    alterTM k = xtTT (deBruijnize k)

lookupTypeMapWithScope :: TypeMap a -> CmEnv -> Type -> Maybe a
lookupTypeMapWithScope m cm t = lkTT (D cm t) m

-- | Extend a 'TypeMap' with a type in the given context.
-- @extendTypeMapWithScope m (mkDeBruijnContext [a,b,c]) t v@ is equivalent to
-- @extendTypeMap m (forall a b c. t) v@, but allows reuse of the context over
-- multiple insertions.
extendTypeMapWithScope :: TypeMap a -> CmEnv -> Type -> a -> TypeMap a
extendTypeMapWithScope m cm t v = xtTT (D cm t) (const (Just v)) m

-- | Construct a deBruijn environment with the given variables in scope.
-- e.g. @mkDeBruijnEnv [a,b,c]@ constructs a context @forall a b c.@
mkDeBruijnContext :: [Var] -> CmEnv
mkDeBruijnContext = foldl' extendCME emptyCME

-- | A 'LooseTypeMap' doesn't do a kind-check. Thus, when lookup up (g t),
-- you'll find entries inserted under (t), even if (g) is non-reflexive.
newtype LooseTypeMap a = LooseTypeMap (TypeMapG a)
  deriving (Foldable, Functor, Traversable)

instance TrieMap LooseTypeMap where
  type Key LooseTypeMap = Type
  emptyTM = LooseTypeMap emptyTM
  lookupTM k (LooseTypeMap m) = lookupTM (deBruijnize k) m
  alterTM k f (LooseTypeMap m) = LooseTypeMap (alterTM (deBruijnize k) f m)

{-
************************************************************************
*                                                                      *
                   Variables
*                                                                      *
************************************************************************
-}

type BoundVar = Int  -- Bound variables are deBruijn numbered
type BoundVarMap = IntMap.IntMap

data CmEnv = CME { cme_next :: !BoundVar
                 , cme_env  :: VarEnv BoundVar }

emptyCME :: CmEnv
emptyCME = CME { cme_next = 0, cme_env = emptyVarEnv }

extendCME :: CmEnv -> Var -> CmEnv
extendCME (CME { cme_next = bv, cme_env = env }) v
  = CME { cme_next = bv+1, cme_env = extendVarEnv env v bv }

lookupCME :: CmEnv -> Var -> Maybe BoundVar
lookupCME (CME { cme_env = env }) = lookupVarEnv env

-- | @DeBruijn a@ represents @a@ modulo alpha-renaming.  This is achieved
-- by equipping the value with a 'CmEnv', which tracks an on-the-fly deBruijn
-- numbering.  This allows us to define an 'Eq' instance for @DeBruijn a@, even
-- if this was not (easily) possible for @a@.  Note: we purposely don't
-- export the constructor.  Make a helper function if you find yourself
-- needing it.
data DeBruijn a = D CmEnv a
  deriving (Functor)

-- | Synthesizes a @DeBruijn a@ from an @a@, by assuming that there are no
-- bound binders (an empty 'CmEnv').  This is usually what you want if there
-- isn't already a 'CmEnv' in scope.
deBruijnize :: a -> DeBruijn a
deBruijnize = D emptyCME

instance Eq (DeBruijn a) => Eq (DeBruijn [a]) where
    D _   []     == D _    []       = True
    D env (x:xs) == D env' (x':xs') = D env x  == D env' x' &&
                                      D env xs == D env' xs'
    _            == _               = False

--------- Variable binders -------------

-- | A 'BndrMap' is a 'TypeMapG' which allows us to distinguish between
-- binding forms whose binders have different types.  For example,
-- if we are doing a 'TrieMap' lookup on @\(x :: Int) -> ()@, we should
-- not pick up an entry in the 'TrieMap' for @\(x :: Bool) -> ()@:
-- we can disambiguate this by matching on the type (or kind, if this
-- a binder in a type) of the binder.
type BndrMap = TypeMapG

-- Note [Binders]
-- ~~~~~~~~~~~~~~
-- We need to use 'BndrMap' for 'Coercion', 'CoreExpr' AND 'Type', since all
-- of these data types have binding forms.

lkBndr :: CmEnv -> Var -> BndrMap a -> Maybe a
lkBndr env v = lkG (D env (varType v))

xtBndr :: CmEnv -> Var -> XT a -> BndrMap a -> BndrMap a
xtBndr env v = xtG (D env (varType v))

--------- Variable occurrence -------------
data VarMap a = VM { vm_bvar   :: BoundVarMap a  -- Bound variable
                   , vm_fvar   :: DVarEnv a }      -- Free variable
  deriving (Foldable, Functor, Traversable)

LENS_FIELD(vm_bvarL, vm_bvar)
LENS_FIELD(vm_fvarL, vm_fvar)

instance TrieMap VarMap where
   type Key VarMap = Var
   emptyTM  = VM { vm_bvar = IntMap.empty, vm_fvar = emptyDVarEnv }
   lookupTM = lkVar emptyCME
   alterTM  = xtVar emptyCME

lkVar :: CmEnv -> Var -> VarMap a -> Maybe a
lkVar env v
  | Just bv <- lookupCME env v = vm_bvar >>> lookupTM bv
  | otherwise                  = vm_fvar >>> lkDFreeVar v

xtVar :: CmEnv -> Var -> XT a -> VarMap a -> VarMap a
xtVar env v f
  | Just bv <- lookupCME env v = over vm_bvarL $ alterTM bv f
  | otherwise                  = over vm_fvarL $ xtDFreeVar v f

lkDFreeVar :: Var -> DVarEnv a -> Maybe a
lkDFreeVar = flip lookupDVarEnv

xtDFreeVar :: Var -> XT a -> DVarEnv a -> DVarEnv a
xtDFreeVar v f m = alterDVarEnv f m v
