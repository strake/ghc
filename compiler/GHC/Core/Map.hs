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
   IsMap (..),
   lkDNamed, xtDNamed, xtDNamedF,
 ) where

import GHC.Prelude

import GHC.Data.Collections
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
import Data.Functor.Compose( Compose (..) )
import Data.Functor.Identity( Identity (..) )

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
-- to specify @KeyOf TypeMapX@ (and not @DeBruijn Type@, the reduced form)

-- The CoreMap makes heavy use of GenMap. However the CoreMap Types are not
-- known when defining GenMap so we can only specialize them here.

{-# SPECIALIZE lkG :: KeyOf TypeMapX     -> TypeMapG a     -> Maybe a #-}
{-# SPECIALIZE lkG :: KeyOf CoercionMapX -> CoercionMapG a -> Maybe a #-}
{-# SPECIALIZE lkG :: KeyOf CoreMapX     -> CoreMapG a     -> Maybe a #-}

{-# SPECIALIZE xtGF :: Functor f => XTF f a -> KeyOf TypeMapX     -> TypeMapG a -> f (TypeMapG a) #-}
{-# SPECIALIZE xtGF :: Functor f => XTF f a -> KeyOf CoercionMapX -> CoercionMapG a -> f (CoercionMapG a) #-}
{-# SPECIALIZE xtGF :: Functor f => XTF f a -> KeyOf CoreMapX     -> CoreMapG a -> f (CoreMapG a) #-}


{-
************************************************************************
*                                                                      *
                   CoreMap
*                                                                      *
************************************************************************
-}

lkDNamed :: NamedThing n => n -> DNameEnv a -> Maybe a
lkDNamed n = mapLookup (getName n)

xtDNamed :: NamedThing n => n -> XT a -> DNameEnv a -> DNameEnv a
xtDNamed tc f = mapAlter f (getName tc)

xtDNamedF :: Functor f => NamedThing n => n -> XTF f a -> DNameEnv a -> f (DNameEnv a)
xtDNamedF tc f = mapAlterF f (getName tc)


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

instance Filtrable CoreMap where
    mapMaybe f (CoreMap x) = CoreMap (mapMaybe f x)

instance IsStaticKeylessMap CoreMap where
    type KeyOf CoreMap = CoreExpr
    mapAdjustLookup f k (CoreMap m) = CoreMap <$> mapAdjustLookup f (deBruijnize k) m
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap CoreMap where
    mapEmpty = CoreMap mapEmpty
    mapAlterF f k (CoreMap m) = CoreMap <$> mapAlterF f (deBruijnize k) m
    mapMergeA f (CoreMap m) (CoreMap n) = CoreMap <$> mapMergeA f m n

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

instance Filtrable CoreMapX where
    mapMaybe f (CM as1 as2 as3 as4 as5 as6 as7 as8 as9 as10 as11 as12) = CM (mapMaybe f as1) (mapMaybe f as2) (mapMaybe f as3) (mapMaybe f as4) (mapMaybe f <$> as5) (mapMaybe f <$> as6) (mapMaybe f <$> as7) (mapMaybe f <$> as8) ((fmap . fmap . mapMaybe) f as9) ((fmap . fmap . mapMaybe) f as10) (mapMaybe f <$> as11) (mapMaybe f <$> as12)

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
emptyE = CM { cm_var = mapEmpty, cm_lit = mapEmpty
            , cm_co = mapEmpty, cm_type = mapEmpty
            , cm_cast = mapEmpty, cm_app = mapEmpty
            , cm_lam = mapEmpty, cm_letn = mapEmpty
            , cm_letr = mapEmpty, cm_case = mapEmpty
            , cm_ecase = mapEmpty, cm_tick = mapEmpty }

instance IsStaticKeylessMap CoreMapX where
    type KeyOf CoreMapX = DeBruijn CoreExpr
    mapLookup = lkE
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap CoreMapX where
    mapEmpty = emptyE
    mapAlterF = flip xtEF
    mapMergeA f (CM as1 as2 as3 as4 as5 as6 as7 as8 as9 as10 as11 as12) (CM bs1 bs2 bs3 bs4 bs5 bs6 bs7 bs8 bs9 bs10 bs11 bs12) = CM <$> mapMergeA f as1 bs1 <*> mapMergeA f as2 bs2 <*> mapMergeA f as3 bs3 <*> mapMergeA f as4 bs4 <*> (getCompose <$> mapMergeA f (Compose as5) (Compose bs5)) <*> (getCompose <$> mapMergeA f (Compose as6) (Compose bs6)) <*> (getCompose <$> mapMergeA f (Compose as7) (Compose bs7)) <*> (getCompose <$> mapMergeA f (Compose as8) (Compose bs8)) <*> (getCompose . getCompose <$> mapMergeA f (Compose (Compose as9)) (Compose (Compose bs9))) <*> (getCompose . getCompose <$> mapMergeA f (Compose (Compose as10)) (Compose (Compose bs10))) <*> (getCompose <$> mapMergeA f (Compose as11) (Compose bs11)) <*> (getCompose <$> mapMergeA f (Compose as12) (Compose bs12))

--------------------------
instance Outputable a => Outputable (CoreMap a) where
  ppr m = text "CoreMap elts" <+> ppr (toList m)

-------------------------
-- lkE: lookup in trie for expressions
lkE :: DeBruijn CoreExpr -> CoreMapX a -> Maybe a
lkE (D env expr) = go expr
  where
    go (Var v)              = cm_var  >>> lkVar env v
    go (Lit l)              = cm_lit  >>> mapLookup l
    go (Type t)             = cm_type >>> lkG (D env t)
    go (Coercion c)         = cm_co   >>> lkG (D env c)
    go (Cast e c)           = cm_cast >>> lkG (D env e) >=> lkG (D env c)
    go (Tick tickish e)     = cm_tick >>> lkG (D env e) >=> mapLookup tickish
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

xtEF :: Functor f => DeBruijn CoreExpr -> XTF f a -> CoreMapX a -> f (CoreMapX a)
xtEF (D env (Var v))              = cm_varL . flip (xtVarF env) v
xtEF (D env (Type t))             = cm_typeL . flip mapAlterF (D env t)
xtEF (D env (Coercion c))         = cm_coL . flip mapAlterF (D env c)
xtEF (D _   (Lit l))              = cm_litL . flip mapAlterF l
xtEF (D env (Cast e c))           = cm_castL . flip mapAlterF (D env e) . xtf . flip mapAlterF (D env c)
xtEF (D env (Tick t e))           = cm_tickL . flip mapAlterF (D env e) . xtf . flip mapAlterF t
xtEF (D env (App e1 e2))          = cm_appL . flip mapAlterF (D env e2) . xtf . flip mapAlterF (D env e1)
xtEF (D env (Lam v e))            = cm_lamL . flip mapAlterF (D (extendCME env v) e) . xtf . xtBndrF env v
xtEF (D env (Let (NonRec b r) e)) = cm_letnL . flip mapAlterF (D (extendCME env b) e) . xtf . flip mapAlterF (D env r) . xtf . xtBndrF env b
xtEF (D env (Let (Rec prs) e))    =
    let (bndrs,rhss) = unzip prs
        env1 = foldl' extendCME env bndrs
    in  cm_letrL
          . flip (xtListF (flip $ flip mapAlterF . D env1)) rhss . xtf
          . flip mapAlterF (D env1 e) . xtf
          . flip (xtListF (flip $ xtBndrF env1)) bndrs
xtEF (D env (Case e b ty as))
                     | null as   = cm_ecaseL . flip xtGF (D env e) . xtf . flip xtGF (D env ty)
                     | otherwise = cm_caseL . flip xtGF (D env e) . xtf
                                                   . let env1 = extendCME env b
                                                     in flip (xtListF (flip $ xtAF env1)) as

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

instance Filtrable AltMap where
    mapMaybe f (AM as bs cs) = AM (mapMaybe f as) ((fmap . mapMaybe) f bs) ((fmap . mapMaybe) f cs)

instance IsStaticKeylessMap AltMap where
    type KeyOf AltMap = CoreAlt
    mapLookup = lkA emptyCME
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap AltMap where
    mapEmpty = AM { am_deflt = mapEmpty
                  , am_data = mapEmpty
                  , am_lit = mapEmpty }
    mapAlterF = flip $ xtAF emptyCME
    mapMergeA f (AM as1 bs1 cs1) (AM as2 bs2 cs2) = AM <$> mapMergeA f as1 as2 <*> (getCompose <$> mapMergeA f (Compose bs1) (Compose bs2)) <*> (getCompose <$> mapMergeA f (Compose cs1) (Compose cs2))

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
lkA env (LitAlt lit, _, rhs)  = am_lit >>> mapLookup lit >=> lkG (D env rhs)
lkA env (DataAlt dc, bs, rhs) = am_data >>> lkDNamed dc >=> lkG (D (foldl' extendCME env bs) rhs)

xtAF :: Functor f => CmEnv -> CoreAlt -> XTF f a -> AltMap a -> f (AltMap a)
xtAF env (DEFAULT, _, rhs)    = am_defltL . flip mapAlterF (D env rhs)
xtAF env (LitAlt l, _, rhs)   = am_litL . flip mapAlterF l . xtf . flip mapAlterF (D env rhs)
xtAF env (DataAlt d, bs, rhs) = am_dataL . xtDNamedF d . xtf . flip mapAlterF (D (foldl' extendCME env bs) rhs)

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

instance Filtrable CoercionMap where
    mapMaybe f (CoercionMap m) = CoercionMap (mapMaybe f m)

instance IsStaticKeylessMap CoercionMap where
    type KeyOf CoercionMap = Coercion
    mapLookup k (CoercionMap m) = mapLookup (deBruijnize k) m
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap CoercionMap where
    mapEmpty = CoercionMap mapEmpty
    mapAlterF f k (CoercionMap m) = CoercionMap <$> mapAlterF f (deBruijnize k) m
    mapMergeA f (CoercionMap m) (CoercionMap n) = CoercionMap <$> mapMergeA f m n

type CoercionMapG = GenMap CoercionMapX
newtype CoercionMapX a = CoercionMapX (TypeMapX a)
  deriving (Foldable, Functor, Traversable)

instance Filtrable CoercionMapX where
    mapMaybe f (CoercionMapX m) = CoercionMapX (mapMaybe f m)

instance IsStaticKeylessMap CoercionMapX where
    type KeyOf CoercionMapX = DeBruijn Coercion
    mapLookup = lkC
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap CoercionMapX where
    mapEmpty = CoercionMapX mapEmpty
    mapAlterF = flip xtCF
    mapMergeA f (CoercionMapX as) (CoercionMapX bs) = CoercionMapX <$> mapMergeA f as bs

instance Eq (DeBruijn Coercion) where
  D env1 co1 == D env2 co2 = D env1 (coercionType co1) == D env2 (coercionType co2)

lkC :: DeBruijn Coercion -> CoercionMapX a -> Maybe a
lkC (D env co) (CoercionMapX core_tm) = lkT (D env $ coercionType co) core_tm

xtCF :: Functor f => DeBruijn Coercion -> XTF f a -> CoercionMapX a -> f (CoercionMapX a)
xtCF (D env co) f (CoercionMapX m) = CoercionMapX <$> xtTF (D env $ coercionType co) f m

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

instance Filtrable TypeMapX where
    mapMaybe f (TM as1 as2 as3 as4 as5 as6) = TM (mapMaybe f as1) ((fmap . mapMaybe) f as2) (mapMaybe f as3) ((fmap . mapMaybe) f as4) (mapMaybe f as5) (mapMaybe f as6)

instance IsStaticKeylessMap TypeMapX where
    type KeyOf TypeMapX = DeBruijn Type
    mapLookup = lkT
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap TypeMapX where
    mapEmpty = emptyT
    mapAlterF = flip xtTF
    mapMergeA f (TM as1 bs1 cs1 ds1 es1 fs1) (TM as2 bs2 cs2 ds2 es2 fs2) = TM <$> mapMergeA f as1 as2 <*> (getCompose <$> mapMergeA f (Compose bs1) (Compose bs2)) <*> mapMergeA f cs1 cs2 <*> (getCompose <$> mapMergeA f (Compose ds1) (Compose ds2)) <*> mapMergeA f es1 es2 <*> mapMergeA f fs1 fs2

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
emptyT = TM { tm_var  = mapEmpty
            , tm_app  = mapEmpty
            , tm_tycon  = mapEmpty
            , tm_forall = mapEmpty
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
xtTF :: Functor f => DeBruijn Type -> XTF f a -> TypeMapX a -> f (TypeMapX a)
xtTF (D env ty) | Just ty' <- trieMapView ty = xtTF (D env ty')

xtTF (D env (TyVarTy v))       = tm_varL . flip (xtVarF env) v
xtTF (D env (AppTy t1 t2))     = tm_appL . flip mapAlterF (D env t1) . xtf . flip mapAlterF (D env t2)
xtTF (D _   (TyConApp tc []))  = tm_tyconL . xtDNamedF tc
xtTF (D _   (LitTy l))         = tm_tylitL . xtTyLitF l
xtTF (D env (CastTy t _))      = xtTF (D env t)
xtTF (D _   (CoercionTy {}))   = tm_coerceL
xtTF (D env (ForAllTy (Bndr tv _) ty)) = tm_forallL .
    flip mapAlterF (D (extendCME env tv) ty) . xtf . xtBndrF env tv
xtTF (D _   ty@(TyConApp _ (_:_))) = pprPanic "xtT TyConApp" (ppr ty)
xtTF (D _   ty@(FunTy {}))         = pprPanic "xtT FunTy" (ppr ty)

------------------------
data TyLitMap a = TLM { tlm_number :: Map.Map Integer a
                      , tlm_string :: Map.Map FastString a
                      }
  deriving (Foldable, Functor, Traversable)

instance Filtrable TyLitMap where
    mapMaybe f (TLM as bs) = TLM (mapMaybe f as) (mapMaybe f bs)

instance IsStaticKeylessMap TyLitMap where
    type KeyOf TyLitMap = TyLit
    mapLookup = lkTyLit
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap TyLitMap where
    mapEmpty = emptyTyLitMap
    mapAlterF = flip xtTyLitF
    mapMergeA f (TLM as1 bs1) (TLM as2 bs2) = TLM <$> mapMergeA f as1 as2 <*> mapMergeA f bs1 bs2

emptyTyLitMap :: TyLitMap a
emptyTyLitMap = TLM { tlm_number = Map.empty, tlm_string = Map.empty }

LENS_FIELD(tlm_numberL, tlm_number)
LENS_FIELD(tlm_stringL, tlm_string)

lkTyLit :: TyLit -> TyLitMap a -> Maybe a
lkTyLit = \ case
    NumTyLit n -> tlm_number >>> Map.lookup n
    StrTyLit n -> tlm_string >>> Map.lookup n

xtTyLitF :: Functor f => TyLit -> XTF f a -> TyLitMap a -> f (TyLitMap a)
xtTyLitF = \ case
    NumTyLit n -> tlm_numberL . flip Map.alterF n
    StrTyLit n -> tlm_stringL . flip Map.alterF n

-------------------------------------------------
-- | @TypeMap a@ is a map from 'Type' to @a@.  If you are a client, this
-- is the type you want. The keys in this map may have different kinds.
newtype TypeMap a = TypeMap (TypeMapG (TypeMapG a))
  deriving (Foldable, Functor, Traversable)

lkTT :: DeBruijn Type -> TypeMap a -> Maybe a
lkTT (D env ty) (TypeMap m) = lkG (D env $ typeKind ty) m >>= lkG (D env ty)

xtTTF :: Functor f => DeBruijn Type -> XTF f a -> TypeMap a -> f (TypeMap a)
xtTTF (D env ty) f (TypeMap m) = TypeMap <$> (flip mapAlterF (D env $ typeKind ty) . xtf . flip mapAlterF (D env ty)) f m

xtTT :: DeBruijn Type -> XT a -> TypeMap a -> TypeMap a
xtTT envd f = runIdentity . xtTTF envd (Identity . f)

-- Below are some client-oriented functions which operate on 'TypeMap'.

instance Filtrable TypeMap where
    mapMaybe f (TypeMap m) = TypeMap ((fmap . mapMaybe) f m)

instance IsStaticKeylessMap TypeMap where
    type KeyOf TypeMap = Type
    mapLookup k = lkTT (deBruijnize k)
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap TypeMap where
    mapEmpty = TypeMap mapEmpty
    mapAlterF = flip $ xtTTF . deBruijnize
    mapMergeA f (TypeMap as) (TypeMap bs) = TypeMap . getCompose <$> mapMergeA f (Compose as) (Compose bs)

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

instance Filtrable LooseTypeMap where
    mapMaybe f (LooseTypeMap as) = LooseTypeMap (mapMaybe f as)

instance IsStaticKeylessMap LooseTypeMap where
    type KeyOf LooseTypeMap = Type
    mapAdjustLookup f k (LooseTypeMap m) = LooseTypeMap <$> mapAdjustLookup f (deBruijnize k) m
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap LooseTypeMap where
    mapEmpty = LooseTypeMap mapEmpty
    mapAlterF f k (LooseTypeMap m) = LooseTypeMap <$> mapAlterF f (deBruijnize k) m
    mapMergeA f (LooseTypeMap as) (LooseTypeMap bs) = LooseTypeMap <$> mapMergeA f as bs

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

xtBndrF :: Functor f => CmEnv -> Var -> XTF f a -> BndrMap a -> f (BndrMap a)
xtBndrF env v = flip mapAlterF (D env (varType v))

--------- Variable occurrence -------------
data VarMap a = VM { vm_bvar   :: BoundVarMap a  -- Bound variable
                   , vm_fvar   :: DVarEnv a }      -- Free variable
  deriving (Foldable, Functor, Traversable)

instance Filtrable VarMap where
    mapMaybe f (VM xs ys) = VM (mapMaybe f xs) (mapMaybe f ys)

LENS_FIELD(vm_bvarL, vm_bvar)
LENS_FIELD(vm_fvarL, vm_fvar)

instance IsStaticKeylessMap VarMap where
    type KeyOf VarMap = Var
    mapAdjustLookup = defaultMapAdjustLookup
    mapIsSubmapOfBy = defaultMapIsSubmapOfBy

instance IsKeylessMap VarMap where
    mapEmpty = VM { vm_bvar = mapEmpty, vm_fvar = mapEmpty }
    mapAlterF = xtVarF emptyCME
    mapMergeA f (VM xs1 ys1) (VM xs2 ys2) = VM <$> mapMergeA f xs1 xs2 <*> mapMergeA f ys1 ys2

lkVar :: CmEnv -> Var -> VarMap a -> Maybe a
lkVar env v
  | Just bv <- lookupCME env v = vm_bvar >>> mapLookup bv
  | otherwise                  = vm_fvar >>> mapLookup v

xtVarF :: Functor f => CmEnv -> XTF f a -> Var -> VarMap a -> f (VarMap a)
xtVarF env f v
  | Just bv <- lookupCME env v = vm_bvarL $ mapAlterF f bv
  | otherwise                  = vm_fvarL $ mapAlterF f v
