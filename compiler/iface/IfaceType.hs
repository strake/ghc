{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}

{-# LANGUAGE CPP #-}
module IfaceType (
        IfExtName, IfLclName,

        IfaceType(..), IfacePredType, IfaceKind, IfaceCoercion(..),
        IfaceTyCon(..), IfaceTyConInfo(..),
        IfaceTyLit(..), IfaceTcArgs(..),
        IfaceContext, IfaceBndr(..), IfaceOneShot(..), IfaceLamBndr, IfaceTvBndr, IfaceIdBndr,

        -- Equality testing
        IfRnEnv2, emptyIfRnEnv2, eqIfaceType, eqIfaceTypes,
        eqIfaceTcArgs, eqIfaceTvBndrs, eqIfaceCoercion,

        -- Conversion from Type -> IfaceType
        toIfaceType, toIfaceTypes, toIfaceKind, toIfaceTyVar,
        toIfaceContext, toIfaceBndr, toIfaceIdBndr,
        toIfaceTvBndrs, toIfaceTyCon, toIfaceTyCon_name,
        toIfaceTcArgs,

        -- Conversion from IfaceTcArgs -> IfaceType
        tcArgsIfaceTypes,

        -- Conversion from Coercion -> IfaceCoercion
        toIfaceCoercion,

        -- Printing
        pprIfaceType, pprParendIfaceType,
        pprIfaceContext, pprIfaceContextArr, pprIfaceContextMaybe,
        pprIfaceIdBndr, pprIfaceLamBndr, pprIfaceTvBndr, pprIfaceTvBndrs,
        pprIfaceBndrs, pprIfaceTcArgs, pprParendIfaceTcArgs,
        pprIfaceForAllPart, pprIfaceForAll, pprIfaceSigmaType,
        pprIfaceCoercion, pprParendIfaceCoercion,
        splitIfaceSigmaTy, pprIfaceTypeApp, pprUserIfaceForAll,

        suppressIfaceKinds,
        stripIfaceKindVars,
        stripKindArgs,
        substIfaceType, substIfaceTyVar, substIfaceTcArgs, mkIfaceTySubst
    ) where

#include "HsVersions.h"

import Coercion
import DataCon ( isTupleDataCon )
import TcType
import DynFlags
import TypeRep
import Unique( hasKey )
import TyCon hiding ( pprPromotionQuote )
import CoAxiom
import Id
import Var
-- import RnEnv( FastStringEnv, mkFsEnv, lookupFsEnv )
import TysWiredIn
import TysPrim
import PrelNames( funTyConKey, ipClassName )
import Name
import BasicTypes
import Binary
import Outputable
import FastString
import UniqSet
import UniqFM
import Util
import Data.Maybe( fromMaybe )

{-
************************************************************************
*                                                                      *
                Local (nested) binders
*                                                                      *
************************************************************************
-}

type IfLclName = FastString     -- A local name in iface syntax

type IfExtName = Name   -- An External or WiredIn Name can appear in IfaceSyn
                        -- (However Internal or System Names never should)

data IfaceBndr          -- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !IfaceIdBndr
  | IfaceTvBndr {-# UNPACK #-} !IfaceTvBndr

type IfaceIdBndr  = (IfLclName, IfaceType)
type IfaceTvBndr  = (IfLclName, IfaceKind)


data IfaceOneShot    -- See Note [Preserve OneShotInfo] in CoreTicy
  = IfaceNoOneShot   -- and Note [The oneShot function] in MkId
  | IfaceOneShot

type IfaceLamBndr
  = (IfaceBndr, IfaceOneShot)

-------------------------------
type IfaceKind     = IfaceType

data IfaceType     -- A kind of universal type, used for types and kinds
  = IfaceTyVar    IfLclName               -- Type/coercion variable only, not tycon
  | IfaceLitTy    IfaceTyLit
  | IfaceAppTy    IfaceType IfaceType
  | IfaceFunTy    IfaceType IfaceType
  | IfaceDFunTy   IfaceType IfaceType
  | IfaceForAllTy IfaceTvBndr IfaceType

  | IfaceTyConApp IfaceTyCon IfaceTcArgs  -- Not necessarily saturated
                                          -- Includes newtypes, synonyms

  | IfaceTupleTy                  -- Saturated tuples (unsaturated ones use IfaceTyConApp)
       TupleSort IfaceTyConInfo   -- A bit like IfaceTyCon
       IfaceTcArgs                -- arity = length args
          -- For promoted data cons, the kind args are omitted

type IfacePredType = IfaceType
type IfaceContext = [IfacePredType]

data IfaceTyLit
  = IfaceNumTyLit Integer
  | IfaceStrTyLit FastString
  deriving (Eq)

-- See Note [Suppressing kinds]
-- We use a new list type (rather than [(IfaceType,Bool)], because
-- it'll be more compact and faster to parse in interface
-- files. Rather than two bytes and two decisions (nil/cons, and
-- type/kind) there'll just be one.
data IfaceTcArgs
  = ITC_Nil
  | ITC_Type IfaceType IfaceTcArgs
  | ITC_Kind IfaceKind IfaceTcArgs

-- Encodes type constructors, kind constructors,
-- coercion constructors, the lot.
-- We have to tag them in order to pretty print them
-- properly.
data IfaceTyCon = IfaceTyCon { ifaceTyConName :: IfExtName
                             , ifaceTyConInfo :: IfaceTyConInfo }
    deriving (Eq)

data IfaceTyConInfo   -- Used to guide pretty-printing
                      -- and to disambiguate D from 'D (they share a name)
  = NoIfaceTyConInfo
  | IfacePromotedDataCon
  | IfacePromotedTyCon
    deriving (Eq)

data IfaceCoercion
  = IfaceReflCo      Role IfaceType
  | IfaceFunCo       Role IfaceCoercion IfaceCoercion
  | IfaceTyConAppCo  Role IfaceTyCon [IfaceCoercion]
  | IfaceAppCo       IfaceCoercion IfaceCoercion
  | IfaceForAllCo    IfaceTvBndr IfaceCoercion
  | IfaceCoVarCo     IfLclName
  | IfaceAxiomInstCo IfExtName BranchIndex [IfaceCoercion]
  | IfaceUnivCo      FastString Role IfaceType IfaceType
  | IfaceSymCo       IfaceCoercion
  | IfaceTransCo     IfaceCoercion IfaceCoercion
  | IfaceNthCo       Int IfaceCoercion
  | IfaceLRCo        LeftOrRight IfaceCoercion
  | IfaceInstCo      IfaceCoercion IfaceType
  | IfaceSubCo       IfaceCoercion
  | IfaceAxiomRuleCo IfLclName [IfaceType] [IfaceCoercion]

{-
************************************************************************
*                                                                      *
                Functions over IFaceTypes
*                                                                      *
************************************************************************
-}

splitIfaceSigmaTy :: IfaceType -> ([IfaceTvBndr], [IfacePredType], IfaceType)
-- Mainly for printing purposes
splitIfaceSigmaTy ty
  = (tvs, theta, tau)
  where
    (tvs,   rho)   = split_foralls ty
    (theta, tau)   = split_rho rho

    split_foralls (IfaceForAllTy tv ty)
        = case split_foralls ty of { (tvs, rho) -> (tv:tvs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceDFunTy ty1 ty2)
        = case split_rho ty2 of { (ps, tau) -> (ty1:ps, tau) }
    split_rho tau = ([], tau)

suppressIfaceKinds :: DynFlags -> [IfaceTvBndr] -> [a] -> [a]
suppressIfaceKinds dflags tys xs
  | gopt Opt_PrintExplicitKinds dflags = xs
  | otherwise = suppress tys xs
    where
      suppress _       []      = []
      suppress []      a       = a
      suppress (k:ks) a@(_:xs)
        | isIfaceKindVar k = suppress ks xs
        | otherwise        = a

stripIfaceKindVars :: DynFlags -> [IfaceTvBndr] -> [IfaceTvBndr]
stripIfaceKindVars dflags tyvars
  | gopt Opt_PrintExplicitKinds dflags = tyvars
  | otherwise = filterOut isIfaceKindVar tyvars

isIfaceKindVar :: IfaceTvBndr -> Bool
isIfaceKindVar (_, IfaceTyConApp tc _) = ifaceTyConName tc == superKindTyConName
isIfaceKindVar _                       = False

ifTyVarsOfType :: IfaceType -> UniqSet IfLclName
ifTyVarsOfType ty
  = case ty of
      IfaceTyVar v -> unitUniqSet v
      IfaceAppTy fun arg
        -> ifTyVarsOfType fun `unionUniqSets` ifTyVarsOfType arg
      IfaceFunTy arg res
        -> ifTyVarsOfType arg `unionUniqSets` ifTyVarsOfType res
      IfaceDFunTy arg res
        -> ifTyVarsOfType arg `unionUniqSets` ifTyVarsOfType res
      IfaceForAllTy (var,t) ty
        -> delOneFromUniqSet (ifTyVarsOfType ty) var `unionUniqSets`
           ifTyVarsOfType t
      IfaceTyConApp _ args  -> ifTyVarsOfArgs args
      IfaceTupleTy _ _ args -> ifTyVarsOfArgs args
      IfaceLitTy    _       -> emptyUniqSet

ifTyVarsOfArgs :: IfaceTcArgs -> UniqSet IfLclName
ifTyVarsOfArgs args = argv emptyUniqSet args
   where
     argv vs (ITC_Type t ts) = argv (vs `unionUniqSets` (ifTyVarsOfType t)) ts
     argv vs (ITC_Kind k ks) = argv (vs `unionUniqSets` (ifTyVarsOfType k)) ks
     argv vs ITC_Nil         = vs

{-
Substitutions on IfaceType. This is only used during pretty-printing to construct
the result type of a GADT, and does not deal with binders (eg IfaceForAll), so
it doesn't need fancy capture stuff.
-}

type IfaceTySubst = FastStringEnv IfaceType

mkIfaceTySubst :: [IfaceTvBndr] -> [IfaceType] -> IfaceTySubst
mkIfaceTySubst tvs tys = mkFsEnv $ zipWithEqual "mkIfaceTySubst" (\(fs,_) ty -> (fs,ty)) tvs tys

substIfaceType :: IfaceTySubst -> IfaceType -> IfaceType
substIfaceType env ty
  = go ty
  where
    go (IfaceTyVar tv)        = substIfaceTyVar env tv
    go (IfaceAppTy  t1 t2)    = IfaceAppTy  (go t1) (go t2)
    go (IfaceFunTy  t1 t2)    = IfaceFunTy  (go t1) (go t2)
    go (IfaceDFunTy t1 t2)    = IfaceDFunTy (go t1) (go t2)
    go ty@(IfaceLitTy {})     = ty
    go (IfaceTyConApp tc tys) = IfaceTyConApp tc (substIfaceTcArgs env tys)
    go (IfaceTupleTy s i tys) = IfaceTupleTy s i (substIfaceTcArgs env tys)
    go (IfaceForAllTy {})     = pprPanic "substIfaceType" (ppr ty)

substIfaceTcArgs :: IfaceTySubst -> IfaceTcArgs -> IfaceTcArgs
substIfaceTcArgs env args
  = go args
  where
    go ITC_Nil           = ITC_Nil
    go (ITC_Type ty tys) = ITC_Type (substIfaceType env ty) (go tys)
    go (ITC_Kind ty tys) = ITC_Kind (substIfaceType env ty) (go tys)

substIfaceTyVar :: IfaceTySubst -> IfLclName -> IfaceType
substIfaceTyVar env tv
  | Just ty <- lookupFsEnv env tv = ty
  | otherwise                     = IfaceTyVar tv

{-
************************************************************************
*                                                                      *
                Equality over IfaceTypes
*                                                                      *
************************************************************************
-}

-- Like an RnEnv2, but mapping from FastString to deBruijn index
-- DeBruijn; see eqTypeX
type BoundVar = Int
data IfRnEnv2
  = IRV2 { ifenvL :: UniqFM BoundVar -- from FastString
         , ifenvR :: UniqFM BoundVar
         , ifenv_next :: BoundVar
         }

emptyIfRnEnv2 :: IfRnEnv2
emptyIfRnEnv2 = IRV2 { ifenvL = emptyUFM
                     , ifenvR = emptyUFM
                     , ifenv_next = 0 }

rnIfOccL :: IfRnEnv2 -> IfLclName -> Maybe BoundVar
rnIfOccL env = lookupUFM (ifenvL env)

rnIfOccR :: IfRnEnv2 -> IfLclName -> Maybe BoundVar
rnIfOccR env = lookupUFM (ifenvR env)

extendIfRnEnv2 :: IfRnEnv2 -> IfLclName -> IfLclName -> IfRnEnv2
extendIfRnEnv2 IRV2 { ifenvL = lenv
                    , ifenvR = renv
                    , ifenv_next = n } tv1 tv2
             = IRV2 { ifenvL = addToUFM lenv tv1 n
                    , ifenvR = addToUFM renv tv2 n
                    , ifenv_next = n + 1
                    }

eqIfaceType :: IfRnEnv2 -> IfaceType -> IfaceType -> Bool
eqIfaceType env (IfaceTyVar tv1) (IfaceTyVar tv2) =
    case (rnIfOccL env tv1, rnIfOccR env tv2) of
        (Just v1, Just v2) -> v1 == v2
        (Nothing, Nothing) -> tv1 == tv2
        _ -> False
eqIfaceType _   (IfaceLitTy l1) (IfaceLitTy l2) = l1 == l2
eqIfaceType env (IfaceAppTy t11 t12) (IfaceAppTy t21 t22)
    = eqIfaceType env t11 t21 && eqIfaceType env t12 t22
eqIfaceType env (IfaceFunTy t11 t12) (IfaceFunTy t21 t22)
    = eqIfaceType env t11 t21 && eqIfaceType env t12 t22
eqIfaceType env (IfaceDFunTy t11 t12) (IfaceDFunTy t21 t22)
    = eqIfaceType env t11 t21 && eqIfaceType env t12 t22
eqIfaceType env (IfaceForAllTy (tv1, k1) t1) (IfaceForAllTy (tv2, k2) t2)
    = eqIfaceType env k1 k2 && eqIfaceType (extendIfRnEnv2 env tv1 tv2) t1 t2
eqIfaceType env (IfaceTyConApp tc1 tys1) (IfaceTyConApp tc2 tys2)
    = tc1 == tc2 && eqIfaceTcArgs env tys1 tys2
eqIfaceType env (IfaceTupleTy s1 tc1 tys1) (IfaceTupleTy s2 tc2 tys2)
    = s1 == s2 && tc1 == tc2 && eqIfaceTcArgs env tys1 tys2
eqIfaceType _ _ _ = False

eqIfaceTypes :: IfRnEnv2 -> [IfaceType] -> [IfaceType] -> Bool
eqIfaceTypes env tys1 tys2 = and (zipWith (eqIfaceType env) tys1 tys2)

eqIfaceTcArgs :: IfRnEnv2 -> IfaceTcArgs -> IfaceTcArgs -> Bool
eqIfaceTcArgs _ ITC_Nil ITC_Nil = True
eqIfaceTcArgs env (ITC_Type ty1 tys1) (ITC_Type ty2 tys2)
    = eqIfaceType env ty1 ty2 && eqIfaceTcArgs env tys1 tys2
eqIfaceTcArgs env (ITC_Kind ty1 tys1) (ITC_Kind ty2 tys2)
    = eqIfaceType env ty1 ty2 && eqIfaceTcArgs env tys1 tys2
eqIfaceTcArgs _ _ _ = False

-- | Similar to 'eqTyVarBndrs', checks that tyvar lists
-- are the same length and have matching kinds; if so, extend the
-- 'IfRnEnv2'.  Returns 'Nothing' if they don't match.
eqIfaceTvBndrs :: IfRnEnv2 -> [IfaceTvBndr] -> [IfaceTvBndr] -> Maybe IfRnEnv2
eqIfaceTvBndrs env [] [] = Just env
eqIfaceTvBndrs env ((tv1, k1):tvs1) ((tv2, k2):tvs2)
  | eqIfaceType env k1 k2
  = eqIfaceTvBndrs (extendIfRnEnv2 env tv1 tv2) tvs1 tvs2
eqIfaceTvBndrs _ _ _ = Nothing

-- coreEqCoercion2
eqIfaceCoercion :: IfRnEnv2 -> IfaceCoercion -> IfaceCoercion -> Bool
eqIfaceCoercion env (IfaceReflCo eq1 ty1) (IfaceReflCo eq2 ty2)
    = eq1 == eq2 && eqIfaceType env ty1 ty2
eqIfaceCoercion env (IfaceFunCo eq1 co11 co12) (IfaceFunCo eq2 co21 co22)
  = eq1 == eq2 && eqIfaceCoercion env co11 co21
               && eqIfaceCoercion env co12 co22
eqIfaceCoercion env (IfaceTyConAppCo eq1 tc1 cos1) (IfaceTyConAppCo eq2 tc2 cos2)
  = eq1 == eq2 && tc1 == tc2 && all2 (eqIfaceCoercion env) cos1 cos2
eqIfaceCoercion env (IfaceAppCo co11 co12) (IfaceAppCo co21 co22)
  = eqIfaceCoercion env co11 co21 && eqIfaceCoercion env co12 co22

eqIfaceCoercion env (IfaceForAllCo (v1,k1) co1) (IfaceForAllCo (v2,k2) co2)
  = eqIfaceType env k1 k2 &&
    eqIfaceCoercion (extendIfRnEnv2 env v1 v2) co1 co2

eqIfaceCoercion env (IfaceCoVarCo cv1) (IfaceCoVarCo cv2)
  = rnIfOccL env cv1 == rnIfOccR env cv2

eqIfaceCoercion env (IfaceAxiomInstCo con1 ind1 cos1)
                    (IfaceAxiomInstCo con2 ind2 cos2)
  = con1 == con2
    && ind1 == ind2
    && all2 (eqIfaceCoercion env) cos1 cos2

-- the provenance string is just a note, so don't use in comparisons
eqIfaceCoercion env (IfaceUnivCo _ r1 ty11 ty12) (IfaceUnivCo _ r2 ty21 ty22)
  = r1 == r2 && eqIfaceType env ty11 ty21 && eqIfaceType env ty12 ty22

eqIfaceCoercion env (IfaceSymCo co1) (IfaceSymCo co2)
  = eqIfaceCoercion env co1 co2

eqIfaceCoercion env (IfaceTransCo co11 co12) (IfaceTransCo co21 co22)
  = eqIfaceCoercion env co11 co21 && eqIfaceCoercion env co12 co22

eqIfaceCoercion env (IfaceNthCo d1 co1) (IfaceNthCo d2 co2)
  = d1 == d2 && eqIfaceCoercion env co1 co2
eqIfaceCoercion env (IfaceLRCo d1 co1) (IfaceLRCo d2 co2)
  = d1 == d2 && eqIfaceCoercion env co1 co2

eqIfaceCoercion env (IfaceInstCo co1 ty1) (IfaceInstCo co2 ty2)
  = eqIfaceCoercion env co1 co2 && eqIfaceType env ty1 ty2

eqIfaceCoercion env (IfaceSubCo co1) (IfaceSubCo co2)
  = eqIfaceCoercion env co1 co2

eqIfaceCoercion env (IfaceAxiomRuleCo a1 ts1 cs1) (IfaceAxiomRuleCo a2 ts2 cs2)
  = a1 == a2 && all2 (eqIfaceType env) ts1 ts2 && all2 (eqIfaceCoercion env) cs1 cs2

eqIfaceCoercion _ _ _ = False

{-
************************************************************************
*                                                                      *
                Functions over IFaceTcArgs
*                                                                      *
************************************************************************
-}

stripKindArgs :: DynFlags -> IfaceTcArgs -> IfaceTcArgs
stripKindArgs dflags tys
  | gopt Opt_PrintExplicitKinds dflags = tys
  | otherwise = suppressKinds tys
    where
      suppressKinds c
        = case c of
            ITC_Kind _ ts -> suppressKinds ts
            _ -> c

toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
-- See Note [Suppressing kinds]
toIfaceTcArgs tc ty_args
  = go (tyConKind tc) ty_args
  where
    go _                []     = ITC_Nil
    go (ForAllTy _ res) (t:ts) = ITC_Kind (toIfaceKind t) (go res ts)
    go (FunTy _ res)    (t:ts) = ITC_Type (toIfaceType t) (go res ts)
    go kind             (t:ts) = WARN( True, ppr tc $$ ppr (tyConKind tc) $$ ppr ty_args )
                                 ITC_Type (toIfaceType t) (go kind ts) -- Ill-kinded

tcArgsIfaceTypes :: IfaceTcArgs -> [IfaceType]
tcArgsIfaceTypes ITC_Nil = []
tcArgsIfaceTypes (ITC_Kind t ts) = t : tcArgsIfaceTypes ts
tcArgsIfaceTypes (ITC_Type t ts) = t : tcArgsIfaceTypes ts

{-
Note [Suppressing kinds]
~~~~~~~~~~~~~~~~~~~~~~~~
We use the IfaceTcArgs to specify which of the arguments to a type
constructor instantiate a for-all, and which are regular kind args.
This in turn used to control kind-suppression when printing types,
under the control of -fprint-explicit-kinds.  See also TypeRep.suppressKinds.
For example, given
    T :: forall k. (k->*) -> k -> *    -- Ordinary kind polymorphism
    'Just :: forall k. k -> 'Maybe k   -- Promoted
we want
  T * Tree Int    prints as    T Tree Int
  'Just *         prints as    Just *


************************************************************************
*                                                                      *
                Pretty-printing
*                                                                      *
************************************************************************
-}

pprIfaceInfixApp :: (TyPrec -> a -> SDoc) -> TyPrec -> SDoc -> a -> a -> SDoc
pprIfaceInfixApp pp p pp_tc ty1 ty2
  = maybeParen p FunPrec $
    sep [pp FunPrec ty1, pprInfixVar True pp_tc <+> pp FunPrec ty2]

pprIfacePrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc
pprIfacePrefixApp p pp_fun pp_tys
  | null pp_tys = pp_fun
  | otherwise   = maybeParen p TyConPrec $
                  hang pp_fun 2 (sep pp_tys)

-- ----------------------------- Printing binders ------------------------------------

instance Outputable IfaceBndr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <+> pprIfaceTvBndr bndr

pprIfaceBndrs :: [IfaceBndr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceLamBndr :: IfaceLamBndr -> SDoc
pprIfaceLamBndr (b, IfaceNoOneShot) = ppr b
pprIfaceLamBndr (b, IfaceOneShot)   = ppr b <> text "[OneShot]"

pprIfaceIdBndr :: (IfLclName, IfaceType) -> SDoc
pprIfaceIdBndr (name, ty) = hsep [ppr name, dcolon, ppr ty]

pprIfaceTvBndr :: IfaceTvBndr -> SDoc
pprIfaceTvBndr (tv, IfaceTyConApp tc ITC_Nil)
  | ifaceTyConName tc == liftedTypeKindTyConName = ppr tv
pprIfaceTvBndr (tv, kind) = parens (ppr tv <+> dcolon <+> ppr kind)

pprIfaceTvBndrs :: [IfaceTvBndr] -> SDoc
pprIfaceTvBndrs tyvars = sep (map pprIfaceTvBndr tyvars)

instance Binary IfaceBndr where
    put_ bh (IfaceIdBndr aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (IfaceTvBndr ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (IfaceIdBndr aa)
              _ -> do ab <- get bh
                      return (IfaceTvBndr ab)

instance Binary IfaceOneShot where
    put_ bh IfaceNoOneShot = do
            putByte bh 0
    put_ bh IfaceOneShot = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return IfaceNoOneShot
              _ -> do return IfaceOneShot

-- ----------------------------- Printing IfaceType ------------------------------------

---------------------------------
instance Outputable IfaceType where
  ppr ty = pprIfaceType ty

pprIfaceType, pprParendIfaceType ::IfaceType -> SDoc
pprIfaceType       = ppr_ty TopPrec
pprParendIfaceType = ppr_ty TyConPrec

ppr_ty :: TyPrec -> IfaceType -> SDoc
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = sdocWithDynFlags (pprTyTcApp ctxt_prec tc tys)
ppr_ty _         (IfaceTupleTy s i tys) = pprTuple s i tys
ppr_ty _         (IfaceLitTy n)         = ppr_tylit n
        -- Function types
ppr_ty ctxt_prec (IfaceFunTy ty1 ty2)
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen ctxt_prec FunPrec $
    sep [ppr_ty FunPrec ty1, sep (ppr_fun_tail ty2)]
  where
    ppr_fun_tail (IfaceFunTy ty1 ty2)
      = (arrow <+> ppr_ty FunPrec ty1) : ppr_fun_tail ty2
    ppr_fun_tail other_ty
      = [arrow <+> pprIfaceType other_ty]

ppr_ty ctxt_prec (IfaceAppTy ty1 ty2)
  = maybeParen ctxt_prec TyConPrec $
    ppr_ty FunPrec ty1 <+> pprParendIfaceType ty2

ppr_ty ctxt_prec ty
  = maybeParen ctxt_prec FunPrec (ppr_iface_sigma_type True ty)

instance Outputable IfaceTcArgs where
  ppr tca = pprIfaceTcArgs tca

pprIfaceTcArgs, pprParendIfaceTcArgs :: IfaceTcArgs -> SDoc
pprIfaceTcArgs  = ppr_tc_args TopPrec
pprParendIfaceTcArgs = ppr_tc_args TyConPrec

ppr_tc_args :: TyPrec -> IfaceTcArgs -> SDoc
ppr_tc_args ctx_prec args
 = let pprTys t ts = ppr_ty ctx_prec t <+> ppr_tc_args ctx_prec ts
   in case args of
        ITC_Nil       -> empty
        ITC_Type t ts -> pprTys t ts
        ITC_Kind t ts -> pprTys t ts

-------------------
ppr_iface_sigma_type :: Bool -> IfaceType -> SDoc
ppr_iface_sigma_type show_foralls_unconditionally ty
  = ppr_iface_forall_part show_foralls_unconditionally tvs theta (ppr tau)
  where
    (tvs, theta, tau) = splitIfaceSigmaTy ty

pprIfaceForAllPart :: Outputable a => [IfaceTvBndr] -> [a] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt sdoc = ppr_iface_forall_part False tvs ctxt sdoc

ppr_iface_forall_part :: Outputable a
                      => Bool -> [IfaceTvBndr] -> [a] -> SDoc -> SDoc
ppr_iface_forall_part show_foralls_unconditionally tvs ctxt sdoc
  = sep [ if show_foralls_unconditionally
          then pprIfaceForAll tvs
          else pprUserIfaceForAll tvs
        , pprIfaceContextArr ctxt
        , sdoc]

pprIfaceForAll :: [IfaceTvBndr] -> SDoc
pprIfaceForAll []  = empty
pprIfaceForAll tvs = ptext (sLit "forall") <+> pprIfaceTvBndrs tvs <> dot

pprIfaceSigmaType :: IfaceType -> SDoc
pprIfaceSigmaType ty = ppr_iface_sigma_type False ty

pprUserIfaceForAll :: [IfaceTvBndr] -> SDoc
pprUserIfaceForAll tvs
   = sdocWithDynFlags $ \dflags ->
     ppWhen (any tv_has_kind_var tvs || gopt Opt_PrintExplicitForalls dflags) $
     pprIfaceForAll tvs
   where
     tv_has_kind_var (_,t) = not (isEmptyUniqSet (ifTyVarsOfType t))
-------------------

-- See equivalent function in TypeRep.hs
pprIfaceTyList :: TyPrec -> IfaceType -> IfaceType -> SDoc
-- Given a type-level list (t1 ': t2), see if we can print
-- it in list notation [t1, ...].
-- Precondition: Opt_PrintExplicitKinds is off
pprIfaceTyList ctxt_prec ty1 ty2
  = case gather ty2 of
      (arg_tys, Nothing)
        -> char '\'' <> brackets (fsep (punctuate comma
                        (map (ppr_ty TopPrec) (ty1:arg_tys))))
      (arg_tys, Just tl)
        -> maybeParen ctxt_prec FunPrec $ hang (ppr_ty FunPrec ty1)
           2 (fsep [ colon <+> ppr_ty FunPrec ty | ty <- arg_tys ++ [tl]])
  where
    gather :: IfaceType -> ([IfaceType], Maybe IfaceType)
     -- (gather ty) = (tys, Nothing) means ty is a list [t1, .., tn]
     --             = (tys, Just tl) means ty is of form t1:t2:...tn:tl
    gather (IfaceTyConApp tc tys)
      | tcname == consDataConName
      , (ITC_Kind _ (ITC_Type ty1 (ITC_Type ty2 ITC_Nil))) <- tys
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tcname == nilDataConName
      = ([], Nothing)
      where tcname = ifaceTyConName tc
    gather ty = ([], Just ty)

pprIfaceTypeApp :: IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfaceTypeApp tc args = sdocWithDynFlags (pprTyTcApp TopPrec tc args)

pprTyTcApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> DynFlags -> SDoc
pprTyTcApp ctxt_prec tc tys dflags
  | ifaceTyConName tc == ipClassName
  , ITC_Type (IfaceLitTy (IfaceStrTyLit n)) (ITC_Type ty ITC_Nil) <- tys
  = char '?' <> ftext n <> ptext (sLit "::") <> ppr_ty TopPrec ty

  | ifaceTyConName tc == consDataConName
  , not (gopt Opt_PrintExplicitKinds dflags)
  , ITC_Kind _ (ITC_Type ty1 (ITC_Type ty2 ITC_Nil)) <- tys
  = pprIfaceTyList ctxt_prec ty1 ty2

  | otherwise
  = ppr_iface_tc_app ppr_ty ctxt_prec tc tys_wo_kinds
  where
    tys_wo_kinds = tcArgsIfaceTypes $ stripKindArgs dflags tys

pprIfaceCoTcApp :: TyPrec -> IfaceTyCon -> [IfaceCoercion] -> SDoc
pprIfaceCoTcApp ctxt_prec tc tys = ppr_iface_tc_app ppr_co ctxt_prec tc tys

ppr_iface_tc_app :: (TyPrec -> a -> SDoc) -> TyPrec -> IfaceTyCon -> [a] -> SDoc
ppr_iface_tc_app pp _ tc [ty]
  | n == listTyConName = pprPromotionQuote tc <> brackets (pp TopPrec ty)
  | n == parrTyConName = pprPromotionQuote tc <> paBrackets (pp TopPrec ty)
  where
    n = ifaceTyConName tc

ppr_iface_tc_app pp ctxt_prec tc tys
  | not (isSymOcc (nameOccName tc_name))
  = pprIfacePrefixApp ctxt_prec (ppr tc) (map (pp TyConPrec) tys)

  | [ty1,ty2] <- tys  -- Infix, two arguments;
                      -- we know nothing of precedence though
  = pprIfaceInfixApp pp ctxt_prec (ppr tc) ty1 ty2

  | tc_name == liftedTypeKindTyConName || tc_name == unliftedTypeKindTyConName
  = ppr tc   -- Do not wrap *, # in parens

  | otherwise
  = pprIfacePrefixApp ctxt_prec (parens (ppr tc)) (map (pp TyConPrec) tys)
  where
    tc_name = ifaceTyConName tc

pprTuple :: TupleSort -> IfaceTyConInfo -> IfaceTcArgs -> SDoc
pprTuple sort info args
  = pprPromotionQuoteI info <>
    tupleParens sort (pprWithCommas pprIfaceType (tcArgsIfaceTypes args))

ppr_tylit :: IfaceTyLit -> SDoc
ppr_tylit (IfaceNumTyLit n) = integer n
ppr_tylit (IfaceStrTyLit n) = text (show n)

pprIfaceCoercion, pprParendIfaceCoercion :: IfaceCoercion -> SDoc
pprIfaceCoercion = ppr_co TopPrec
pprParendIfaceCoercion = ppr_co TyConPrec

ppr_co :: TyPrec -> IfaceCoercion -> SDoc
ppr_co _         (IfaceReflCo r ty) = angleBrackets (ppr ty) <> ppr_role r
ppr_co ctxt_prec (IfaceFunCo r co1 co2)
  = maybeParen ctxt_prec FunPrec $
    sep (ppr_co FunPrec co1 : ppr_fun_tail co2)
  where
    ppr_fun_tail (IfaceFunCo r co1 co2)
      = (arrow <> ppr_role r <+> ppr_co FunPrec co1) : ppr_fun_tail co2
    ppr_fun_tail other_co
      = [arrow <> ppr_role r <+> pprIfaceCoercion other_co]

ppr_co _         (IfaceTyConAppCo r tc cos)
  = parens (pprIfaceCoTcApp TopPrec tc cos) <> ppr_role r
ppr_co ctxt_prec (IfaceAppCo co1 co2)
  = maybeParen ctxt_prec TyConPrec $
    ppr_co FunPrec co1 <+> pprParendIfaceCoercion co2
ppr_co ctxt_prec co@(IfaceForAllCo _ _)
  = maybeParen ctxt_prec FunPrec (sep [ppr_tvs, pprIfaceCoercion inner_co])
  where
    (tvs, inner_co) = split_co co
    ppr_tvs = ptext (sLit "forall") <+> pprIfaceTvBndrs tvs <> dot

    split_co (IfaceForAllCo tv co')
      = let (tvs, co'') = split_co co' in (tv:tvs,co'')
    split_co co' = ([], co')

ppr_co _         (IfaceCoVarCo covar)       = ppr covar

ppr_co ctxt_prec (IfaceUnivCo s r ty1 ty2)
  = maybeParen ctxt_prec TyConPrec $
    ptext (sLit "UnivCo") <+> ftext s <+> ppr r <+>
    pprParendIfaceType ty1 <+> pprParendIfaceType ty2

ppr_co ctxt_prec (IfaceInstCo co ty)
  = maybeParen ctxt_prec TyConPrec $
    ptext (sLit "Inst") <+> pprParendIfaceCoercion co <+> pprParendIfaceType ty

ppr_co ctxt_prec (IfaceAxiomRuleCo tc tys cos)
  = maybeParen ctxt_prec TyConPrec
               (sep [ppr tc, nest 4 (sep (map pprParendIfaceType tys ++ map pprParendIfaceCoercion cos))])

ppr_co ctxt_prec co
  = ppr_special_co ctxt_prec doc cos
  where (doc, cos) = case co of
                     { IfaceAxiomInstCo n i cos -> (ppr n <> brackets (ppr i), cos)
                     ; IfaceSymCo co            -> (ptext (sLit "Sym"), [co])
                     ; IfaceTransCo co1 co2     -> (ptext (sLit "Trans"), [co1,co2])
                     ; IfaceNthCo d co          -> (ptext (sLit "Nth:") <> int d,
                                                    [co])
                     ; IfaceLRCo lr co          -> (ppr lr, [co])
                     ; IfaceSubCo co            -> (ptext (sLit "Sub"), [co])
                     ; _                        -> panic "pprIfaceCo" }

ppr_special_co :: TyPrec -> SDoc -> [IfaceCoercion] -> SDoc
ppr_special_co ctxt_prec doc cos
  = maybeParen ctxt_prec TyConPrec
               (sep [doc, nest 4 (sep (map pprParendIfaceCoercion cos))])

ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

-------------------
instance Outputable IfaceTyCon where
  ppr tc = pprPromotionQuote tc <> ppr (ifaceTyConName tc)

pprPromotionQuote :: IfaceTyCon -> SDoc
pprPromotionQuote tc = pprPromotionQuoteI (ifaceTyConInfo tc)

pprPromotionQuoteI  :: IfaceTyConInfo -> SDoc
pprPromotionQuoteI NoIfaceTyConInfo     = empty
pprPromotionQuoteI IfacePromotedDataCon = char '\''
pprPromotionQuoteI IfacePromotedTyCon   = ifPprDebug (char '\'')

instance Outputable IfaceCoercion where
  ppr = pprIfaceCoercion

instance Binary IfaceTyCon where
   put_ bh (IfaceTyCon n i) = put_ bh n >> put_ bh i

   get bh = do n <- get bh
               i <- get bh
               return (IfaceTyCon n i)

instance Binary IfaceTyConInfo where
   put_ bh NoIfaceTyConInfo     = putByte bh 0
   put_ bh IfacePromotedDataCon = putByte bh 1
   put_ bh IfacePromotedTyCon   = putByte bh 2

   get bh =
     do i <- getByte bh
        case i of
          0 -> return NoIfaceTyConInfo
          1 -> return IfacePromotedDataCon
          _ -> return IfacePromotedTyCon

instance Outputable IfaceTyLit where
  ppr = ppr_tylit

instance Binary IfaceTyLit where
  put_ bh (IfaceNumTyLit n)  = putByte bh 1 >> put_ bh n
  put_ bh (IfaceStrTyLit n)  = putByte bh 2 >> put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         _ -> panic ("get IfaceTyLit " ++ show tag)

instance Binary IfaceTcArgs where
  put_ bh tk =
    case tk of
      ITC_Type t ts -> putByte bh 0 >> put_ bh t >> put_ bh ts
      ITC_Kind t ts -> putByte bh 1 >> put_ bh t >> put_ bh ts
      ITC_Nil       -> putByte bh 2

  get bh =
    do c <- getByte bh
       case c of
         0 -> do
           t  <- get bh
           ts <- get bh
           return $! ITC_Type t ts
         1 -> do
           t  <- get bh
           ts <- get bh
           return $! ITC_Kind t ts
         2 -> return ITC_Nil
         _ -> panic ("get IfaceTcArgs " ++ show c)

-------------------
pprIfaceContextArr :: Outputable a => [a] -> SDoc
-- Prints "(C a, D b) =>", including the arrow
pprIfaceContextArr = maybe empty (<+> darrow) . pprIfaceContextMaybe

pprIfaceContext :: Outputable a => [a] -> SDoc
pprIfaceContext = fromMaybe (parens empty) . pprIfaceContextMaybe

pprIfaceContextMaybe :: Outputable a => [a] -> Maybe SDoc
pprIfaceContextMaybe [] = Nothing
pprIfaceContextMaybe [pred] = Just $ ppr pred -- No parens
pprIfaceContextMaybe preds  = Just $ parens (fsep (punctuate comma (map ppr preds)))

instance Binary IfaceType where
    put_ bh (IfaceForAllTy aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (IfaceTyVar ad) = do
            putByte bh 1
            put_ bh ad
    put_ bh (IfaceAppTy ae af) = do
            putByte bh 2
            put_ bh ae
            put_ bh af
    put_ bh (IfaceFunTy ag ah) = do
            putByte bh 3
            put_ bh ag
            put_ bh ah
    put_ bh (IfaceDFunTy ag ah) = do
            putByte bh 4
            put_ bh ag
            put_ bh ah
    put_ bh (IfaceTyConApp tc tys)
      = do { putByte bh 5; put_ bh tc; put_ bh tys }
    put_ bh (IfaceTupleTy s i tys)
      = do { putByte bh 6; put_ bh s; put_ bh i; put_ bh tys }
    put_ bh (IfaceLitTy n)
      = do { putByte bh 7; put_ bh n }

    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      return (IfaceForAllTy aa ab)
              1 -> do ad <- get bh
                      return (IfaceTyVar ad)
              2 -> do ae <- get bh
                      af <- get bh
                      return (IfaceAppTy ae af)
              3 -> do ag <- get bh
                      ah <- get bh
                      return (IfaceFunTy ag ah)
              4 -> do ag <- get bh
                      ah <- get bh
                      return (IfaceDFunTy ag ah)
              5 -> do { tc <- get bh; tys <- get bh
                      ; return (IfaceTyConApp tc tys) }
              6 -> do { s <- get bh; i <- get bh; tys <- get bh
                      ; return (IfaceTupleTy s i tys) }
              _  -> do n <- get bh
                       return (IfaceLitTy n)

instance Binary IfaceCoercion where
  put_ bh (IfaceReflCo a b) = do
          putByte bh 1
          put_ bh a
          put_ bh b
  put_ bh (IfaceFunCo a b c) = do
          putByte bh 2
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceTyConAppCo a b c) = do
          putByte bh 3
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceAppCo a b) = do
          putByte bh 4
          put_ bh a
          put_ bh b
  put_ bh (IfaceForAllCo a b) = do
          putByte bh 5
          put_ bh a
          put_ bh b
  put_ bh (IfaceCoVarCo a) = do
          putByte bh 6
          put_ bh a
  put_ bh (IfaceAxiomInstCo a b c) = do
          putByte bh 7
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceUnivCo a b c d) = do
          putByte bh 8
          put_ bh a
          put_ bh b
          put_ bh c
          put_ bh d
  put_ bh (IfaceSymCo a) = do
          putByte bh 9
          put_ bh a
  put_ bh (IfaceTransCo a b) = do
          putByte bh 10
          put_ bh a
          put_ bh b
  put_ bh (IfaceNthCo a b) = do
          putByte bh 11
          put_ bh a
          put_ bh b
  put_ bh (IfaceLRCo a b) = do
          putByte bh 12
          put_ bh a
          put_ bh b
  put_ bh (IfaceInstCo a b) = do
          putByte bh 13
          put_ bh a
          put_ bh b
  put_ bh (IfaceSubCo a) = do
          putByte bh 14
          put_ bh a
  put_ bh (IfaceAxiomRuleCo a b c) = do
          putByte bh 15
          put_ bh a
          put_ bh b
          put_ bh c

  get bh = do
      tag <- getByte bh
      case tag of
           1 -> do a <- get bh
                   b <- get bh
                   return $ IfaceReflCo a b
           2 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceFunCo a b c
           3 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceTyConAppCo a b c
           4 -> do a <- get bh
                   b <- get bh
                   return $ IfaceAppCo a b
           5 -> do a <- get bh
                   b <- get bh
                   return $ IfaceForAllCo a b
           6 -> do a <- get bh
                   return $ IfaceCoVarCo a
           7 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceAxiomInstCo a b c
           8 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   d <- get bh
                   return $ IfaceUnivCo a b c d
           9 -> do a <- get bh
                   return $ IfaceSymCo a
           10-> do a <- get bh
                   b <- get bh
                   return $ IfaceTransCo a b
           11-> do a <- get bh
                   b <- get bh
                   return $ IfaceNthCo a b
           12-> do a <- get bh
                   b <- get bh
                   return $ IfaceLRCo a b
           13-> do a <- get bh
                   b <- get bh
                   return $ IfaceInstCo a b
           14-> do a <- get bh
                   return $ IfaceSubCo a
           15-> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceAxiomRuleCo a b c
           _ -> panic ("get IfaceCoercion " ++ show tag)

{-
************************************************************************
*                                                                      *
        Conversion from Type to IfaceType
*                                                                      *
************************************************************************
-}

----------------
toIfaceTvBndr :: TyVar -> (IfLclName, IfaceType)
toIfaceTvBndr tyvar   = (occNameFS (getOccName tyvar), toIfaceKind (tyVarKind tyvar))
toIfaceIdBndr :: Id -> (IfLclName, IfaceType)
toIfaceIdBndr id      = (occNameFS (getOccName id),    toIfaceType (idType id))
toIfaceTvBndrs :: [TyVar] -> [(IfLclName, IfaceType)]
toIfaceTvBndrs tyvars = map toIfaceTvBndr tyvars

toIfaceBndr :: Var -> IfaceBndr
toIfaceBndr var
  | isId var  = IfaceIdBndr (toIfaceIdBndr var)
  | otherwise = IfaceTvBndr (toIfaceTvBndr var)

toIfaceKind :: Type -> IfaceType
toIfaceKind = toIfaceType

---------------------
toIfaceType :: Type -> IfaceType
-- Synonyms are retained in the interface type
toIfaceType (TyVarTy tv)      = IfaceTyVar (toIfaceTyVar tv)
toIfaceType (AppTy t1 t2)     = IfaceAppTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (LitTy n)         = IfaceLitTy (toIfaceTyLit n)
toIfaceType (ForAllTy tv t)   = IfaceForAllTy (toIfaceTvBndr tv) (toIfaceType t)
toIfaceType (FunTy t1 t2)
  | isPredTy t1 = IfaceDFunTy (toIfaceType t1) (toIfaceType t2)
  | otherwise   = IfaceFunTy  (toIfaceType t1) (toIfaceType t2)

toIfaceType (TyConApp tc tys)  -- Look for the three sorts of saturated tuple
  | Just sort <- tyConTuple_maybe tc
  , n_tys == arity
  = IfaceTupleTy sort NoIfaceTyConInfo (toIfaceTcArgs tc tys)

  | Just tc' <- isPromotedTyCon_maybe tc
  , Just sort <- tyConTuple_maybe tc'
  , n_tys == arity
  = IfaceTupleTy sort IfacePromotedTyCon (toIfaceTcArgs tc tys)

  | Just dc <- isPromotedDataCon_maybe tc
  , isTupleDataCon dc
  , n_tys == 2*arity
  = IfaceTupleTy BoxedTuple IfacePromotedDataCon (toIfaceTcArgs tc (drop arity tys))

  | otherwise
  = IfaceTyConApp (toIfaceTyCon tc) (toIfaceTcArgs tc tys)
  where
    arity = tyConArity tc
    n_tys = length tys

toIfaceTyVar :: TyVar -> FastString
toIfaceTyVar = occNameFS . getOccName

toIfaceCoVar :: CoVar -> FastString
toIfaceCoVar = occNameFS . getOccName

----------------
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon tc
  = IfaceTyCon tc_name info
  where
    tc_name = tyConName tc
    info | isPromotedDataCon tc = IfacePromotedDataCon
         | isPromotedTyCon tc   = IfacePromotedTyCon
         | otherwise            = NoIfaceTyConInfo

toIfaceTyCon_name :: Name -> IfaceTyCon
toIfaceTyCon_name n = IfaceTyCon n NoIfaceTyConInfo
  -- Used for the "rough-match" tycon stuff,
  -- where pretty-printing is not an issue

toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceTyLit (NumTyLit x) = IfaceNumTyLit x
toIfaceTyLit (StrTyLit x) = IfaceStrTyLit x

----------------
toIfaceTypes :: [Type] -> [IfaceType]
toIfaceTypes ts = map toIfaceType ts

----------------
toIfaceContext :: ThetaType -> IfaceContext
toIfaceContext = toIfaceTypes

----------------
toIfaceCoercion :: Coercion -> IfaceCoercion
toIfaceCoercion (Refl r ty)         = IfaceReflCo r (toIfaceType ty)
toIfaceCoercion (TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [arg,res] <- cos                = IfaceFunCo r (toIfaceCoercion arg) (toIfaceCoercion res)
  | otherwise                       = IfaceTyConAppCo r (toIfaceTyCon tc)
                                                      (map toIfaceCoercion cos)
toIfaceCoercion (AppCo co1 co2)     = IfaceAppCo  (toIfaceCoercion co1)
                                                  (toIfaceCoercion co2)
toIfaceCoercion (ForAllCo v co)     = IfaceForAllCo (toIfaceTvBndr v)
                                                    (toIfaceCoercion co)
toIfaceCoercion (CoVarCo cv)        = IfaceCoVarCo  (toIfaceCoVar cv)
toIfaceCoercion (AxiomInstCo con ind cos)
                                    = IfaceAxiomInstCo (coAxiomName con) ind
                                                       (map toIfaceCoercion cos)
toIfaceCoercion (UnivCo s r ty1 ty2)= IfaceUnivCo s r (toIfaceType ty1)
                                                  (toIfaceType ty2)
toIfaceCoercion (SymCo co)          = IfaceSymCo (toIfaceCoercion co)
toIfaceCoercion (TransCo co1 co2)   = IfaceTransCo (toIfaceCoercion co1)
                                                   (toIfaceCoercion co2)
toIfaceCoercion (NthCo d co)        = IfaceNthCo d (toIfaceCoercion co)
toIfaceCoercion (LRCo lr co)        = IfaceLRCo lr (toIfaceCoercion co)
toIfaceCoercion (InstCo co ty)      = IfaceInstCo (toIfaceCoercion co)
                                                  (toIfaceType ty)
toIfaceCoercion (SubCo co)          = IfaceSubCo (toIfaceCoercion co)

toIfaceCoercion (AxiomRuleCo co ts cs) = IfaceAxiomRuleCo
                                          (coaxrName co)
                                          (map toIfaceType ts)
                                          (map toIfaceCoercion cs)
