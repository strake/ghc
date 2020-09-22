{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeFamilies     #-}

#include "lens.h"

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998

-}

-- | Specialisations of the @HsSyn@ syntax for the typechecker
--
-- This module is an extension of @HsSyn@ syntax, for use in the type checker.
module GHC.Tc.Utils.Zonk (
        -- * Extracting types from HsSyn
        hsLitType, hsPatType, hsLPatType,

        -- * Other HsSyn functions
        mkHsDictLet, mkHsApp,
        mkHsAppTy, mkHsCaseAlt,
        shortCutLit, hsOverLitName,
        conLikeResTy,

        -- * re-exported from TcMonad
        TcId, TcIdSet,

        -- * Zonking
        -- | For a description of "zonking", see Note [What is zonking?]
        -- in "GHC.Tc.Utils.TcMType"
        zonkTopDecls, zonkTopExpr, zonkTopLExpr,
        zonkTopBndrs,
        ZonkEnv, ZonkFlexi(..), emptyZonkEnv, mkEmptyZonkEnv, initZonkEnv,
        zonkTyVarBinders, zonkTyVarBindersX, zonkTyVarBinderX,
        zonkTyBndrs, zonkTyBndrsX,
        zonkTcTypeToType,  zonkTcTypeToTypeX,
        zonkTcTypesToTypes, zonkTcTypesToTypesX,
        zonkTyVarOcc,
        zonkCoToCo,
        zonkEvBinds, zonkTcEvBinds,
        zonkTcMethInfoToMethInfoX,
        lookupTyVarOcc
  ) where

import GHC.Prelude

import GHC.Platform

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names

import GHC.Hs

import {-# SOURCE #-} GHC.Tc.Gen.Splice (runTopSplice)
import GHC.Tc.Utils.Monad
import GHC.Tc.TyCl.Build ( TcMethInfo, MethInfo )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.Env   ( tcLookupGlobalOnly )
import GHC.Tc.Types.Evidence

import GHC.Core.TyCo.Ppr ( pprTyVar )
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.ConLike
import GHC.Core.DataCon

import GHC.Utils.Constants ( debugIsOn )
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import GHC.Core
import GHC.Core.Predicate

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Id as Id
import GHC.Types.Id.Info
import GHC.Types.TypeEnv
import GHC.Types.SourceText
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Types.TyThing

import GHC.Data.Maybe
import GHC.Data.Bag

import Control.Monad
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( StateT (..), evalStateT, get, modify )
import Data.Functor.Compose ( Compose (..) )
import Data.Tuple ( swap )
import Lens.Micro.Extras ( view )

{-
************************************************************************
*                                                                      *
       Extracting the type from HsSyn
*                                                                      *
************************************************************************

-}

hsLPatType :: LPat GhcTc -> Type
hsLPatType (L _ p) = hsPatType p

hsPatType :: Pat GhcTc -> Type
hsPatType (ParPat _ pat)                = hsLPatType pat
hsPatType (WildPat ty)                  = ty
hsPatType (VarPat _ lvar)               = idType (unLoc lvar)
hsPatType (BangPat _ pat)               = hsLPatType pat
hsPatType (LazyPat _ pat)               = hsLPatType pat
hsPatType (LitPat _ lit)                = hsLitType lit
hsPatType (AsPat _ var _)               = idType (unLoc var)
hsPatType (ViewPat ty _ _)              = ty
hsPatType (ListPat (ListPatTc ty Nothing) _)      = mkListTy ty
hsPatType (ListPat (ListPatTc _ (Just (ty,_))) _) = ty
hsPatType (TuplePat tys _ bx)           = mkTupleTy1 bx tys
                  -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
hsPatType (SumPat tys _ _ _ )           = mkSumTy tys
hsPatType (ConPat { pat_con = lcon
                  , pat_con_ext = ConPatTc
                    { cpt_arg_tys = tys
                    }
                  })
                                        = conLikeResTy (unLoc lcon) tys
hsPatType (SigPat ty _ _)               = ty
hsPatType (NPat ty _ _ _)               = ty
hsPatType (XPat (CoPat _ _ ty))         = ty
hsPatType SplicePat{}                   = panic "hsPatType: SplicePat"

hsLitType :: HsLit (GhcPass p) -> TcType
hsLitType (HsChar _ _)       = charTy
hsLitType (HsCharPrim _ _)   = charPrimTy
hsLitType (HsString _ _)     = stringTy
hsLitType (HsStringPrim _ _) = addrPrimTy
hsLitType (HsInt _ _)        = intTy
hsLitType (HsIntPrim _ _)    = intPrimTy
hsLitType (HsWordPrim _ _)   = wordPrimTy
hsLitType (HsInt64Prim _ _)  = int64PrimTy
hsLitType (HsWord64Prim _ _) = word64PrimTy
hsLitType (HsInteger _ _ ty) = ty
hsLitType (HsRat _ _ ty)     = ty
hsLitType (HsFloatPrim _ _)  = floatPrimTy
hsLitType (HsDoublePrim _ _) = doublePrimTy

-- Overloaded literals. Here mainly because it uses isIntTy etc

shortCutLit :: Platform -> OverLitVal -> TcType -> Maybe (HsExpr GhcTc)
shortCutLit platform (HsIntegral int@(IL src neg i)) ty
  | isIntTy ty  && platformInIntRange  platform i = Just (HsLit noExtField (HsInt noExtField int))
  | isWordTy ty && platformInWordRange platform i = Just (mkLit wordDataCon (HsWordPrim src i))
  | isIntegerTy ty = Just (HsLit noExtField (HsInteger src i ty))
  | otherwise = shortCutLit platform (HsFractional (integralFractionalLit neg i)) ty
        -- The 'otherwise' case is important
        -- Consider (3 :: Float).  Syntactically it looks like an IntLit,
        -- so we'll call shortCutIntLit, but of course it's a float
        -- This can make a big difference for programs with a lot of
        -- literals, compiled without -O

shortCutLit _ (HsFractional f) ty
  | isFloatTy ty  = Just (mkLit floatDataCon  (HsFloatPrim noExtField f))
  | isDoubleTy ty = Just (mkLit doubleDataCon (HsDoublePrim noExtField f))
  | otherwise     = Nothing

shortCutLit _ (HsIsString src s) ty
  | isStringTy ty = Just (HsLit noExtField (HsString src s))
  | otherwise     = Nothing

mkLit :: DataCon -> HsLit GhcTc -> HsExpr GhcTc
mkLit con lit = HsApp noExtField (nlHsDataCon con) (nlHsLit lit)

------------------------------
hsOverLitName :: OverLitVal -> Name
-- Get the canonical 'fromX' name for a particular OverLitVal
hsOverLitName (HsIntegral {})   = fromIntegerName
hsOverLitName (HsFractional {}) = fromRationalName
hsOverLitName (HsIsString {})   = fromStringName

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
*                                                                      *
************************************************************************

The rest of the zonking is done *after* typechecking.
The main zonking pass runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void
 c) convert each TcId to an Id by zonking its type

The type variables are converted by binding mutable tyvars to immutable ones
and then zonking as normal.

The Ids are converted by binding them in the normal Tc envt; that
way we maintain sharing; eg an Id is zonked at its binding site and they
all occurrences of that Id point to the common zonked copy

It's all pretty boring stuff, because HsSyn is such a large type, and
the environment manipulation is tiresome.
-}

-- Confused by zonking? See Note [What is zonking?] in GHC.Tc.Utils.TcMType.

-- | See Note [The ZonkEnv]
-- Confused by zonking? See Note [What is zonking?] in "GHC.Tc.Utils.TcMType".
data ZonkEnv  -- See Note [The ZonkEnv]
  = ZonkEnv { ze_flexi  :: ZonkFlexi
            , ze_tv_env :: TyCoVarEnv TyCoVar
            , ze_id_env :: IdEnv      Id
            , ze_meta_tv_env :: TcRef (TyVarEnv Type) }

LENS_FIELD(ze_flexiL, ze_flexi)
LENS_FIELD(ze_tv_envL, ze_tv_env)
LENS_FIELD(ze_id_envL, ze_id_env)
LENS_FIELD(ze_meta_tv_envL, ze_meta_tv_env)

{- Note [The ZonkEnv]
~~~~~~~~~~~~~~~~~~~~~
* ze_flexi :: ZonkFlexi says what to do with a
  unification variable that is still un-unified.
  See Note [Un-unified unification variables]

* ze_tv_env :: TyCoVarEnv TyCoVar promotes sharing. At a binding site
  of a tyvar or covar, we zonk the kind right away and add a mapping
  to the env. This prevents re-zonking the kind at every
  occurrence. But this is *just* an optimisation.

* ze_id_env : IdEnv Id promotes sharing among Ids, by making all
  occurrences of the Id point to a single zonked copy, built at the
  binding site.

  Unlike ze_tv_env, it is knot-tied: see extendIdZonkEnvRec.
  In a mutually recursive group
     rec { f = ...g...; g = ...f... }
  we want the occurrence of g to point to the one zonked Id for g,
  and the same for f.

  Because it is knot-tied, we must be careful to consult it lazily.
  Specifically, zonkIdOcc is not monadic.

* ze_meta_tv_env: see Note [Sharing when zonking to Type]


Notes:
  * We must be careful never to put coercion variables (which are Ids,
    after all) in the knot-tied ze_id_env, because coercions can
    appear in types, and we sometimes inspect a zonked type in this
    module.  [Question: where, precisely?]

  * In zonkTyVarOcc we consult ze_tv_env in a monadic context,
    a second reason that ze_tv_env can't be monadic.

  * An obvious suggestion would be to have one VarEnv Var to
    replace both ze_id_env and ze_tv_env, but that doesn't work
    because of the knot-tying stuff mentioned above.

Note [Un-unified unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we do if we find a Flexi unification variable?
There are three possibilities:

* DefaultFlexi: this is the common case, in situations like
     length @alpha ([] @alpha)
  It really doesn't matter what type we choose for alpha.  But
  we must choose a type!  We can't leave mutable unification
  variables floating around: after typecheck is complete, every
  type variable occurrence must have a binding site.

  So we default it to 'Any' of the right kind.

  All this works for both type and kind variables (indeed
  the two are the same thing).

* SkolemiseFlexi: is a special case for the LHS of RULES.
  See Note [Zonking the LHS of a RULE]

* RuntimeUnkFlexi: is a special case for the GHCi debugger.
  It's a way to have a variable that is not a mutable
  unification variable, but doesn't have a binding site
  either.
-}

data ZonkFlexi   -- See Note [Un-unified unification variables]
  = DefaultFlexi    -- Default unbound unification variables to Any
  | SkolemiseFlexi  -- Skolemise unbound unification variables
                    -- See Note [Zonking the LHS of a RULE]
  | RuntimeUnkFlexi -- Used in the GHCi debugger

instance Outputable ZonkEnv where
  ppr (ZonkEnv { ze_tv_env = tv_env
               , ze_id_env = id_env })
    = text "ZE" <+> braces (vcat
         [ text "ze_tv_env =" <+> ppr tv_env
         , text "ze_id_env =" <+> ppr id_env ])

-- The EvBinds have to already be zonked, but that's usually the case.
emptyZonkEnv :: TcM ZonkEnv
emptyZonkEnv = mkEmptyZonkEnv DefaultFlexi

mkEmptyZonkEnv :: ZonkFlexi -> TcM ZonkEnv
mkEmptyZonkEnv flexi
  = do { mtv_env_ref <- newMutVar emptyVarEnv
       ; return (ZonkEnv { ze_flexi = flexi
                         , ze_tv_env = emptyVarEnv
                         , ze_id_env = emptyVarEnv
                         , ze_meta_tv_env = mtv_env_ref }) }

initZonkEnv :: (ZonkEnv -> TcM b) -> TcM b
initZonkEnv thing_inside = do { ze <- mkEmptyZonkEnv DefaultFlexi
                              ; thing_inside ze }

-- | Extend the knot-tied environment.
extendIdZonkEnvRec :: ZonkEnv -> [Var] -> ZonkEnv
extendIdZonkEnvRec ze@(ZonkEnv { ze_id_env = id_env }) ids
    -- NB: Don't look at the var to decide which env't to put it in. That
    -- would end up knot-tying all the env'ts.
  = ze { ze_id_env = extendVarEnvList id_env [(id,id) | id <- ids] }
  -- Given coercion variables will actually end up here. That's OK though:
  -- coercion variables are never looked up in the knot-tied env't, so zonking
  -- them simply doesn't get optimised. No one gets hurt. An improvement (?)
  -- would be to do SCC analysis in zonkEvBinds and then only knot-tie the
  -- recursive groups. But perhaps the time it takes to do the analysis is
  -- more than the savings.

extendZonkEnv :: ZonkEnv -> [Var] -> ZonkEnv
extendZonkEnv ze@(ZonkEnv { ze_tv_env = tyco_env, ze_id_env = id_env }) vars
  = ze { ze_tv_env = extendVarEnvList tyco_env [(tv,tv) | tv <- tycovars]
       , ze_id_env = extendVarEnvList id_env   [(id,id) | id <- ids] }
  where
    (tycovars, ids) = partition isTyCoVar vars

extendIdZonkEnv :: ZonkEnv -> Var -> ZonkEnv
extendIdZonkEnv = flip $ over ze_id_envL . join (slipr extendVarEnv)

extendTyZonkEnv :: ZonkEnv -> TyVar -> ZonkEnv
extendTyZonkEnv = flip $ over ze_tv_envL . join (slipr extendVarEnv)

zonkEnvIds :: ZonkEnv -> TypeEnv
zonkEnvIds (ZonkEnv { ze_id_env = id_env})
  = mkNameEnv [(getName id, AnId id) | id <- nonDetEltsUFM id_env]
  -- It's OK to use nonDetEltsUFM here because we forget the ordering
  -- immediately by creating a TypeEnv

zonkIdOcc :: ZonkEnv -> TcId -> Id
-- Ids defined in this module should be in the envt;
-- ignore others.  (Actually, data constructors are also
-- not LocalVars, even when locally defined, but that is fine.)
-- (Also foreign-imported things aren't currently in the ZonkEnv;
--  that's ok because they don't need zonking.)
--
-- Actually, Template Haskell works in 'chunks' of declarations, and
-- an earlier chunk won't be in the 'env' that the zonking phase
-- carries around.  Instead it'll be in the tcg_gbl_env, already fully
-- zonked.  There's no point in looking it up there (except for error
-- checking), and it's not conveniently to hand; hence the simple
-- 'orElse' case in the LocalVar branch.
--
-- Even without template splices, in module Main, the checking of
-- 'main' is done as a separate chunk.
zonkIdOcc (ZonkEnv { ze_id_env = id_env}) id
  | isLocalVar id = lookupVarEnv id_env id `orElse`
                    id
  | otherwise     = id

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give
zonkIdBndr :: ZonkEnv -> TcId -> TcM Id
zonkIdBndr env v =
  [ modifyIdInfo (`setLevityInfoWithType` ty') (set Id.idTypeL ty' v)
  | ty' <- zonkTcTypeToTypeX env (idType v)
  , () <- ensureNotLevPoly ty' (text "In the type of binder" <+> quotes (ppr v)) ]

zonkTopBndrs :: Traversable f => f TcId -> TcM (f Id)
zonkTopBndrs ids = initZonkEnv $ \ ze -> zonkIdBndr ze `traverse` ids

zonkFieldOcc :: ZonkEnv -> FieldOcc GhcTc -> TcM (FieldOcc GhcTc)
zonkFieldOcc env (FieldOcc sel lbl) = flip FieldOcc lbl <$> zonkIdBndr env sel

zonkEvBndrsX :: Traversable t => t EvVar -> StateT ZonkEnv TcM (t Var)
zonkEvBndrsX = traverse zonkEvBndrX

zonkEvBndrX :: EvVar -> StateT ZonkEnv TcM EvVar
-- Works for dictionaries and coercions
zonkEvBndrX var = StateT \ env ->
  [ (var', extendZonkEnv env [var']) | var' <- zonkEvBndr env var ]

zonkEvBndr :: ZonkEnv -> EvVar -> TcM EvVar
-- Works for dictionaries and coercions
-- Does not extend the ZonkEnv
zonkEvBndr env
  = varTypeL ({-# SCC "zonkEvBndr_zonkTcTypeToType" #-} zonkTcTypeToTypeX env)

{-
zonkEvVarOcc :: ZonkEnv -> EvVar -> TcM EvTerm
zonkEvVarOcc env v
  | isCoVar v
  = EvCoercion <$> zonkCoVarOcc env v
  | otherwise
  = return (EvId $ zonkIdOcc env v)
-}

zonkCoreBndrX :: Var -> StateT ZonkEnv TcM Var
zonkCoreBndrX v
  | isId v = StateT \ env -> [ (v', extendIdZonkEnv env v') | v' <- zonkIdBndr env v ]
  | otherwise = zonkTyBndrX v

zonkCoreBndrsX :: Traversable t => t Var -> StateT ZonkEnv TcM (t Var)
zonkCoreBndrsX = traverse zonkCoreBndrX

zonkTyBndrs :: Traversable t => t TcTyVar -> TcM (ZonkEnv, t TyVar)
zonkTyBndrs tvs = initZonkEnv $ (fmap . fmap) swap $ runStateT $ zonkTyBndrsX tvs

zonkTyBndrsX :: Traversable t => t TcTyVar -> StateT ZonkEnv TcM (t TyVar)
zonkTyBndrsX = traverse zonkTyBndrX

zonkTyBndrX :: TcTyVar -> StateT ZonkEnv TcM TyVar
-- This guarantees to return a TyVar (not a TcTyVar)
-- then we add it to the envt, so all occurrences are replaced
--
-- It does not clone: the new TyVar has the sane Name
-- as the old one.  This important when zonking the
-- TyVarBndrs of a TyCon, whose Names may scope.
zonkTyBndrX tv = StateT \ env ->
    assertPpr (isImmutableTyVar tv) (ppr tv <+> dcolon <+> ppr (tyVarKind tv)) $
  [ (tv', extendTyZonkEnv env tv')
  | ki <- zonkTcTypeToTypeX env (tyVarKind tv)
    -- Internal names tidy up better, for iface files.
  , let tv' = mkTyVar (tyVarName tv) ki ]

zonkTyVarBinders :: [VarBndr TcTyVar vis]
                 -> TcM (ZonkEnv, [VarBndr TyVar vis])
zonkTyVarBinders tvbs = initZonkEnv $ (fmap . fmap) swap $ runStateT $ zonkTyVarBindersX tvbs

zonkTyVarBindersX :: Traversable t => t (VarBndr TcTyVar vis) -> StateT ZonkEnv TcM (t (VarBndr TyVar vis))
zonkTyVarBindersX = traverse zonkTyVarBinderX

zonkTyVarBinderX :: VarBndr TcTyVar vis -> StateT ZonkEnv TcM (VarBndr TyVar vis)
-- Takes a TcTyVar and guarantees to return a TyVar
zonkTyVarBinderX (Bndr tv vis) = flip Bndr vis <$> zonkTyBndrX tv

zonkTopExpr :: HsExpr GhcTc -> TcM (HsExpr GhcTc)
zonkTopExpr e = initZonkEnv $ \ ze -> zonkExpr ze e

zonkTopLExpr :: LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
zonkTopLExpr e = initZonkEnv $ \ ze -> zonkLExpr ze e

zonkTopDecls :: Bag EvBind
             -> LHsBinds GhcTc
             -> [LRuleDecl GhcTc] -> [LTcSpecPrag]
             -> [LForeignDecl GhcTc]
             -> TcM (TypeEnv,
                     Bag EvBind,
                     LHsBinds GhcTc,
                     [LForeignDecl GhcTc],
                     [LTcSpecPrag],
                     [LRuleDecl    GhcTc])
zonkTopDecls ev_binds binds rules imp_specs fords =
  [ (zonkEnvIds env2, ev_binds', binds', fords', specs', rules')
  | (env1, ev_binds') <- initZonkEnv $ (fmap . fmap) swap $ runStateT $ zonkEvBinds ev_binds
  , (binds', env2)    <- zonkRecMonoBinds binds `runStateT` env1
    -- Top level is implicitly recursive
  , rules' <- traverse (wrapLocM (zonkRule env2)) rules
  , specs' <- zonkLTcSpecPrags env2 imp_specs
  , fords' <- zonkForeignExports env2 fords
  ]

---------------------------------------------
zonkLocalBinds :: HsLocalBinds GhcTc -> StateT ZonkEnv TcM (HsLocalBinds GhcTc)
zonkLocalBinds (EmptyLocalBinds x) = pure (EmptyLocalBinds x)

zonkLocalBinds (HsValBinds _ (ValBinds {}))
  = panic "zonkLocalBinds" -- Not in typechecker output

zonkLocalBinds (HsValBinds x (XValBindsLR (NValBinds binds sigs))) =
  [ HsValBinds x (XValBindsLR (NValBinds new_binds sigs))
  | Compose new_binds <- traverse zonkRecMonoBinds (Compose binds) ]

zonkLocalBinds (HsIPBinds x (IPBinds dict_binds binds )) = StateT \ env -> do
  { let zonk_ip_bind (IPBind x n e) = IPBind x <$> traverse (zonkIdBndr env) n <*> zonkLExpr env e
  ; new_binds <- traverse (wrapLocM zonk_ip_bind) binds
  ; let env1 = extendIdZonkEnvRec env [ n | (L _ (IPBind _ (Right n) _)) <- new_binds]
  ; [ HsIPBinds x (IPBinds new_dict_binds new_binds)
    | new_dict_binds <- zonkTcEvBinds dict_binds ] `runStateT` env1 }

---------------------------------------------
zonkRecMonoBinds :: LHsBinds GhcTc -> StateT ZonkEnv TcM (LHsBinds GhcTc)
zonkRecMonoBinds binds = mfix \ new_binds -> do
  { modify (`extendIdZonkEnvRec` collectHsBindsBinders new_binds)
  ; statify1 (traverse . wrapLocM . zonk_bind) binds }

---------------------------------------------
zonk_bind :: ZonkEnv -> HsBind GhcTc -> TcM (HsBind GhcTc)
zonk_bind env bind@(PatBind { pat_lhs = pat, pat_rhs = grhss
                            , pat_ext = NPatBindTc fvs ty})
  = do  { new_pat <- zonkPat pat `evalStateT` env -- Env already extended
        ; new_grhss <- zonkGRHSs env zonkLExpr grhss
        ; new_ty    <- zonkTcTypeToTypeX env ty
        ; return (bind { pat_lhs = new_pat, pat_rhs = new_grhss
                       , pat_ext = NPatBindTc fvs new_ty }) }

zonk_bind env (VarBind { var_ext = x
                       , var_id = var, var_rhs = expr })
  = do { new_var  <- zonkIdBndr env var
       ; new_expr <- zonkLExpr env expr
       ; return (VarBind { var_ext = x
                         , var_id = new_var
                         , var_rhs = new_expr }) }

zonk_bind env bind@(FunBind { fun_id = L loc var
                            , fun_matches = ms
                            , fun_ext = co_fn })
  = do { new_var <- zonkIdBndr env var
       ; (new_co_fn, env1) <- zonkCoFn co_fn `runStateT` env
       ; new_ms <- zonkMatchGroup env1 zonkLExpr ms
       ; return (bind { fun_id = L loc new_var
                      , fun_matches = new_ms
                      , fun_ext = new_co_fn }) }

zonk_bind env (AbsBinds { abs_tvs = tyvars, abs_ev_vars = evs
                        , abs_ev_binds = ev_binds
                        , abs_exports = exports
                        , abs_binds = val_binds
                        , abs_sig = has_sig })
  = assert (all isImmutableTyVar tyvars) $
  [ AbsBinds { abs_ext = noExtField
                          , abs_tvs = new_tyvars, abs_ev_vars = new_evs
                          , abs_ev_binds = new_ev_binds
                          , abs_exports = new_exports, abs_binds = new_val_bind
                          , abs_sig = has_sig }
  | (new_tyvars, env0) <- zonkTyBndrsX tyvars `runStateT` env
  , (new_evs, env1) <- zonkEvBndrsX evs `runStateT` env0
  , (new_ev_binds, env2) <- zonkTcEvBinds_s ev_binds `runStateT` env1
  , (new_val_bind, new_exports) <- mfix $ \ ~(new_val_binds, _) ->
         do { let env3 = extendIdZonkEnvRec env2 $
                         collectHsBindsBinders new_val_binds
            ; new_val_binds <- traverse (zonk_val_bind env3) val_binds
            ; new_exports   <- traverse (zonk_export env3) exports
            ; return (new_val_binds, new_exports) } ]
  where
    zonk_val_bind env lbind
      | has_sig
      , (L loc bind@(FunBind { fun_id      = L mloc mono_id
                             , fun_matches = ms
                             , fun_ext     = co_fn })) <- lbind =
      [ L loc $ bind
                  { fun_id      = L mloc new_mono_id
                  , fun_matches = new_ms
                  , fun_ext     = new_co_fn }
      | new_mono_id <- varTypeL (zonkTcTypeToTypeX env) mono_id
                            -- Specifically /not/ zonkIdBndr; we do not
                            -- want to complain about a levity-polymorphic binder
      , (new_co_fn, env') <- zonkCoFn co_fn `runStateT` env
      , new_ms            <- zonkMatchGroup env' zonkLExpr ms ]
      | otherwise
      = wrapLocM (zonk_bind env) lbind   -- The normal case

    zonk_export :: ZonkEnv -> ABExport GhcTc -> TcM (ABExport GhcTc)
    zonk_export env (ABE{ abe_ext = x
                        , abe_wrap = wrap
                        , abe_poly = poly_id
                        , abe_mono = mono_id
                        , abe_prags = prags })
        = do new_poly_id <- zonkIdBndr env poly_id
             new_wrap <- zonkCoFn wrap `evalStateT` env
             new_prags <- zonkSpecPrags env prags
             return (ABE{ abe_ext = x
                        , abe_wrap = new_wrap
                        , abe_poly = new_poly_id
                        , abe_mono = zonkIdOcc env mono_id
                        , abe_prags = new_prags })

zonk_bind env (PatSynBind x bind@(PSB { psb_id = L loc id
                                      , psb_args = details
                                      , psb_def = lpat
                                      , psb_dir = dir }))
  = do { id' <- zonkIdBndr env id
       ; (lpat', env1) <- zonkPat lpat `runStateT` env
       ; let details' = zonkPatSynDetails env1 details
       ; dir' <- zonkPatSynDir dir `evalStateT` env1
       ; return $ PatSynBind x $
                  bind { psb_id = L loc id'
                       , psb_args = details'
                       , psb_def = lpat'
                       , psb_dir = dir' } }

zonkPatSynDetails :: ZonkEnv
                  -> HsPatSynDetails (Located TcId)
                  -> HsPatSynDetails (Located Id)
zonkPatSynDetails env (PrefixCon as) = PrefixCon ((fmap . fmap) (zonkIdOcc env) as)
zonkPatSynDetails env (InfixCon a1 a2) = InfixCon (zonkIdOcc env <$> a1) (zonkIdOcc env <$> a2)
zonkPatSynDetails env (RecCon flds) = RecCon ((fmap . fmap . fmap) (zonkIdOcc env) flds)

zonkPatSynDir :: HsPatSynDir GhcTc -> StateT ZonkEnv TcM (HsPatSynDir GhcTc)
zonkPatSynDir Unidirectional        = pure Unidirectional
zonkPatSynDir ImplicitBidirectional = pure ImplicitBidirectional
zonkPatSynDir (ExplicitBidirectional mg) = statify \ env ->
    ExplicitBidirectional <$> zonkMatchGroup env zonkLExpr mg

zonkSpecPrags :: ZonkEnv -> TcSpecPrags -> TcM TcSpecPrags
zonkSpecPrags _   IsDefaultMethod = pure IsDefaultMethod
zonkSpecPrags env (SpecPrags ps)  = SpecPrags <$> zonkLTcSpecPrags env ps

zonkLTcSpecPrags :: Traversable t => ZonkEnv -> t LTcSpecPrag -> TcM (t LTcSpecPrag)
zonkLTcSpecPrags env = (traverse . traverse) \ (SpecPrag id co_fn inl) ->
    [ SpecPrag (zonkIdOcc env id) co_fn' inl | co_fn' <- zonkCoFn co_fn `evalStateT` env ]

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
*                                                                      *
************************************************************************
-}

zonkMatchGroup :: ZonkEnv
            -> (ZonkEnv -> Located (body GhcTc) -> TcM (Located (body GhcTc)))
            -> MatchGroup GhcTc (Located (body GhcTc))
            -> TcM (MatchGroup GhcTc (Located (body GhcTc)))
zonkMatchGroup env zBody MG
  { mg_alts = L l ms, mg_ext = MatchGroupTc arg_tys res_ty, mg_origin = origin } =
  [ MG { mg_alts = L l ms', mg_ext = MatchGroupTc arg_tys' res_ty', mg_origin = origin }
  | ms' <- traverse (zonkMatch env zBody) ms
  , arg_tys' <- zonkTcTypesToTypesX env arg_tys
  , res_ty'  <- zonkTcTypeToTypeX env res_ty ]

zonkMatch :: ZonkEnv
          -> (ZonkEnv -> Located (body GhcTc) -> TcM (Located (body GhcTc)))
          -> LMatch GhcTc (Located (body GhcTc))
          -> TcM (LMatch GhcTc (Located (body GhcTc)))
zonkMatch env zBody (L loc match@(Match { m_pats = pats
                                        , m_grhss = grhss })) =
  [ L loc match { m_pats = new_pats, m_grhss = new_grhss }
  | (new_pats, env1) <- zonkPats pats `runStateT` env
  , new_grhss <- zonkGRHSs env1 zBody grhss ]

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv
          -> (ZonkEnv -> Located (body GhcTc) -> TcM (Located (body GhcTc)))
          -> GRHSs GhcTc (Located (body GhcTc))
          -> TcM (GRHSs GhcTc (Located (body GhcTc)))
zonkGRHSs env zBody (GRHSs x grhss (L l binds)) =
  [ GRHSs x new_grhss (L l new_binds)
  | (new_binds, new_env) <- zonkLocalBinds binds `runStateT` env
  , let zonk_grhs (GRHS xx guarded rhs)
          = do (new_guarded, env2) <- zonkStmts zonkLExpr guarded `runStateT` new_env
               GRHS xx new_guarded <$> zBody env2 rhs
  , new_grhss <- traverse (wrapLocM zonk_grhs) grhss ]

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
*                                                                      *
************************************************************************
-}

zonkLExpr  :: ZonkEnv -> LHsExpr GhcTc   -> TcM (LHsExpr GhcTc)
zonkExpr   :: ZonkEnv -> HsExpr GhcTc    -> TcM (HsExpr GhcTc)

zonkLExpr  = wrapLocM . zonkExpr

zonkExpr env (HsVar x (L l id))
  = assertPpr (isNothing (isDataConId_maybe id)) (ppr id) $
    pure (HsVar x (L l (zonkIdOcc env id)))

zonkExpr _ e@(HsConLikeOut {}) = return e

zonkExpr _ (HsIPVar x id)
  = return (HsIPVar x id)

zonkExpr _ e@HsOverLabel{} = return e

zonkExpr env (HsLit x (HsRat e f ty))
  = [ HsLit x (HsRat e f new_ty) | new_ty <- zonkTcTypeToTypeX env ty ]

zonkExpr _ (HsLit x lit)
  = return (HsLit x lit)

zonkExpr env (HsOverLit x lit)
  = HsOverLit x <$> zonkOverLit env lit

zonkExpr env (HsLam x matches)
  = HsLam x <$> zonkMatchGroup env zonkLExpr matches

zonkExpr env (HsLamCase x matches)
  = HsLamCase x <$> zonkMatchGroup env zonkLExpr matches

zonkExpr env (HsApp x e1 e2)
  = HsApp x <$> zonkLExpr env e1 <*> zonkLExpr env e2

zonkExpr env (HsAppType ty e t)
  = [ HsAppType new_ty new_e t | new_e <- zonkLExpr env e, new_ty <- zonkTcTypeToTypeX env ty ]
       -- NB: the type is an HsType; can't zonk that!

zonkExpr _ e@(HsRnBracketOut _ _ _)
  = pprPanic "zonkExpr: HsRnBracketOut" (ppr e)

zonkExpr env (HsTcBracketOut x wrap body bs)
  = do wrap' <- traverse zonkQuoteWrap wrap
       HsTcBracketOut x wrap' body <$> traverse (zonk_b env) bs
  where
    zonkQuoteWrap (QuoteWrapper ev ty) = do
        let ev' = zonkIdOcc env ev
        QuoteWrapper ev' <$> zonkTcTypeToTypeX env ty

    zonk_b env' (PendingTcSplice n e) = PendingTcSplice n <$> zonkLExpr env' e

zonkExpr env (HsSpliceE _ (XSplice (HsSplicedT s))) = runTopSplice s >>= zonkExpr env

zonkExpr _ e@(HsSpliceE _ _) = pprPanic "zonkExpr: HsSpliceE" (ppr e)

zonkExpr env (OpApp fixity e1 op e2)
  = OpApp fixity <$> zonkLExpr env e1 <*> zonkLExpr env op <*> zonkLExpr env e2

zonkExpr env (NegApp x expr op) =
  [ NegApp x new_expr new_op
  | (new_op, env') <- zonkSyntaxExpr op `runStateT` env
  , new_expr <- zonkLExpr env' expr ]

zonkExpr env (HsPar x e)
  = HsPar x <$> zonkLExpr env e

zonkExpr env (SectionL x expr op)
  = SectionL x <$> zonkLExpr env expr <*> zonkLExpr env op

zonkExpr env (SectionR x op expr)
  = SectionR x <$> zonkLExpr env op <*> zonkLExpr env expr

zonkExpr env (ExplicitTuple x tup_args boxed) =
  [ ExplicitTuple x new_tup_args boxed
  | new_tup_args <- (traverse . traverse) zonk_tup_arg tup_args ]
  where
    zonk_tup_arg (Present x e) = Present x <$> zonkLExpr env e
    zonk_tup_arg (Missing t) = Missing <$> zonkTcTypeToTypeX env t


zonkExpr env (ExplicitSum args alt arity expr)
  = do new_args <- traverse (zonkTcTypeToTypeX env) args
       ExplicitSum new_args alt arity <$> zonkLExpr env expr

zonkExpr env (HsCase x expr ms)
  = HsCase x <$> zonkLExpr env expr <*> zonkMatchGroup env zonkLExpr ms

zonkExpr env (HsIf x fun e1 e2 e3)
  = do (new_fun, env1) <- zonkSyntaxExpr fun `runStateT` env
       HsIf x new_fun <$> zonkLExpr env1 e1 <*> zonkLExpr env1 e2 <*> zonkLExpr env1 e3

zonkExpr env (HsMultiIf ty alts)
  = flip HsMultiIf <$> traverse (wrapLocM zonk_alt) alts <*> zonkTcTypeToTypeX env ty
  where zonk_alt (GRHS x guard expr)
          = do { (guard', env') <- zonkStmts zonkLExpr guard `runStateT` env
               ; GRHS x guard' <$> zonkLExpr env' expr }

zonkExpr env (HsLet x (L l binds) expr)
  = do (new_binds, new_env) <- zonkLocalBinds binds `runStateT` env
       HsLet x (L l new_binds) <$> zonkLExpr new_env expr

zonkExpr env (HsDo ty do_or_lc (L l stmts)) =
  [ HsDo new_ty do_or_lc (L l new_stmts)
  | new_stmts <- zonkStmts zonkLExpr stmts `evalStateT` env
  , new_ty <- zonkTcTypeToTypeX env ty ]

zonkExpr env (ExplicitList ty wit exprs)
  = do (new_wit, env1) <- traverse zonkSyntaxExpr wit `runStateT` env
       new_ty <- zonkTcTypeToTypeX env1 ty
       ExplicitList new_ty new_wit <$> traverse (zonkLExpr env1) exprs

zonkExpr env expr@(RecordCon { rcon_ext = ext, rcon_flds = rbinds }) =
  [ expr { rcon_ext = ext { rcon_con_expr = new_con_expr }, rcon_flds = new_rbinds }
  | new_con_expr <- zonkExpr env (rcon_con_expr ext)
  , new_rbinds   <- zonkRecFields env rbinds ]

zonkExpr env (RecordUpd { rupd_flds = rbinds
                        , rupd_expr = expr
                        , rupd_ext = RecordUpdTc
                            { rupd_cons = cons, rupd_in_tys = in_tys
                            , rupd_out_tys = out_tys, rupd_wrap = req_wrap }}) =
  [ RecordUpd { rupd_expr = new_expr, rupd_flds =  new_rbinds
                            , rupd_ext = RecordUpdTc
                                { rupd_cons = cons, rupd_in_tys = new_in_tys
                                , rupd_out_tys = new_out_tys
                                , rupd_wrap = new_recwrap }}
  | new_expr    <- zonkLExpr env expr
  , new_in_tys  <- traverse (zonkTcTypeToTypeX env) in_tys
  , new_out_tys <- traverse (zonkTcTypeToTypeX env) out_tys
  , new_rbinds  <- zonkRecUpdFields env rbinds
  , new_recwrap <- zonkCoFn req_wrap `evalStateT` env ]

zonkExpr env (ExprWithTySig _ e ty)
  = [ ExprWithTySig noExtField e' ty | e' <- zonkLExpr env e ]

zonkExpr env (ArithSeq expr wit info)
  = do (new_wit, env1) <- traverse zonkSyntaxExpr wit `runStateT` env
       new_expr <- zonkExpr env expr
       ArithSeq new_expr new_wit <$> zonkArithSeq env1 info

zonkExpr env (HsPragE x prag expr) = HsPragE x prag <$> zonkLExpr env expr

-- arrow notation extensions
zonkExpr env (HsProc x pat body)
  = do  { (new_pat, env1) <- zonkPat pat `runStateT` env
        ; HsProc x new_pat <$> zonkCmdTop env1 body }

-- StaticPointers extension
zonkExpr env (HsStatic fvs expr) = HsStatic fvs <$> zonkLExpr env expr

zonkExpr env (XExpr (HsWrap co_fn expr))
  = do (new_co_fn, env1) <- zonkCoFn co_fn `runStateT` env
       XExpr . HsWrap new_co_fn <$> zonkExpr env1 expr

zonkExpr _ e@(HsUnboundVar {})
  = return e

zonkExpr _ expr = pprPanic "zonkExpr" (ppr expr)

-------------------------------------------------------------------------
{-
Note [Skolems in zonkSyntaxExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider rebindable syntax with something like

  (>>=) :: (forall x. blah) -> (forall y. blah') -> blah''

The x and y become skolems that are in scope when type-checking the
arguments to the bind. This means that we must extend the ZonkEnv with
these skolems when zonking the arguments to the bind. But the skolems
are different between the two arguments, and so we should theoretically
carry around different environments to use for the different arguments.

However, this becomes a logistical nightmare, especially in dealing with
the more exotic Stmt forms. So, we simplify by making the critical
assumption that the uniques of the skolems are different. (This assumption
is justified by the use of newUnique in GHC.Tc.Utils.TcMType.instSkolTyCoVarX.)
Now, we can safely just extend one environment.
-}

-- See Note [Skolems in zonkSyntaxExpr]
zonkSyntaxExpr :: SyntaxExpr GhcTc -> StateT ZonkEnv TcM (SyntaxExpr GhcTc)
zonkSyntaxExpr SyntaxExprTc
  { syn_expr = expr, syn_arg_wraps = arg_wraps, syn_res_wrap = res_wrap } =
  [ SyntaxExprTc
      { syn_expr = expr', syn_arg_wraps = arg_wraps', syn_res_wrap = res_wrap' }
  | res_wrap'  <- zonkCoFn res_wrap
  , expr'      <- statify1 zonkExpr expr
  , arg_wraps' <- traverse zonkCoFn arg_wraps ]
zonkSyntaxExpr NoSyntaxExprTc = pure NoSyntaxExprTc

-------------------------------------------------------------------------

zonkLCmd  :: ZonkEnv -> LHsCmd GhcTc   -> TcM (LHsCmd GhcTc)
zonkCmd   :: ZonkEnv -> HsCmd GhcTc    -> TcM (HsCmd GhcTc)

zonkLCmd = wrapLocM . zonkCmd

zonkCmd env (XCmd (HsWrap w cmd))
  = do { (w', env1) <- zonkCoFn w `runStateT` env
       ; XCmd . HsWrap w' <$> zonkCmd env1 cmd }
zonkCmd env (HsCmdArrApp ty e1 e2 ho rl) =
  [ HsCmdArrApp new_ty new_e1 new_e2 ho rl
  | new_e1 <- zonkLExpr env e1
  , new_e2 <- zonkLExpr env e2
  , new_ty <- zonkTcTypeToTypeX env ty ]

zonkCmd env (HsCmdArrForm x op f fixity args)
  = do new_op <- zonkLExpr env op
       HsCmdArrForm x new_op f fixity <$> traverse (zonkCmdTop env) args

zonkCmd env (HsCmdApp x c e)
  = HsCmdApp x <$> zonkLCmd env c <*> zonkLExpr env e

zonkCmd env (HsCmdLam x matches)
  = HsCmdLam x <$> zonkMatchGroup env zonkLCmd matches

zonkCmd env (HsCmdPar x c)
  = HsCmdPar x <$> zonkLCmd env c

zonkCmd env (HsCmdCase x expr ms)
  = HsCmdCase x <$> zonkLExpr env expr <*> zonkMatchGroup env zonkLCmd ms

zonkCmd env (HsCmdLamCase x ms)
  = HsCmdLamCase x <$> zonkMatchGroup env zonkLCmd ms

zonkCmd env (HsCmdIf x eCond ePred cThen cElse)
  = do { (new_eCond, env1) <- zonkSyntaxExpr eCond `runStateT` env
       ; HsCmdIf x new_eCond <$> zonkLExpr env1 ePred
         <*> zonkLCmd env1 cThen <*> zonkLCmd env1 cElse }

zonkCmd env (HsCmdLet x (L l binds) cmd)
  = do (new_binds, new_env) <- zonkLocalBinds binds `runStateT` env
       HsCmdLet x (L l new_binds) <$> zonkLCmd new_env cmd

zonkCmd env (HsCmdDo ty (L l stmts)) =
  [ HsCmdDo new_ty (L l new_stmts)
  | new_stmts <- zonkStmts zonkLCmd stmts `evalStateT` env
  , new_ty <- zonkTcTypeToTypeX env ty ]


zonkCmdTop :: ZonkEnv -> LHsCmdTop GhcTc -> TcM (LHsCmdTop GhcTc)
zonkCmdTop = wrapLocM . zonk_cmd_top

zonk_cmd_top :: ZonkEnv -> HsCmdTop GhcTc -> TcM (HsCmdTop GhcTc)
zonk_cmd_top env (HsCmdTop (CmdTopTc stack_tys ty ids) cmd) =
  [ HsCmdTop (CmdTopTc new_stack_tys new_ty new_ids) new_cmd
  | new_cmd <- zonkLCmd env cmd
  , new_stack_tys <- zonkTcTypeToTypeX env stack_tys
  , new_ty <- zonkTcTypeToTypeX env ty
  , new_ids <- (traverse . traverse) (zonkExpr env) ids

  , () <- massert (isLiftedTypeKind (tcTypeKind new_stack_tys))
         -- desugarer assumes that this is not levity polymorphic...
         -- but indeed it should always be lifted due to the typing
         -- rules for arrows
  ]

-------------------------------------------------------------------------
zonkCoFn :: HsWrapper -> StateT ZonkEnv TcM HsWrapper
zonkCoFn WpHole   = pure WpHole
zonkCoFn (WpCompose c1 c2) = WpCompose <$> zonkCoFn c1 <*> zonkCoFn c2
zonkCoFn (WpFun c1 c2 t1 d) =
  [ WpFun c1' c2' t1' d
  | c1' <- zonkCoFn c1
  , c2' <- zonkCoFn c2
  , t1' <- statify1 zonkTcTypeToTypeX t1 ]
zonkCoFn (WpCast co)    = WpCast <$> statify1 zonkCoToCo co
zonkCoFn (WpEvLam ev)   = WpEvLam <$> zonkEvBndrX ev
zonkCoFn (WpEvApp arg)  = WpEvApp <$> statify1 zonkEvTerm arg
zonkCoFn (WpTyLam tv)   = assert (isImmutableTyVar tv) $ WpTyLam <$> zonkTyBndrX tv
zonkCoFn (WpTyApp ty)   = WpTyApp <$> statify1 zonkTcTypeToTypeX ty
zonkCoFn (WpLet bs)     = WpLet <$> zonkTcEvBinds bs

-------------------------------------------------------------------------
zonkOverLit :: ZonkEnv -> HsOverLit GhcTc -> TcM (HsOverLit GhcTc)
zonkOverLit env lit@(OverLit {ol_ext = OverLitTc r ty, ol_witness = e }) =
  [ lit { ol_witness = e', ol_ext = OverLitTc r ty' }
  | ty' <- zonkTcTypeToTypeX env ty
  , e' <- zonkExpr env e ]

-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> ArithSeqInfo GhcTc -> TcM (ArithSeqInfo GhcTc)
zonkArithSeq = arithSeqInfoExprsL . zonkLExpr

-------------------------------------------------------------------------
zonkStmts :: Traversable t => (ZonkEnv -> Located (body GhcTc) -> TcM (Located (body GhcTc)))
          -> t (LStmt GhcTc (Located (body GhcTc)))
          -> StateT ZonkEnv TcM (t (LStmt GhcTc (Located (body GhcTc))))
zonkStmts = traverse . wrapLocM . zonkStmt

zonkStmt :: (ZonkEnv -> Located (body GhcTc) -> TcM (Located (body GhcTc)))
         -> Stmt GhcTc (Located (body GhcTc))
         -> StateT ZonkEnv TcM (Stmt GhcTc (Located (body GhcTc)))
zonkStmt _ (ParStmt bind_ty stmts_w_bndrs mzip_op bind_op) = StateT \ env ->
  [ (ParStmt new_bind_ty new_stmts_w_bndrs new_mzip new_bind_op, env2)
  | (new_bind_op, env1) <- zonkSyntaxExpr bind_op `runStateT` env
  , new_bind_ty <- zonkTcTypeToTypeX env1 bind_ty
  , new_stmts_w_bndrs <- traverse (zonk_branch env1) stmts_w_bndrs
  , let new_binders = [b | ParStmtBlock _ _ bs _ <- new_stmts_w_bndrs, b <- bs]
        env2 = extendIdZonkEnvRec env1 new_binders
  , new_mzip <- zonkExpr env2 mzip_op ]
  where
    zonk_branch :: ZonkEnv -> ParStmtBlock GhcTc GhcTc
                -> TcM (ParStmtBlock GhcTc GhcTc)
    zonk_branch env1 (ParStmtBlock x stmts bndrs return_op) =
       [ ParStmtBlock x new_stmts (zonkIdOcc env3 <$> bndrs) new_return
       | (new_stmts, env2)  <- zonkStmts zonkLExpr stmts `runStateT` env1
       , (new_return, env3) <- zonkSyntaxExpr return_op `runStateT` env2 ]

zonkStmt zBody RecStmt
  { recS_stmts = segStmts, recS_later_ids = lvs, recS_rec_ids = rvs
                            , recS_ret_fn = ret_id, recS_mfix_fn = mfix_id
                            , recS_bind_fn = bind_id
                            , recS_ext =
                                       RecStmtTc { recS_bind_ty = bind_ty
                                                 , recS_later_rets = later_rets
                                                 , recS_rec_rets = rec_rets
                                                 , recS_ret_ty = ret_ty} } = StateT \ env ->
  [ (            RecStmt { recS_stmts = new_segStmts, recS_later_ids = new_lvs
                         , recS_rec_ids = new_rvs, recS_ret_fn = new_ret_id
                         , recS_mfix_fn = new_mfix_id, recS_bind_fn = new_bind_id
                         , recS_ext = RecStmtTc
                             { recS_bind_ty = new_bind_ty
                             , recS_later_rets = new_later_rets
                             , recS_rec_rets = new_rec_rets
                             , recS_ret_ty = new_ret_ty } }
    , extendIdZonkEnvRec env3 new_lvs {- Only the lvs are needed -} )
  | (new_bind_id, env1) <- zonkSyntaxExpr bind_id `runStateT` env
  , (new_mfix_id, env2) <- zonkSyntaxExpr mfix_id `runStateT` env1
  , (new_ret_id, env3)  <- zonkSyntaxExpr ret_id `runStateT` env2
  , new_bind_ty <- zonkTcTypeToTypeX env3 bind_ty
  , new_rvs <- zonkIdBndr env3 `traverse` rvs
  , new_lvs <- zonkIdBndr env3 `traverse` lvs
  , new_ret_ty  <- zonkTcTypeToTypeX env3 ret_ty
  , let env4 = extendIdZonkEnvRec env3 new_rvs
  , (new_segStmts, env5) <- zonkStmts zBody segStmts `runStateT` env4
    -- Zonk the ret-expressions in an envt that
    -- has the polymorphic bindings in the envt
  , new_later_rets <- traverse (zonkExpr env5) later_rets
  , new_rec_rets <- traverse (zonkExpr env5) rec_rets
  ]

zonkStmt zBody (BodyStmt ty body then_op guard_op) =
  [ BodyStmt new_ty new_body new_then_op new_guard_op
  | new_then_op  <- zonkSyntaxExpr then_op
  , new_guard_op <- zonkSyntaxExpr guard_op
  , new_body <- statify1 zBody body
  , new_ty   <- statify1 zonkTcTypeToTypeX ty
  ]

zonkStmt zBody (LastStmt x body noret ret_op) =
  [ LastStmt x new_body noret new_ret
  | new_ret <- zonkSyntaxExpr ret_op
  , new_body <- statify1 zBody body ]

zonkStmt _ TransStmt
  { trS_stmts = stmts, trS_bndrs = binderMap
                          , trS_by = by, trS_form = form, trS_using = using
                          , trS_ret = return_op, trS_bind = bind_op
                          , trS_ext = bind_arg_ty
                          , trS_fmap = liftM_op } = StateT \ env ->
  [ (TransStmt { trS_stmts = stmts', trS_bndrs = binderMap'
                               , trS_by = by', trS_form = form, trS_using = using'
                               , trS_ret = return_op', trS_bind = bind_op'
                               , trS_ext = bind_arg_ty'
                               , trS_fmap = liftM_op' }, env3')
  | (bind_op', env1) <- zonkSyntaxExpr bind_op `runStateT` env
  , bind_arg_ty' <- zonkTcTypeToTypeX env1 bind_arg_ty
  , (stmts', env2) <- zonkStmts zonkLExpr stmts `runStateT` env1
  , by'        <- traverse (zonkLExpr env2) by
  , using'     <- zonkLExpr env2 using

  , (return_op', env3) <- zonkSyntaxExpr return_op `runStateT` env2
  , binderMap' <- traverse (zonkBinderMapEntry env3) binderMap
  , liftM_op'  <- zonkExpr env3 liftM_op
  , let env3' = extendIdZonkEnvRec env3 (map snd binderMap')
  ]
  where
    zonkBinderMapEntry env  (oldBinder, newBinder) = do
        let oldBinder' = zonkIdOcc env oldBinder
        (,) oldBinder' <$> zonkIdBndr env newBinder

zonkStmt _ (LetStmt x (L l binds)) = LetStmt x . L l <$> zonkLocalBinds binds

zonkStmt zBody (BindStmt xbs pat body) = StateT \ env ->
  [ (BindStmt XBindStmtTc
      { xbstc_bindOp = new_bind
      , xbstc_boundResultType = new_bind_ty
      , xbstc_failOp = new_fail
      } new_pat new_body, env2)
  | (new_bind, env1) <- zonkSyntaxExpr (xbstc_bindOp xbs) `runStateT` env
  , new_bind_ty <- zonkTcTypeToTypeX env1 (xbstc_boundResultType xbs)
  , new_body <- zBody env1 body
  , (new_pat, env2) <- zonkPat pat `runStateT` env1
  , new_fail <- (zonkSyntaxExpr `traverse` xbstc_failOp xbs) `evalStateT` env1
  ]

-- Scopes: join > ops (in reverse order) > pats (in forward order)
--              > rest of stmts
zonkStmt _zBody (ApplicativeStmt body_ty args mb_join) =
  [ ApplicativeStmt new_body_ty new_args new_mb_join
  | new_mb_join   <- traverse zonkSyntaxExpr mb_join
  , new_args      <- zonk_args args
  , new_body_ty   <- statify1 zonkTcTypeToTypeX body_ty
  ]
  where
    zonk_args args =
      [ zipWithEqual "zonkStmt" (fmap . set argPatL) new_pats (reverse new_args_rev)
      | new_args_rev <- zonk_args_rev (reverse args)
      , new_pats     <- zonkPats (view argPatL . snd <$> args)
      ]

     -- these need to go backward, because if any operators are higher-rank,
     -- later operators may introduce skolems that are in scope for earlier
     -- arguments
    zonk_args_rev ((op, arg) : args) =
      [ (new_op, new_arg) : new_args
      | new_op         <- zonkSyntaxExpr op
      , new_arg        <- statify1 zonk_arg arg
      , new_args       <- zonk_args_rev args
      ]
    zonk_args_rev [] = pure []

    zonk_arg env (ApplicativeArgOne fail_op pat expr isBody) =
      [ ApplicativeArgOne new_fail pat new_expr isBody
      | new_expr <- zonkLExpr env expr
      , new_fail <- for fail_op $ flip evalStateT env . zonkSyntaxExpr ]
    zonk_arg env (ApplicativeArgMany x stmts ret pat ctxt) = evalStateT
      [ ApplicativeArgMany x new_stmts new_ret pat ctxt
      | new_stmts <- zonkStmts zonkLExpr stmts
      , new_ret <- statify1 zonkExpr ret ] env

-------------------------------------------------------------------------
zonkRecFields :: ZonkEnv -> HsRecordBinds GhcTc -> TcM (HsRecordBinds GhcTc)
zonkRecFields env (HsRecFields flds dd) =
  [ HsRecFields flds' dd | flds' <- traverse zonk_rbind flds ]
  where
    zonk_rbind = traverse \ fld ->
      [ fld { hsRecFieldLbl = new_id, hsRecFieldArg = new_expr }
      | new_id   <- wrapLocM (zonkFieldOcc env) (hsRecFieldLbl fld)
      , new_expr <- zonkLExpr env (hsRecFieldArg fld) ]

zonkRecUpdFields
 :: (Traversable s, Traversable t)
 => ZonkEnv -> t (s (HsRecUpdField GhcTc)) -> TcM (t (s (HsRecUpdField GhcTc)))
zonkRecUpdFields env = (traverse . traverse) \ fld ->
  [ fld { hsRecFieldLbl = ambiguousFieldOcc <$> new_id, hsRecFieldArg = new_expr }
  | new_id   <- wrapLocM (zonkFieldOcc env) (hsRecUpdFieldOcc fld)
  , new_expr <- zonkLExpr env (hsRecFieldArg fld) ]

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Pats]{Patterns}
*                                                                      *
************************************************************************
-}

zonkPat :: LPat GhcTc -> StateT ZonkEnv TcM (LPat GhcTc)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat = wrapLocM zonk_pat

zonk_pat :: Pat GhcTc -> StateT ZonkEnv TcM (Pat GhcTc)
zonk_pat (ParPat x p) = ParPat x <$> zonkPat p

zonk_pat (WildPat ty)
  = do  { ty' <- statify1 zonkTcTypeToTypeX ty
        ; lift $ WildPat ty' <$ ensureNotLevPoly ty' (text "In a wildcard pattern") }

zonk_pat (VarPat x (L l v)) = StateT \ env ->
  [ (VarPat x (L l v'), extendIdZonkEnv env v') | v' <- zonkIdBndr env v ]

zonk_pat (LazyPat x pat) = LazyPat x <$> zonkPat pat

zonk_pat (BangPat x pat) = BangPat x <$> zonkPat pat

zonk_pat (AsPat x (L loc v) pat) = do
  { v' <- statify1 zonkIdBndr v
  ; modify (`extendIdZonkEnv` v')
  ; AsPat x (L loc v') <$> zonkPat pat }

zonk_pat (ViewPat ty expr pat) = StateT \ env ->
  [ (ViewPat ty' expr' pat', env')
  | expr' <- zonkLExpr env expr
  , (pat', env') <- zonkPat pat `runStateT` env
  , ty' <- zonkTcTypeToTypeX env ty ]

zonk_pat (ListPat (ListPatTc ty Nothing) pats)
  = do  { ty' <- statify1 zonkTcTypeToTypeX ty
        ; ListPat (ListPatTc ty' Nothing) <$> zonkPats pats }

zonk_pat (ListPat (ListPatTc ty (Just (ty2,wit))) pats)
  = do  { wit' <- zonkSyntaxExpr wit
        ; ty2' <- statify1 zonkTcTypeToTypeX ty2
        ; ty' <- statify1 zonkTcTypeToTypeX ty
        ; ListPat (ListPatTc ty' (Just (ty2',wit'))) <$> zonkPats pats }

zonk_pat (TuplePat tys pats boxed) =
  [ TuplePat tys' pats' boxed
  | tys' <- statify1 (traverse . zonkTcTypeToTypeX) tys
  , pats' <- zonkPats pats ]

zonk_pat (SumPat tys pat alt arity) =
  [ SumPat tys' pat' alt arity
  | tys' <- statify1 (traverse . zonkTcTypeToTypeX) tys
  , pat' <- zonkPat pat ]

zonk_pat p@ConPat { pat_con = L _ con
                       , pat_args = args
                       , pat_con_ext = p'@ConPatTc
                         { cpt_tvs = tyvars
                         , cpt_dicts = evs
                         , cpt_binds = binds
                         , cpt_wrap = wrapper
                         , cpt_arg_tys = tys
                         }
                       }
  = assert (all isImmutableTyVar tyvars)
  [ p            { pat_args = new_args
                 , pat_con_ext = p'
                   { cpt_arg_tys = new_tys
                   , cpt_tvs = new_tyvars
                   , cpt_dicts = new_evs
                   , cpt_binds = new_binds
                   , cpt_wrap = new_wrapper
                   }
                 }
  | new_tys <- statify1 (traverse . zonkTcTypeToTypeX) tys

    -- an unboxed tuple pattern (but only an unboxed tuple pattern)
    -- might have levity-polymorphic arguments. Check for this badness.
  , () <- lift $ case con of
            RealDataCon dc
              | isUnboxedTupleTyCon (dataConTyCon dc)
              -> traverse_ (checkForLevPoly doc) (dropRuntimeRepArgs new_tys)
            _ -> pure ()

  , new_tyvars <- zonkTyBndrsX tyvars
    -- Must zonk the existential variables, because their /kind/ need potential zonking.
    -- cf typecheck/should_compile/tc221.hs
  , new_evs <- zonkEvBndrsX evs
  , new_binds <- zonkTcEvBinds binds
  , new_wrapper <- zonkCoFn wrapper
  , new_args <- zonkConStuff args
  ]
  where
    doc = text "In the type of an element of an unboxed tuple pattern:" $$ ppr p

zonk_pat (LitPat x lit) = pure (LitPat x lit)

zonk_pat (SigPat ty pat hs_ty) =
  [ SigPat ty' pat' hs_ty
  | ty' <- statify1 zonkTcTypeToTypeX ty
  , pat' <- zonkPat pat ]

zonk_pat (NPat ty (L l lit) mb_neg eq_expr) =
  [ NPat ty' (L l lit') mb_neg' eq_expr'
  | eq_expr' <- zonkSyntaxExpr eq_expr
  , mb_neg' <- traverse zonkSyntaxExpr mb_neg
  , lit' <- statify1 zonkOverLit lit
  , ty' <- statify1 zonkTcTypeToTypeX ty ]

zonk_pat (XPat (CoPat co_fn pat ty))
  = do { co_fn' <- zonkCoFn co_fn
       ; pat' <- zonkPat (noLoc pat)
       ; XPat . CoPat co_fn' (unLoc pat') <$> statify1 zonkTcTypeToTypeX ty
       }

zonk_pat pat = pprPanic "zonk_pat" (ppr pat)

---------------------------
zonkConStuff :: HsConDetails (LPat GhcTc) (HsRecFields id (LPat GhcTc))
             -> StateT ZonkEnv TcM (HsConDetails (LPat GhcTc) (HsRecFields id (LPat GhcTc)))
zonkConStuff (PrefixCon pats) = PrefixCon <$> traverse zonkPat pats
zonkConStuff (InfixCon p1 p2) = InfixCon <$> zonkPat p1 <*> zonkPat p2
zonkConStuff (RecCon (HsRecFields rpats dd)) =
  [ RecCon (HsRecFields rpats' dd)
  | pats' <- traverse (zonkPat . hsRecFieldArg . unLoc) rpats
  , let rpats' = zipWith (set (traverse . hsRecFieldArgL)) pats' rpats
    -- Field selectors have declared types; hence no zonking
  ]

---------------------------
zonkPats :: Traversable t => t (LPat GhcTc) -> StateT ZonkEnv TcM (t (LPat GhcTc))
zonkPats = traverse zonkPat

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Foreign]{Foreign exports}
*                                                                      *
************************************************************************
-}

zonkForeignExports :: Traversable t => ZonkEnv -> t (LForeignDecl GhcTc) -> TcM (t (LForeignDecl GhcTc))
zonkForeignExports env = traverse (wrapLocM (zonkForeignExport env))

zonkForeignExport :: ZonkEnv -> ForeignDecl GhcTc -> TcM (ForeignDecl GhcTc)
zonkForeignExport env (ForeignExport { fd_name = i, fd_e_ext = co
                                     , fd_fe = spec })
  = return (ForeignExport { fd_name = zonkIdOcc env <$> i
                          , fd_sig_ty = undefined, fd_e_ext = co
                          , fd_fe = spec })
zonkForeignExport _ for_imp
  = return for_imp     -- Foreign imports don't need zonking

zonkRule :: ZonkEnv -> RuleDecl GhcTc -> TcM (RuleDecl GhcTc)
zonkRule env rule@HsRule
  { rd_tmvs = tm_bndrs{-::[RuleBndr TcId]-}, rd_lhs = lhs, rd_rhs = rhs } =
  [ rule { rd_tmvs = new_tm_bndrs, rd_lhs = new_lhs, rd_rhs = new_rhs }
  | (new_tm_bndrs, env_inside) <- traverse zonk_tm_bndr tm_bndrs `runStateT` env

  , let env_lhs = set ze_flexiL SkolemiseFlexi env_inside
        -- See Note [Zonking the LHS of a RULE]

  , new_lhs <- zonkLExpr env_lhs    lhs
  , new_rhs <- zonkLExpr env_inside rhs
  ]
  where
    zonk_tm_bndr :: LRuleBndr GhcTc -> StateT ZonkEnv TcM (LRuleBndr GhcTc)
    zonk_tm_bndr = traverse \ case
        RuleBndr x (L loc v) -> RuleBndr x . L loc <$> zonk_it v
        RuleBndrSig {} -> panic "zonk_tm_bndr RuleBndrSig"

    zonk_it v
      | isId v     = StateT \ env -> [ (v', extendIdZonkEnvRec env [v']) | v' <- zonkIdBndr env v ]
      | otherwise  = assert (isImmutableTyVar v) zonkTyBndrX v
                     -- DV: used to be return (env,v) but that is plain
                     -- wrong because we may need to go inside the kind
                     -- of v and zonk there!

{-
************************************************************************
*                                                                      *
              Constraints and evidence
*                                                                      *
************************************************************************
-}

zonkEvTerm :: ZonkEnv -> EvTerm -> TcM EvTerm
zonkEvTerm env (EvExpr e)
  = EvExpr <$> zonkCoreExpr env e
zonkEvTerm env (EvTypeable ty ev)
  = EvTypeable <$> zonkTcTypeToTypeX env ty <*> zonkEvTypeable env ev
zonkEvTerm env EvFun { et_tvs = tvs, et_given = evs, et_binds = ev_binds, et_body = body_id } =
  [ EvFun
      { et_tvs = new_tvs, et_given = new_evs, et_binds = new_ev_binds, et_body = new_body_id }
  | new_tvs <- zonkTyBndrsX tvs
  , new_evs <- zonkEvBndrsX evs
  , new_ev_binds <- zonkTcEvBinds ev_binds
  , new_body_id <- flip zonkIdOcc body_id <$> get
  ] `evalStateT` env

zonkCoreExpr :: ZonkEnv -> CoreExpr -> TcM CoreExpr
zonkCoreExpr env (Var v)
    | isCoVar v = Coercion <$> zonkCoVarOcc env v
    | otherwise = pure $ Var (zonkIdOcc env v)
zonkCoreExpr _ (Lit l) = pure $ Lit l
zonkCoreExpr env (Coercion co) = Coercion <$> zonkCoToCo env co
zonkCoreExpr env (Type ty) = Type <$> zonkTcTypeToTypeX env ty

zonkCoreExpr env (Cast e co) = Cast <$> zonkCoreExpr env e <*> zonkCoToCo env co
zonkCoreExpr env (Tick t e) = Tick t <$> zonkCoreExpr env e -- Do we need to zonk in ticks?

zonkCoreExpr env (App e1 e2) = App <$> zonkCoreExpr env e1 <*> zonkCoreExpr env e2
zonkCoreExpr env (Lam v e)
    = do { (v', env1) <- zonkCoreBndrX v `runStateT` env
         ; Lam v' <$> zonkCoreExpr env1 e }
zonkCoreExpr env (Let bind e)
    = do (bind', env1) <- zonkCoreBind bind `runStateT` env
         Let bind' <$> zonkCoreExpr env1 e
zonkCoreExpr env (Case scrut b ty alts)
    = do scrut' <- zonkCoreExpr env scrut
         ty' <- zonkTcTypeToTypeX env ty
         b' <- zonkIdBndr env b
         let env1 = extendIdZonkEnv env b'
         Case scrut' b' ty' <$> traverse (zonkCoreAlt env1) alts

zonkCoreAlt :: ZonkEnv -> CoreAlt -> TcM CoreAlt
zonkCoreAlt env (dc, bndrs, rhs) =
  [ (dc, bndrs', rhs')
  | (bndrs', env1) <- zonkCoreBndrsX bndrs `runStateT` env
  , rhs' <- zonkCoreExpr env1 rhs ]

zonkCoreBind :: CoreBind -> StateT ZonkEnv TcM CoreBind
zonkCoreBind (NonRec v e) = StateT \ env ->
  [ (NonRec v' e', extendIdZonkEnv env v')
  | v' <- zonkIdBndr env v
  , e' <- zonkCoreExpr env e ]
zonkCoreBind (Rec pairs) = Rec <$> mfix \ new_pairs -> do
  { modify (`extendIdZonkEnvRec` fmap fst new_pairs)
  ; statify1 (traverse . zonkCorePair) pairs }

zonkCorePair :: ZonkEnv -> (CoreBndr, CoreExpr) -> TcM (CoreBndr, CoreExpr)
zonkCorePair env (v,e) = (,) <$> zonkIdBndr env v <*> zonkCoreExpr env e

zonkEvTypeable :: ZonkEnv -> EvTypeable -> TcM EvTypeable
zonkEvTypeable env (EvTypeableTyCon tycon e) = EvTypeableTyCon tycon <$> traverse (zonkEvTerm env) e
zonkEvTypeable env (EvTypeableTyApp t1 t2) = EvTypeableTyApp <$> zonkEvTerm env t1 <*> zonkEvTerm env t2
zonkEvTypeable env (EvTypeableTrFun t1 t2) = EvTypeableTrFun <$> zonkEvTerm env t1 <*> zonkEvTerm env t2
zonkEvTypeable env (EvTypeableTyLit t1) = EvTypeableTyLit <$> zonkEvTerm env t1

zonkTcEvBinds_s :: [TcEvBinds] -> StateT ZonkEnv TcM [TcEvBinds]
zonkTcEvBinds_s bs = pure . EvBinds . asum <$> traverse zonk_tc_ev_binds bs

zonkTcEvBinds :: TcEvBinds -> StateT ZonkEnv TcM TcEvBinds
zonkTcEvBinds = fmap EvBinds . zonk_tc_ev_binds

zonk_tc_ev_binds :: TcEvBinds -> StateT ZonkEnv TcM (Bag EvBind)
zonk_tc_ev_binds (TcEvBinds var) = zonkEvBindsVar var
zonk_tc_ev_binds (EvBinds bs)    = zonkEvBinds bs

zonkEvBindsVar :: EvBindsVar -> StateT ZonkEnv TcM (Bag EvBind)
zonkEvBindsVar EvBindsVar { ebv_binds = ref }
  = do { bs <- readMutVar ref
       ; zonkEvBinds (evBindMapBinds bs) }
zonkEvBindsVar CoEvBindsVar {} = pure empty

zonkEvBinds :: Bag EvBind -> StateT ZonkEnv TcM (Bag EvBind)
zonkEvBinds binds
  = {-# SCC "zonkEvBinds" #-} mfix \ new_binds -> do
  { modify (`extendIdZonkEnvRec` collect_ev_bndrs new_binds)
  ; statify1 (traverse . zonkEvBind) binds }
  where
    collect_ev_bndrs :: Bag EvBind -> [EvVar]
    collect_ev_bndrs = foldr add []
    add EvBind { eb_lhs = var } vars = var : vars

zonkEvBind :: ZonkEnv -> EvBind -> TcM EvBind
zonkEvBind env bind@(EvBind { eb_lhs = var, eb_rhs = term }) =
  [ bind { eb_lhs = var', eb_rhs = term' }
  | var'  <- {-# SCC "zonkEvBndr" #-} zonkEvBndr env var

    -- Optimise the common case of Refl coercions
    -- See Note [Optimise coercion zonking]
    -- This has a very big effect on some programs (eg #5030)

  , term' <- case getEqPredTys_maybe (idType var') of
           Just (r, ty1, ty2) | ty1 `eqType` ty2
                  -> return (evCoercion (mkTcReflCo r ty1))
           _other -> zonkEvTerm env term ]

{- Note [Optimise coercion zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When optimising evidence binds we may come across situations where
a coercion looks like
      cv = ReflCo ty
or    cv1 = cv2
where the type 'ty' is big.  In such cases it is a waste of time to zonk both
  * The variable on the LHS
  * The coercion on the RHS
Rather, we can zonk the variable, and if its type is (ty ~ ty), we can just
use Refl on the right, ignoring the actual coercion on the RHS.

This can have a very big effect, because the constraint solver sometimes does go
to a lot of effort to prove Refl!  (Eg when solving  10+3 = 10+3; cf #5030)


************************************************************************
*                                                                      *
                         Zonking types
*                                                                      *
************************************************************************
-}

{- Note [Sharing when zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem:

    In GHC.Tc.Utils.TcMType.zonkTcTyVar, we short-circuit (Indirect ty) to
    (Indirect zty), see Note [Sharing in zonking] in GHC.Tc.Utils.TcMType. But we
    /can't/ do this when zonking a TcType to a Type (#15552, esp
    comment:3).  Suppose we have

       alpha -> alpha
         where
            alpha is already unified:
             alpha := T{tc-tycon} Int -> Int
         and T is knot-tied

    By "knot-tied" I mean that the occurrence of T is currently a TcTyCon,
    but the global env contains a mapping "T" :-> T{knot-tied-tc}. See
    Note [Type checking recursive type and class declarations] in
    GHC.Tc.TyCl.

    Now we call zonkTcTypeToType on that (alpha -> alpha). If we follow
    the same path as Note [Sharing in zonking] in GHC.Tc.Utils.TcMType, we'll
    update alpha to
       alpha := T{knot-tied-tc} Int -> Int

    But alas, if we encounter alpha for a /second/ time, we end up
    looking at T{knot-tied-tc} and fall into a black hole. The whole
    point of zonkTcTypeToType is that it produces a type full of
    knot-tied tycons, and you must not look at the result!!

    To put it another way (zonkTcTypeToType . zonkTcTypeToType) is not
    the same as zonkTcTypeToType. (If we distinguished TcType from
    Type, this issue would have been a type error!)

Solutions: (see #15552 for other variants)

One possible solution is simply not to do the short-circuiting.
That has less sharing, but maybe sharing is rare. And indeed,
that usually turns out to be viable from a perf point of view

But zonkTyVarOcc implements something a bit better

* ZonkEnv contains ze_meta_tv_env, which maps
      from a MetaTyVar (unification variable)
      to a Type (not a TcType)

* In zonkTyVarOcc, we check this map to see if we have zonked
  this variable before. If so, use the previous answer; if not
  zonk it, and extend the map.

* The map is of course stateful, held in a TcRef. (That is unlike
  the treatment of lexically-scoped variables in ze_tv_env and
  ze_id_env.)

* In zonkTyVarOcc we read the TcRef to look up the unification
  variable:
    - if we get a hit we use the zonked result;
    - if not, in zonk_meta we see if the variable is `Indirect ty`,
      zonk that, and update the map (in finish_meta)
  But Nota Bene that the "update map" step must re-read the TcRef
  (or, more precisely, use updMutVar) because the zonking of the
  `Indirect ty` may have added lots of stuff to the map.  See
  #19668 for an example where this made an asymptotic difference!

Is it worth the extra work of carrying ze_meta_tv_env? Some
non-systematic perf measurements suggest that compiler allocation is
reduced overall (by 0.5% or so) but compile time really doesn't
change.  But in some cases it makes a HUGE difference: see test
T9198 and #19668.  So yes, it seems worth it.
-}

zonkTyVarOcc :: ZonkEnv -> TyVar -> TcM TcType
zonkTyVarOcc env@(ZonkEnv { ze_flexi = flexi
                          , ze_tv_env = tv_env
                          , ze_meta_tv_env = mtv_env_ref }) tv
  | isTcTyVar tv
  = case tcTyVarDetails tv of
      SkolemTv {}    -> lookup_in_tv_env
      RuntimeUnk {}  -> lookup_in_tv_env
      MetaTv { mtv_ref = ref }
        -> do { mtv_env <- readMutVar mtv_env_ref
                -- See Note [Sharing when zonking to Type]
              ; case lookupVarEnv mtv_env tv of
                  Just ty -> return ty
                  Nothing -> do { mtv_details <- readMutVar ref
                                ; zonk_meta ref mtv_details } }
  | otherwise
  = lookup_in_tv_env

  where
    lookup_in_tv_env    -- Look up in the env just as we do for Ids
      = case lookupVarEnv tv_env tv of
          Nothing  -> mkTyVarTy <$> tyVarKindL (zonkTcTypeToTypeX env) tv
          Just tv' -> pure (mkTyVarTy tv')

    zonk_meta ref Flexi
      = do { kind <- zonkTcTypeToTypeX env (tyVarKind tv)
           ; ty <- commitFlexi flexi tv kind
           ; writeMetaTyVarRef tv ref ty  -- Belt and braces
           ; finish_meta ty }

    zonk_meta _ (Indirect ty)
      = do { zty <- zonkTcTypeToTypeX env ty
           ; finish_meta zty }

    finish_meta ty = ty <$ updMutVar mtv_env_ref \env -> extendVarEnv env tv ty

lookupTyVarOcc :: ZonkEnv -> TcTyVar -> Maybe TyVar
lookupTyVarOcc ZonkEnv { ze_tv_env = tv_env } = lookupVarEnv tv_env

commitFlexi :: ZonkFlexi -> TcTyVar -> Kind -> TcM Type
-- Only monadic so we can do tc-tracing
commitFlexi flexi tv zonked_kind = case flexi of
      SkolemiseFlexi  -> return (mkTyVarTy (mkTyVar name zonked_kind))

      DefaultFlexi
        | isRuntimeRepTy zonked_kind
        -> liftedRepTy <$ traceTc "Defaulting flexi tyvar to LiftedRep:" (pprTyVar tv)
        | otherwise
        -> anyTypeOfKind zonked_kind <$ traceTc "Defaulting flexi tyvar to Any:" (pprTyVar tv)

      RuntimeUnkFlexi
        -> mkTyVarTy (mkTcTyVar name zonked_kind RuntimeUnk) <$
           traceTc "Defaulting flexi tyvar to RuntimeUnk:" (pprTyVar tv)
                        -- This is where RuntimeUnks are born:
                        -- otherwise-unconstrained unification variables are
                        -- turned into RuntimeUnks as they leave the
                        -- typechecker's monad
  where
     name = tyVarName tv

zonkCoVarOcc :: ZonkEnv -> CoVar -> TcM Coercion
zonkCoVarOcc (ZonkEnv { ze_tv_env = tyco_env }) cv
  | Just cv' <- lookupVarEnv tyco_env cv  -- don't look in the knot-tied env
  = return $ mkCoVarCo cv'
  | otherwise
  = mkCoVarCo <$> zonkCoVar cv

zonkCoHole :: ZonkEnv -> CoercionHole -> TcM Coercion
zonkCoHole env hole@(CoercionHole { ch_ref = ref, ch_co_var = cv })
  = readMutVar ref >>= \ case
           Just co -> do { co' <- zonkCoToCo env co
                         ; checkCoercionHole cv co' }

              -- This next case should happen only in the presence of
              -- (undeferred) type errors. Originally, I put in a panic
              -- here, but that caused too many uses of `failIfErrsM`.
           Nothing -> do { traceTc "Zonking unfilled coercion hole" (ppr hole)
                         ; when debugIsOn $
                           whenNoErrs $
                           massertPpr False
                                      (text "Type-correct unfilled coercion hole"
                                       <+> ppr hole)
                         ; mkCoVarCo <$> zonkCoVar cv }
                             -- This will be an out-of-scope variable, but keeping
                             -- this as a coercion hole led to #15787

zonk_tycomapper :: TyCoMapper ZonkEnv TcM
zonk_tycomapper = TyCoMapper
  { tcm_tyvar      = zonkTyVarOcc
  , tcm_covar      = zonkCoVarOcc
  , tcm_hole       = zonkCoHole
  , tcm_tycobinder = \env tv _vis -> swap <$> runStateT (zonkTyBndrX tv) env
  , tcm_tycon      = zonkTcTyConToTyCon }

-- Zonk a TyCon by changing a TcTyCon to a regular TyCon
zonkTcTyConToTyCon :: TcTyCon -> TcM TyCon
zonkTcTyConToTyCon tc
  | isTcTyCon tc = tcLookupGlobalOnly (getName tc) >>= \ case
                          ATyCon real_tc -> return real_tc
                          thing          -> pprPanic "zonkTcTyCon" (ppr tc $$ ppr thing)
  | otherwise    = return tc -- it's already zonked

-- Confused by zonking? See Note [What is zonking?] in GHC.Tc.Utils.TcMType.
zonkTcTypeToType :: TcType -> TcM Type
zonkTcTypeToType ty = initZonkEnv $ \ ze -> zonkTcTypeToTypeX ze ty

zonkTcTypesToTypes :: [TcType] -> TcM [Type]
zonkTcTypesToTypes tys = initZonkEnv $ \ ze -> zonkTcTypesToTypesX ze tys

zonkTcTypeToTypeX   :: ZonkEnv -> TcType   -> TcM Type
zonkTcTypesToTypesX :: ZonkEnv -> [TcType] -> TcM [Type]
zonkCoToCo          :: ZonkEnv -> Coercion -> TcM Coercion
(zonkTcTypeToTypeX, zonkTcTypesToTypesX, zonkCoToCo, _) = mapTyCoX zonk_tycomapper

zonkTcMethInfoToMethInfoX :: ZonkEnv -> TcMethInfo -> TcM MethInfo
zonkTcMethInfoToMethInfoX ze (name, ty, gdm_spec)
  = (,,) name <$> zonkTcTypeToTypeX ze ty <*> zonk_gdm gdm_spec
  where
    zonk_gdm :: Maybe (DefMethSpec (SrcSpan, TcType))
             -> TcM (Maybe (DefMethSpec (SrcSpan, Type)))
    zonk_gdm Nothing = return Nothing
    zonk_gdm (Just VanillaDM) = return (Just VanillaDM)
    zonk_gdm (Just (GenericDM (loc, ty))) =
      [ Just (GenericDM (loc, ty')) | ty' <- zonkTcTypeToTypeX ze ty ]

---------------------------------------
{- Note [Zonking the LHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also GHC.HsToCore.Binds Note [Free tyvars on rule LHS]

We need to gather the type variables mentioned on the LHS so we can
quantify over them.  Example:
  data T a = C

  foo :: T a -> Int
  foo C = 1

  {-# RULES "myrule"  foo C = 1 #-}

After type checking the LHS becomes (foo alpha (C alpha)) and we do
not want to zap the unbound meta-tyvar 'alpha' to Any, because that
limits the applicability of the rule.  Instead, we want to quantify
over it!

We do this in two stages.

* During zonking, we skolemise the TcTyVar 'alpha' to TyVar 'a'.  We
  do this by using zonkTvSkolemising as the UnboundTyVarZonker in the
  ZonkEnv.  (This is in fact the whole reason that the ZonkEnv has a
  UnboundTyVarZonker.)

* In GHC.HsToCore.Binds, we quantify over it.  See GHC.HsToCore.Binds
  Note [Free tyvars on rule LHS]

Quantifying here is awkward because (a) the data type is big and (b)
finding the free type vars of an expression is necessarily monadic
operation. (consider /\a -> f @ b, where b is side-effected to a)
-}

statify :: Functor m => (s -> m a) -> StateT s m a
statify f = StateT \ s -> flip (,) s <$> f s

statify1 :: Functor m => (s -> a -> m b) -> a -> StateT s m b
statify1 f a = StateT \ s -> flip (,) s <$> f s a
