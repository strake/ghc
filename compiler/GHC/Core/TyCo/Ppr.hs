-- | Pretty-printing types and coercions.
module GHC.Core.TyCo.Ppr
  (
        -- * Precedence
        PprPrec(..), topPrec, sigPrec, opPrec, funPrec, appPrec, maybeParen,

        -- * Pretty-printing types
        pprType, pprParendType, pprTidiedType, pprPrecType, pprPrecTypeX,
        pprTypeApp, pprTCvBndr, pprTCvBndrs,
        pprSigmaType,
        pprTheta, pprParendTheta, pprForAll, pprUserForAll,
        pprTyVar, pprTyVars,
        pprThetaArrowTy, pprClassPred,
        pprKind, pprParendKind, pprTyLit,
        pprDataCons, pprWithExplicitKindsWhen,
        pprWithTYPE, pprSourceTyCon,


        -- * Pretty-printing coercions
        pprCo, pprParendCo,

        debugPprType,
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.CoreToIface
   ( toIfaceTypeX, toIfaceTyLit, toIfaceForAllBndr
   , toIfaceTyCon, toIfaceTcArgs, toIfaceCoercionX )

import {-# SOURCE #-} GHC.Core.DataCon
   ( dataConFullSig , dataConUserTyVarBinders
   , DataCon )

import GHC.Core.Type ( isLiftedTypeKind )

import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Tidy
import GHC.Core.TyCo.FVs
import GHC.Core.Class
import GHC.Types.Var

import GHC.Iface.Type

import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Utils.Outputable
import GHC.Types.Basic ( PprPrec(..), topPrec, sigPrec, opPrec
                       , funPrec, appPrec, maybeParen )

{-
%************************************************************************
%*                                                                      *
                   Pretty-printing types

       Defined very early because of debug printing in assertions
%*                                                                      *
%************************************************************************

@pprType@ is the standard @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

Note that any function which pretty-prints a @Type@ first converts the @Type@
to an @IfaceType@. See Note [IfaceType and pretty-printing] in GHC.Iface.Type.

See Note [Precedence in types] in GHC.Types.Basic.
-}

--------------------------------------------------------
-- When pretty-printing types, we convert to IfaceType,
--   and pretty-print that.
-- See Note [Pretty printing via Iface syntax] in GHC.Types.TyThing.Ppr
--------------------------------------------------------

pprType, pprParendType, pprTidiedType :: Type -> SDoc
pprType       = pprPrecType topPrec
pprParendType = pprPrecType appPrec

-- already pre-tidied
pprTidiedType = pprIfaceType . toIfaceTypeX emptyVarSet

pprPrecType :: PprPrec -> Type -> SDoc
pprPrecType = pprPrecTypeX emptyTidyEnv

pprPrecTypeX :: TidyEnv -> PprPrec -> Type -> SDoc
pprPrecTypeX env prec ty
  = getPprStyle $ \sty ->
    getPprDebug $ \debug ->
    if debug                    -- Use debugPprType when in
    then debug_ppr_ty prec ty   -- when in debug-style
    else pprPrecIfaceType prec (tidyToIfaceTypeStyX env ty sty)
    -- NB: debug-style is used for -dppr-debug
    --     dump-style  is used for -ddump-tc-trace etc

pprTyLit :: TyLit -> SDoc
pprTyLit = pprIfaceTyLit . toIfaceTyLit

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

tidyToIfaceTypeStyX :: TidyEnv -> Type -> PprStyle -> IfaceType
tidyToIfaceTypeStyX env ty sty
  | userStyle sty = tidyToIfaceTypeX env ty
  | otherwise     = toIfaceTypeX (tyCoVarsOfType ty) ty
     -- in latter case, don't tidy, as we'll be printing uniques.

tidyToIfaceType :: Type -> IfaceType
tidyToIfaceType = tidyToIfaceTypeX emptyTidyEnv

tidyToIfaceTypeX :: TidyEnv -> Type -> IfaceType
-- It's vital to tidy before converting to an IfaceType
-- or nested binders will become indistinguishable!
--
-- Also for the free type variables, tell toIfaceTypeX to
-- leave them as IfaceFreeTyVar.  This is super-important
-- for debug printing.
tidyToIfaceTypeX env ty = toIfaceTypeX (mkVarSet free_tcvs) (tidyType env' ty)
  where
    env'      = tidyFreeTyCoVars env free_tcvs
    free_tcvs = tyCoVarsOfTypeWellScoped ty

------------
pprCo, pprParendCo :: Coercion -> SDoc
pprCo       co = getPprStyle $ \ sty -> pprIfaceCoercion (tidyToIfaceCoSty co sty)
pprParendCo co = getPprStyle $ \ sty -> pprParendIfaceCoercion (tidyToIfaceCoSty co sty)

tidyToIfaceCoSty :: Coercion -> PprStyle -> IfaceCoercion
tidyToIfaceCoSty co sty
  | userStyle sty = tidyToIfaceCo co
  | otherwise     = toIfaceCoercionX (tyCoVarsOfCo co) co
     -- in latter case, don't tidy, as we'll be printing uniques.

tidyToIfaceCo :: Coercion -> IfaceCoercion
-- It's vital to tidy before converting to an IfaceType
-- or nested binders will become indistinguishable!
--
-- Also for the free type variables, tell toIfaceCoercionX to
-- leave them as IfaceFreeCoVar.  This is super-important
-- for debug printing.
tidyToIfaceCo co = toIfaceCoercionX (mkVarSet free_tcvs) (tidyCo env co)
  where
    env       = tidyFreeTyCoVars emptyTidyEnv free_tcvs
    free_tcvs = scopedSort $ tyCoVarsOfCoList co
------------
pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = pprTypeApp (classTyCon clas) tys

------------
pprTheta :: ThetaType -> SDoc
pprTheta = pprIfaceContext topPrec . map tidyToIfaceType

pprParendTheta :: ThetaType -> SDoc
pprParendTheta = pprIfaceContext appPrec . map tidyToIfaceType

pprThetaArrowTy :: ThetaType -> SDoc
pprThetaArrowTy = pprIfaceContextArr . map tidyToIfaceType

------------------
pprSigmaType :: Type -> SDoc
pprSigmaType = pprIfaceSigmaType ShowForAllWhen . tidyToIfaceType

pprForAll :: [TyCoVarBinder] -> SDoc
pprForAll tvs = pprIfaceForAll (map toIfaceForAllBndr tvs)

-- | Print a user-level forall; see @Note [When to print foralls]@ in
-- "GHC.Iface.Type".
pprUserForAll :: [TyCoVarBinder] -> SDoc
pprUserForAll = pprUserIfaceForAll . map toIfaceForAllBndr

pprTCvBndrs :: [TyCoVarBinder] -> SDoc
pprTCvBndrs tvs = sep (map pprTCvBndr tvs)

pprTCvBndr :: TyCoVarBinder -> SDoc
pprTCvBndr = pprTyVar . binderVar

pprTyVars :: [TyVar] -> SDoc
pprTyVars tvs = sep (map pprTyVar tvs)

pprTyVar :: TyVar -> SDoc
-- Print a type variable binder with its kind (but not if *)
-- Here we do not go via IfaceType, because the duplication with
-- pprIfaceTvBndr is minimal, and the loss of uniques etc in
-- debug printing is disastrous
pprTyVar tv
  | isLiftedTypeKind kind = ppr tv
  | otherwise             = parens (ppr tv <+> dcolon <+> ppr kind)
  where
    kind = tyVarKind tv

-----------------
debugPprType :: Type -> SDoc
-- ^ debugPprType is a simple pretty printer that prints a type
-- without going through IfaceType.  It does not format as prettily
-- as the normal route, but it's much more direct, and that can
-- be useful for debugging.  E.g. with -dppr-debug it prints the
-- kind on type-variable /occurrences/ which the normal route
-- fundamentally cannot do.
debugPprType ty = debug_ppr_ty topPrec ty

debug_ppr_ty :: PprPrec -> Type -> SDoc
debug_ppr_ty _ (LitTy l)
  = ppr l

debug_ppr_ty _ (TyVarTy tv)
  = ppr tv  -- With -dppr-debug we get (tv :: kind)

debug_ppr_ty prec (FunTy { ft_af = af, ft_arg = arg, ft_res = res })
  = maybeParen prec funPrec $
    sep [debug_ppr_ty funPrec arg, arrow <+> debug_ppr_ty prec res]
  where
    arrow = case af of
              VisArg   -> text "->"
              InvisArg -> text "=>"

debug_ppr_ty prec (TyConApp tc tys)
  | null tys  = ppr tc
  | otherwise = maybeParen prec appPrec $
                hang (ppr tc) 2 (sep (map (debug_ppr_ty appPrec) tys))

debug_ppr_ty _ (AppTy t1 t2)
  = hang (debug_ppr_ty appPrec t1)  -- Print parens so we see ((a b) c)
       2 (debug_ppr_ty appPrec t2)  -- so that we can distinguish
                                    -- TyConApp from AppTy

debug_ppr_ty prec (CastTy ty co)
  = maybeParen prec topPrec $
    hang (debug_ppr_ty topPrec ty)
       2 (text "|>" <+> ppr co)

debug_ppr_ty _ (CoercionTy co)
  = parens (text "CO" <+> ppr co)

debug_ppr_ty prec ty@(ForAllTy {})
  | (tvs, body) <- split ty
  = maybeParen prec funPrec $
    hang (text "forall" <+> fsep (map ppr tvs) <> dot)
         -- The (map ppr tvs) will print kind-annotated
         -- tvs, because we are (usually) in debug-style
       2 (ppr body)
  where
    split ty | ForAllTy tv ty' <- ty
             , (tvs, body) <- split ty'
             = (tv:tvs, body)
             | otherwise
             = ([], ty)

{-
Note [Infix type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
With TypeOperators you can say

   f :: (a ~> b) -> b

and the (~>) is considered a type variable.  However, the type
pretty-printer in this module will just see (a ~> b) as

   App (App (TyVarTy "~>") (TyVarTy "a")) (TyVarTy "b")

So it'll print the type in prefix form.  To avoid confusion we must
remember to parenthesise the operator, thus

   (~>) a b -> b

See #2766.
-}

pprDataCons :: TyCon -> SDoc
pprDataCons = sepWithVBars . fmap pprDataConWithArgs . tyConDataCons
  where
    sepWithVBars [] = mempty
    sepWithVBars docs = sep (punctuate (space <> vbar) docs)

pprDataConWithArgs :: DataCon -> SDoc
pprDataConWithArgs dc = sep [forAllDoc, thetaDoc, ppr dc <+> argsDoc]
  where
    (_univ_tvs, _ex_tvs, _eq_spec, theta, arg_tys, _res_ty) = dataConFullSig dc
    user_bndrs = tyVarSpecToBinders $ dataConUserTyVarBinders dc
    forAllDoc  = pprUserForAll user_bndrs
    thetaDoc   = pprThetaArrowTy theta
    argsDoc    = hsep (fmap pprParendType arg_tys)


pprTypeApp :: TyCon -> [Type] -> SDoc
pprTypeApp tc tys
  = pprIfaceTypeApp topPrec (toIfaceTyCon tc)
                            (toIfaceTcArgs tc tys)
    -- TODO: toIfaceTcArgs seems rather wasteful here

------------------
-- | Display all kind information (with @-fprint-explicit-kinds@) when the
-- provided 'Bool' argument is 'True'.
-- See @Note [Kind arguments in error messages]@ in "GHC.Tc.Errors".
pprWithExplicitKindsWhen :: Bool -> SDoc -> SDoc
pprWithExplicitKindsWhen b
  = updSDocContext $ \ctx ->
      if b then ctx { sdocPrintExplicitKinds = True }
           else ctx

-- | This variant preserves any use of TYPE in a type, effectively
-- locally setting -fprint-explicit-runtime-reps.
pprWithTYPE :: Type -> SDoc
pprWithTYPE ty = updSDocContext (\ctx -> ctx { sdocPrintExplicitRuntimeReps = True }) $
                 ppr ty

-- | Pretty prints a 'TyCon', using the family instance in case of a
-- representation tycon.  For example:
--
-- > data T [a] = ...
--
-- In that case we want to print @T [a]@, where @T@ is the family 'TyCon'
pprSourceTyCon :: TyCon -> SDoc
pprSourceTyCon tycon
  | Just (fam_tc, tys) <- tyConFamInst_maybe tycon
  = ppr $ fam_tc `TyConApp` tys        -- can't be FunTyCon
  | otherwise
  = ppr tycon
