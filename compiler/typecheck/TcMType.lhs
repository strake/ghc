%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Monadic type operations

This module contains monadic operations over types that contain
mutable type variables

\begin{code}
{-# LANGUAGE CPP #-}

module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newFlexiTyVarTy,              -- Kind -> TcM TcType
  newFlexiTyVarTys,             -- Int -> Kind -> TcM [TcType]
  newPolyFlexiTyVarTy,
  newMetaKindVar, newMetaKindVars,
  mkTcTyVarName, cloneMetaTyVar,

  newMetaTyVar, readMetaTyVar, writeMetaTyVar, writeMetaTyVarRef,
  newMetaDetails, isFilledMetaTyVar, isFlexiMetaTyVar,

  --------------------------------
  -- Creating new evidence variables
  newEvVar, newEvVars, newEq, newDict,
  newWantedEvVar, newWantedEvVars,
  newTcEvBinds, addTcEvBind,
  newFlatWanted, newFlatWanteds,

  --------------------------------
  -- Instantiation
  tcInstTyVars, newSigTyVar,
  tcInstType,
  tcInstSkolTyVars, tcInstSuperSkolTyVarsX,
  tcInstSigTyVarsLoc, tcInstSigTyVars,
  tcInstSkolType,
  tcSkolDFunType, tcSuperSkolTyVars,

  instSkolTyVars, freshenTyVarBndrs,

  --------------------------------
  -- Zonking
  zonkTcPredType,
  skolemiseUnboundMetaTyVar,
  zonkTcTyVar, zonkTcTyVars, zonkTyVarsAndFV, zonkTcTypeAndFV,
  zonkQuantifiedTyVar, quantifyTyVars,
  zonkTcTyVarBndr, zonkTcType, zonkTcTypes, zonkTcThetaType,

  zonkTcKind, defaultKindVarToStar,
  zonkEvVar, zonkWC, zonkFlats, zonkId, zonkCt, zonkSkolemInfo,

  tcGetGlobalTyVars,
  ) where

#include "HsVersions.h"

-- friends:
import TypeRep
import TcType
import Type
import Class
import Var

-- others:
import TcRnMonad        -- TcType, amongst others
import Id
import Name
import VarSet
import PrelNames
import DynFlags
import Util
import Outputable
import FastString
import SrcLoc
import Bag

import Control.Monad
import Data.List        ( partition, mapAccumL )
\end{code}


%************************************************************************
%*                                                                      *
        Kind variables
%*                                                                      *
%************************************************************************

\begin{code}
mkKindName :: Unique -> Name
mkKindName unique = mkSystemName unique kind_var_occ

kind_var_occ :: OccName -- Just one for all MetaKindVars
                        -- They may be jiggled by tidying
kind_var_occ = mkOccName tvName "k"

newMetaKindVar :: TcM TcKind
newMetaKindVar = do { uniq <- newUnique
                    ; details <- newMetaDetails TauTv
                    ; let kv = mkTcTyVar (mkKindName uniq) superKind details
                    ; return (mkTyVarTy kv) }

newMetaKindVars :: Int -> TcM [TcKind]
newMetaKindVars n = mapM (\ _ -> newMetaKindVar) (nOfThem n ())
\end{code}


%************************************************************************
%*                                                                      *
     Evidence variables; range over constraints we can abstract over
%*                                                                      *
%************************************************************************

\begin{code}
newEvVars :: TcThetaType -> TcM [EvVar]
newEvVars theta = mapM newEvVar theta

newWantedEvVar :: TcPredType -> TcM EvVar
newWantedEvVar = newEvVar

newWantedEvVars :: TcThetaType -> TcM [EvVar]
newWantedEvVars theta = mapM newWantedEvVar theta

--------------

newEvVar :: TcPredType -> TcM EvVar
-- Creates new *rigid* variables for predicates
newEvVar ty = do { name <- newSysName (predTypeOccName ty)
                 ; return (mkLocalId name ty) }

newEq :: TcType -> TcType -> TcM EvVar
newEq ty1 ty2
  = do { name <- newSysName (mkVarOccFS (fsLit "cobox"))
       ; return (mkLocalId name (mkTcEqPred ty1 ty2)) }

newDict :: Class -> [TcType] -> TcM DictId
newDict cls tys
  = do { name <- newSysName (mkDictOcc (getOccName cls))
       ; return (mkLocalId name (mkClassPred cls tys)) }

predTypeOccName :: PredType -> OccName
predTypeOccName ty = case classifyPredType ty of
    ClassPred cls _ -> mkDictOcc (getOccName cls)
    EqPred _ _      -> mkVarOccFS (fsLit "cobox")
    TuplePred _     -> mkVarOccFS (fsLit "tup")
    IrredPred _     -> mkVarOccFS (fsLit "irred")
\end{code}

*********************************************************************************
*                                                                               *
*                   Wanted constraints
*                                                                               *
*********************************************************************************

\begin{code}
newFlatWanted :: CtOrigin -> PredType -> TcM Ct
newFlatWanted orig pty
  = do loc <- getCtLoc orig
       v <- newWantedEvVar pty
       return $ mkNonCanonical $
            CtWanted { ctev_evar = v
                     , ctev_pred = pty
                     , ctev_loc = loc }

newFlatWanteds :: CtOrigin -> ThetaType -> TcM [Ct]
newFlatWanteds orig = mapM (newFlatWanted orig)
\end{code}

%************************************************************************
%*                                                                      *
        SkolemTvs (immutable)
%*                                                                      *
%************************************************************************

\begin{code}
tcInstType :: ([TyVar] -> TcM (TvSubst, [TcTyVar]))     -- How to instantiate the type variables
           -> TcType                                    -- Type to instantiate
           -> TcM ([TcTyVar], TcThetaType, TcType)      -- Result
                -- (type vars (excl coercion vars), preds (incl equalities), rho)
tcInstType inst_tyvars ty
  = case tcSplitForAllTys ty of
        ([],     rho) -> let    -- There may be overloading despite no type variables;
                                --      (?x :: Int) => Int -> Int
                           (theta, tau) = tcSplitPhiTy rho
                         in
                         return ([], theta, tau)

        (tyvars, rho) -> do { (subst, tyvars') <- inst_tyvars tyvars
                            ; let (theta, tau) = tcSplitPhiTy (substTy subst rho)
                            ; return (tyvars', theta, tau) }

tcSkolDFunType :: Type -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type signature with skolem constants.
-- We could give them fresh names, but no need to do so
tcSkolDFunType ty = tcInstType tcInstSuperSkolTyVars ty

tcSuperSkolTyVars :: [TyVar] -> (TvSubst, [TcTyVar])
-- Make skolem constants, but do *not* give them new names, as above
-- Moreover, make them "super skolems"; see comments with superSkolemTv
-- see Note [Kind substitution when instantiating]
-- Precondition: tyvars should be ordered (kind vars first)
tcSuperSkolTyVars = mapAccumL tcSuperSkolTyVar (mkTopTvSubst [])

tcSuperSkolTyVar :: TvSubst -> TyVar -> (TvSubst, TcTyVar)
tcSuperSkolTyVar subst tv
  = (extendTvSubst subst tv (mkTyVarTy new_tv), new_tv)
  where
    kind   = substTy subst (tyVarKind tv)
    new_tv = mkTcTyVar (tyVarName tv) kind superSkolemTv

tcInstSkolTyVars :: [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSkolTyVars = tcInstSkolTyVars' False emptyTvSubst

tcInstSuperSkolTyVars :: [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSuperSkolTyVars = tcInstSuperSkolTyVarsX emptyTvSubst

tcInstSuperSkolTyVarsX :: TvSubst -> [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSuperSkolTyVarsX subst = tcInstSkolTyVars' True subst

tcInstSkolTyVars' :: Bool -> TvSubst -> [TyVar] -> TcM (TvSubst, [TcTyVar])
-- Precondition: tyvars should be ordered (kind vars first)
-- see Note [Kind substitution when instantiating]
-- Get the location from the monad; this is a complete freshening operation
tcInstSkolTyVars' overlappable subst tvs
  = do { loc <- getSrcSpanM
       ; instSkolTyVarsX (mkTcSkolTyVar loc overlappable) subst tvs }

mkTcSkolTyVar :: SrcSpan -> Bool -> Unique -> Name -> Kind -> TcTyVar
mkTcSkolTyVar loc overlappable uniq old_name kind
  = mkTcTyVar (mkInternalName uniq (getOccName old_name) loc)
              kind
              (SkolemTv overlappable)

tcInstSigTyVarsLoc :: SrcSpan -> [TyVar] -> TcRnIf gbl lcl (TvSubst, [TcTyVar])
-- We specify the location
tcInstSigTyVarsLoc loc = instSkolTyVars (mkTcSkolTyVar loc False)

tcInstSigTyVars :: [TyVar] -> TcRnIf gbl lcl (TvSubst, [TcTyVar])
-- Get the location from the TyVar itself, not the monad
tcInstSigTyVars
  = instSkolTyVars mk_tv
  where
    mk_tv uniq old_name kind
       = mkTcTyVar (setNameUnique old_name uniq) kind (SkolemTv False)

tcInstSkolType :: TcType -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type with fresh skolem constants
-- Binding location comes from the monad
tcInstSkolType ty = tcInstType tcInstSkolTyVars ty

------------------
freshenTyVarBndrs :: [TyVar] -> TcRnIf gbl lcl (TvSubst, [TyVar])
-- ^ Give fresh uniques to a bunch of TyVars, but they stay
--   as TyVars, rather than becoming TcTyVars
-- Used in FamInst.newFamInst, and Inst.newClsInst
freshenTyVarBndrs = instSkolTyVars mk_tv
  where
    mk_tv uniq old_name kind = mkTyVar (setNameUnique old_name uniq) kind

------------------
instSkolTyVars :: (Unique -> Name -> Kind -> TyVar)
                -> [TyVar] -> TcRnIf gbl lcl (TvSubst, [TyVar])
instSkolTyVars mk_tv = instSkolTyVarsX mk_tv emptyTvSubst

instSkolTyVarsX :: (Unique -> Name -> Kind -> TyVar)
                -> TvSubst -> [TyVar] -> TcRnIf gbl lcl (TvSubst, [TyVar])
instSkolTyVarsX mk_tv = mapAccumLM (instSkolTyVarX mk_tv)

instSkolTyVarX :: (Unique -> Name -> Kind -> TyVar)
               -> TvSubst -> TyVar -> TcRnIf gbl lcl (TvSubst, TyVar)
instSkolTyVarX mk_tv subst tyvar
  = do  { uniq <- newUnique
        ; let new_tv = mk_tv uniq old_name kind
        ; return (extendTvSubst subst tyvar (mkTyVarTy new_tv), new_tv) }
  where
    old_name = tyVarName tyvar
    kind     = substTy subst (tyVarKind tyvar)
\end{code}

Note [Kind substitution when instantiating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we instantiate a bunch of kind and type variables, first we
expect them to be sorted (kind variables first, then type variables).
Then we have to instantiate the kind variables, build a substitution
from old variables to the new variables, then instantiate the type
variables substituting the original kind.

Exemple: If we want to instantiate
  [(k1 :: BOX), (k2 :: BOX), (a :: k1 -> k2), (b :: k1)]
we want
  [(?k1 :: BOX), (?k2 :: BOX), (?a :: ?k1 -> ?k2), (?b :: ?k1)]
instead of the buggous
  [(?k1 :: BOX), (?k2 :: BOX), (?a :: k1 -> k2), (?b :: k1)]


%************************************************************************
%*                                                                      *
        MetaTvs (meta type variables; mutable)
%*                                                                      *
%************************************************************************

\begin{code}
newMetaTyVar :: MetaInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newMetaTyVar meta_info kind
  = do  { uniq <- newUnique
        ; let name = mkTcTyVarName uniq s
              s = case meta_info of
                        PolyTv     -> fsLit "s"
                        TauTv      -> fsLit "t"
                        FlatMetaTv -> fsLit "fmv"
                        SigTv      -> fsLit "a"
        ; details <- newMetaDetails meta_info
        ; return (mkTcTyVar name kind details) }

newSigTyVar :: Name -> Kind -> TcM TcTyVar
newSigTyVar name kind
  = do { uniq <- newUnique
       ; let name' = setNameUnique name uniq
                      -- Use the same OccName so that the tidy-er
                      -- doesn't gratuitously rename 'a' to 'a0' etc
       ; details <- newMetaDetails SigTv
       ; return (mkTcTyVar name' kind details) }

newMetaDetails :: MetaInfo -> TcM TcTyVarDetails
newMetaDetails info
  = do { ref <- newMutVar Flexi
       ; untch <- getUntouchables
       ; return (MetaTv { mtv_info = info, mtv_ref = ref, mtv_untch = untch }) }

cloneMetaTyVar :: TcTyVar -> TcM TcTyVar
cloneMetaTyVar tv
  = ASSERT( isTcTyVar tv )
    do  { uniq <- newUnique
        ; ref  <- newMutVar Flexi
        ; let name'    = setNameUnique (tyVarName tv) uniq
              details' = case tcTyVarDetails tv of
                           details@(MetaTv {}) -> details { mtv_ref = ref }
                           _ -> pprPanic "cloneMetaTyVar" (ppr tv)
        ; return (mkTcTyVar name' (tyVarKind tv) details') }

mkTcTyVarName :: Unique -> FastString -> Name
-- Make sure that fresh TcTyVar names finish with a digit
-- leaving the un-cluttered names free for user names
mkTcTyVarName uniq str = mkSysTvName uniq str

-- Works for both type and kind variables
readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
                      readMutVar (metaTvRef tyvar)

isFilledMetaTyVar :: TyVar -> TcM Bool
-- True of a filled-in (Indirect) meta type variable
isFilledMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do  { details <- readMutVar ref
        ; return (isIndirect details) }
  | otherwise = return False

isFlexiMetaTyVar :: TyVar -> TcM Bool
-- True of a un-filled-in (Flexi) meta type variable
isFlexiMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do  { details <- readMutVar ref
        ; return (isFlexi details) }
  | otherwise = return False

--------------------
-- Works with both type and kind variables
writeMetaTyVar :: TcTyVar -> TcType -> TcM ()
-- Write into a currently-empty MetaTyVar

writeMetaTyVar tyvar ty
  | not debugIsOn
  = writeMetaTyVarRef tyvar (metaTvRef tyvar) ty

-- Everything from here on only happens if DEBUG is on
  | not (isTcTyVar tyvar)
  = WARN( True, text "Writing to non-tc tyvar" <+> ppr tyvar )
    return ()

  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tyvar
  = writeMetaTyVarRef tyvar ref ty

  | otherwise
  = WARN( True, text "Writing to non-meta tyvar" <+> ppr tyvar )
    return ()

--------------------
writeMetaTyVarRef :: TcTyVar -> TcRef MetaDetails -> TcType -> TcM ()
-- Here the tyvar is for error checking only;
-- the ref cell must be for the same tyvar
writeMetaTyVarRef tyvar ref ty
  | not debugIsOn
  = do { traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)
       ; writeMutVar ref (Indirect ty) }

-- Everything from here on only happens if DEBUG is on
  | otherwise
  = do { meta_details <- readMutVar ref;
       -- Zonk kinds to allow the error check to work
       ; zonked_tv_kind <- zonkTcKind tv_kind
       ; zonked_ty_kind <- zonkTcKind ty_kind

       -- Check for double updates
       ; ASSERT2( isFlexi meta_details,
                  hang (text "Double update of meta tyvar")
                   2 (ppr tyvar $$ ppr meta_details) )

         traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)
       ; writeMutVar ref (Indirect ty)
       ; when (   not (isPredTy tv_kind)
                    -- Don't check kinds for updates to coercion variables
               && not (zonked_ty_kind `tcIsSubKind` zonked_tv_kind))
       $ WARN( True, hang (text "Ill-kinded update to meta tyvar")
                        2 (    ppr tyvar <+> text "::" <+> (ppr tv_kind $$ ppr zonked_tv_kind)
                           <+> text ":="
                           <+> ppr ty    <+> text "::" <+> (ppr ty_kind $$ ppr zonked_ty_kind) ) )
         (return ()) }
  where
    tv_kind = tyVarKind tyvar
    ty_kind = typeKind ty
\end{code}


%************************************************************************
%*                                                                      *
        MetaTvs: TauTvs
%*                                                                      *
%************************************************************************

\begin{code}
newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newMetaTyVar TauTv kind

newFlexiTyVarTy  :: Kind -> TcM TcType
newFlexiTyVarTy kind = do
    tc_tyvar <- newFlexiTyVar kind
    return (TyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mapM newFlexiTyVarTy (nOfThem n kind)

newPolyFlexiTyVarTy :: TcM TcType
newPolyFlexiTyVarTy = do { tv <- newMetaTyVar PolyTv liftedTypeKind
                         ; return (TyVarTy tv) }

tcInstTyVars :: [TKVar] -> TcM (TvSubst, [TcTyVar])
-- Instantiate with META type variables
-- Note that this works for a sequence of kind and type
-- variables.  Eg    [ (k:BOX), (a:k->k) ]
--             Gives [ (k7:BOX), (a8:k7->k7) ]
tcInstTyVars tyvars = mapAccumLM tcInstTyVarX emptyTvSubst tyvars
    -- emptyTvSubst has an empty in-scope set, but that's fine here
    -- Since the tyvars are freshly made, they cannot possibly be
    -- captured by any existing for-alls.

tcInstTyVarX :: TvSubst -> TKVar -> TcM (TvSubst, TcTyVar)
-- Make a new unification variable tyvar whose Name and Kind come from
-- an existing TyVar. We substitute kind variables in the kind.
tcInstTyVarX subst tyvar
  = do  { uniq <- newUnique
        ; details <- newMetaDetails TauTv
        ; let name   = mkSystemName uniq (getOccName tyvar)
              kind   = substTy subst (tyVarKind tyvar)
              new_tv = mkTcTyVar name kind details
        ; return (extendTvSubst subst tyvar (mkTyVarTy new_tv), new_tv) }
\end{code}


%************************************************************************
%*                                                                      *
             Quantification
%*                                                                      *
%************************************************************************

Note [quantifyTyVars]
~~~~~~~~~~~~~~~~~~~~~
quantifyTyVars is give the free vars of a type that we
are about to wrap in a forall.

It takes these free type/kind variables and
  1. Zonks them and remove globals
  2. Partitions into type and kind variables (kvs1, tvs)
  3. Extends kvs1 with free kind vars in the kinds of tvs (removing globals)
  4. Calls zonkQuantifiedTyVar on each

Step (3) is often unimportant, because the kind variable is often
also free in the type.  Eg
     Typeable k (a::k)
has free vars {k,a}.  But the type (see Trac #7916)
    (f::k->*) (a::k)
has free vars {f,a}, but we must add 'k' as well! Hence step (3).

\begin{code}
quantifyTyVars :: TcTyVarSet -> TcTyVarSet -> TcM [TcTyVar]
-- See Note [quantifyTyVars]
-- The input is a mixture of type and kind variables; a kind variable k
--   may occur *after* a tyvar mentioning k in its kind
-- Can be given a mixture of TcTyVars and TyVars, in the case of
--   associated type declarations

quantifyTyVars gbl_tvs tkvs
  = do { tkvs    <- zonkTyVarsAndFV tkvs
       ; gbl_tvs <- zonkTyVarsAndFV gbl_tvs
       ; let (kvs, tvs) = partitionVarSet isKindVar (closeOverKinds tkvs `minusVarSet` gbl_tvs)
                              -- NB kinds of tvs are zonked by zonkTyVarsAndFV
             kvs2 = varSetElems kvs
             qtvs = varSetElems tvs

             -- In the non-PolyKinds case, default the kind variables
             -- to *, and zonk the tyvars as usual.  Notice that this
             -- may make quantifyTyVars return a shorter list
             -- than it was passed, but that's ok
       ; poly_kinds <- xoptM Opt_PolyKinds
       ; qkvs <- if poly_kinds
                 then return kvs2
                 else do { let (meta_kvs, skolem_kvs) = partition is_meta kvs2
                               is_meta kv = isTcTyVar kv && isMetaTyVar kv
                         ; mapM_ defaultKindVarToStar meta_kvs
                         ; return skolem_kvs }  -- should be empty

       ; mapM zonk_quant (qkvs ++ qtvs) }
           -- Because of the order, any kind variables
           -- mentioned in the kinds of the type variables refer to
           -- the now-quantified versions
  where
    zonk_quant tkv
      | isTcTyVar tkv = zonkQuantifiedTyVar tkv
      | otherwise     = return tkv
      -- For associated types, we have the class variables
      -- in scope, and they are TyVars not TcTyVars

zonkQuantifiedTyVar :: TcTyVar -> TcM TcTyVar
-- The quantified type variables often include meta type variables
-- we want to freeze them into ordinary type variables, and
-- default their kind (e.g. from OpenTypeKind to TypeKind)
--                      -- see notes with Kind.defaultKind
-- The meta tyvar is updated to point to the new skolem TyVar.  Now any
-- bound occurrences of the original type variable will get zonked to
-- the immutable version.
--
-- We leave skolem TyVars alone; they are immutable.
--
-- This function is called on both kind and type variables,
-- but kind variables *only* if PolyKinds is on.
zonkQuantifiedTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      SkolemTv {} -> do { kind <- zonkTcKind (tyVarKind tv)
                        ; return $ setTyVarKind tv kind }
        -- It might be a skolem type variable,
        -- for example from a user type signature

      MetaTv { mtv_ref = ref } ->
          do when debugIsOn $ do
                 -- [Sept 04] Check for non-empty.
                 -- See note [Silly Type Synonym]
                 cts <- readMutVar ref
                 case cts of
                     Flexi -> return ()
                     Indirect ty -> WARN( True, ppr tv $$ ppr ty )
                                    return ()
             skolemiseUnboundMetaTyVar tv vanillaSkolemTv
      _other -> pprPanic "zonkQuantifiedTyVar" (ppr tv) -- FlatSkol, RuntimeUnk

defaultKindVarToStar :: TcTyVar -> TcM Kind
-- We have a meta-kind: unify it with '*'
defaultKindVarToStar kv
  = do { ASSERT( isKindVar kv && isMetaTyVar kv )
         writeMetaTyVar kv liftedTypeKind
       ; return liftedTypeKind }

skolemiseUnboundMetaTyVar :: TcTyVar -> TcTyVarDetails -> TcM TyVar
-- We have a Meta tyvar with a ref-cell inside it
-- Skolemise it, including giving it a new Name, so that
--   we are totally out of Meta-tyvar-land
-- We create a skolem TyVar, not a regular TyVar
--   See Note [Zonking to Skolem]
skolemiseUnboundMetaTyVar tv details
  = ASSERT2( isMetaTyVar tv, ppr tv )
    do  { span <- getSrcSpanM    -- Get the location from "here"
                                 -- ie where we are generalising
        ; uniq <- newUnique      -- Remove it from TcMetaTyVar unique land
        ; kind <- zonkTcKind (tyVarKind tv)
        ; let final_kind = defaultKind kind
              final_name = mkInternalName uniq (getOccName tv) span
              final_tv   = mkTcTyVar final_name final_kind details

        ; traceTc "Skolemising" (ppr tv <+> ptext (sLit ":=") <+> ppr final_tv)
        ; writeMetaTyVar tv (mkTyVarTy final_tv)
        ; return final_tv }
\end{code}

Note [Zonking to Skolem]
~~~~~~~~~~~~~~~~~~~~~~~~
We used to zonk quantified type variables to regular TyVars.  However, this
leads to problems.  Consider this program from the regression test suite:

  eval :: Int -> String -> String -> String
  eval 0 root actual = evalRHS 0 root actual

  evalRHS :: Int -> a
  evalRHS 0 root actual = eval 0 root actual

It leads to the deferral of an equality (wrapped in an implication constraint)

  forall a. () => ((String -> String -> String) ~ a)

which is propagated up to the toplevel (see TcSimplify.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by TcRnDriver.tcRnSrcDecls calling TcSimplify.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem.  The skolem is rigid
(which we require for a quantified variable), but is still a TcTyVar that the
simplifier knows how to deal with.

Note [Silly Type Synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
        type C u a = u  -- Note 'a' unused

        foo :: (forall a. C u a -> C u a) -> u
        foo x = ...

        bar :: Num u => u
        bar = foo (\t -> t + t)

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
        {Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
        \/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
        a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()

%************************************************************************
%*                                                                      *
              Zonking types
%*                                                                      *
%************************************************************************

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: TcM TcTyVarSet
tcGetGlobalTyVars
  = do { (TcLclEnv {tcl_tyvars = gtv_var}) <- getLclEnv
       ; gbl_tvs  <- readMutVar gtv_var
       ; gbl_tvs' <- zonkTyVarsAndFV gbl_tvs
       ; writeMutVar gtv_var gbl_tvs'
       ; return gbl_tvs' }
  where
\end{code}

\begin{code}
zonkTcTypeAndFV :: TcType -> TcM TyVarSet
-- Zonk a type and take its free variables
-- With kind polymorphism it can be essential to zonk *first*
-- so that we find the right set of free variables.  Eg
--    forall k1. forall (a:k2). a
-- where k2:=k1 is in the substitution.  We don't want
-- k2 to look free in this type!
zonkTcTypeAndFV ty = do { ty <- zonkTcType ty; return (tyVarsOfType ty) }

zonkTyVar :: TyVar -> TcM TcType
-- Works on TyVars and TcTyVars
zonkTyVar tv | isTcTyVar tv = zonkTcTyVar tv
             | otherwise    = return (mkTyVarTy tv)
   -- Hackily, when typechecking type and class decls
   -- we have TyVars in scopeadded (only) in
   -- TcHsType.tcTyClTyVars, but it seems
   -- painful to make them into TcTyVars there

zonkTyVarsAndFV :: TyVarSet -> TcM TyVarSet
zonkTyVarsAndFV tyvars = tyVarsOfTypes <$> mapM zonkTyVar (varSetElems tyvars)

zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

-----------------  Types
zonkTyVarKind :: TyVar -> TcM TyVar
zonkTyVarKind tv = do { kind' <- zonkTcKind (tyVarKind tv)
                      ; return (setTyVarKind tv kind') }

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mapM zonkTcType tys

zonkTcThetaType :: TcThetaType -> TcM TcThetaType
zonkTcThetaType theta = mapM zonkTcPredType theta

zonkTcPredType :: TcPredType -> TcM TcPredType
zonkTcPredType = zonkTcType
\end{code}

%************************************************************************
%*                                                                      *
              Zonking constraints
%*                                                                      *
%************************************************************************

\begin{code}
zonkImplication :: Implication -> TcM (Bag Implication)
zonkImplication implic@(Implic { ic_skols  = skols
                               , ic_given  = given
                               , ic_wanted = wanted
                               , ic_info   = info })
  = do { skols'  <- mapM zonkTcTyVarBndr skols  -- Need to zonk their kinds!
                                                -- as Trac #7230 showed
       ; given'  <- mapM zonkEvVar given
       ; info'   <- zonkSkolemInfo info
       ; wanted' <- zonkWCRec wanted
       ; if isEmptyWC wanted'
         then return emptyBag
         else return $ unitBag $
              implic { ic_skols  = skols'
                     , ic_given  = given'
                     , ic_wanted = wanted'
                     , ic_info   = info' } }

zonkEvVar :: EvVar -> TcM EvVar
zonkEvVar var = do { ty' <- zonkTcType (varType var)
                   ; return (setVarType var ty') }


zonkWC :: WantedConstraints -> TcM WantedConstraints
zonkWC wc = zonkWCRec wc

zonkWCRec :: WantedConstraints -> TcM WantedConstraints
zonkWCRec (WC { wc_flat = flat, wc_impl = implic, wc_insol = insol })
  = do { flat'   <- zonkFlats flat
       ; implic' <- flatMapBagM zonkImplication implic
       ; insol'  <- zonkFlats insol
       ; return (WC { wc_flat = flat', wc_impl = implic', wc_insol = insol' }) }
\end{code}

\begin{code}
zonkFlats :: Cts -> TcM Cts
zonkFlats cts = do { cts' <- mapBagM zonkCt' cts
                   ; traceTc "zonkFlats done:" (ppr cts')
                   ; return cts' }

zonkCt' :: Ct -> TcM Ct
zonkCt' ct = zonkCt ct

zonkCt :: Ct -> TcM Ct
zonkCt ct@(CHoleCan { cc_ev = ev })
  = do { ev' <- zonkCtEvidence ev
       ; return $ ct { cc_ev = ev' } }
zonkCt ct
  = do { fl' <- zonkCtEvidence (cc_ev ct)
       ; return (mkNonCanonical fl') }

zonkCtEvidence :: CtEvidence -> TcM CtEvidence
zonkCtEvidence ctev@(CtGiven { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred'}) }
zonkCtEvidence ctev@(CtWanted { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }
zonkCtEvidence ctev@(CtDerived { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }

zonkSkolemInfo :: SkolemInfo -> TcM SkolemInfo
zonkSkolemInfo (SigSkol cx ty)  = do { ty' <- zonkTcType ty
                                     ; return (SigSkol cx ty') }
zonkSkolemInfo (InferSkol ntys) = do { ntys' <- mapM do_one ntys
                                     ; return (InferSkol ntys') }
  where
    do_one (n, ty) = do { ty' <- zonkTcType ty; return (n, ty') }
zonkSkolemInfo skol_info = return skol_info
\end{code}



%************************************************************************
%*                                                                      *
\subsection{Zonking -- the main work-horses: zonkTcType, zonkTcTyVar}
%*                                                                      *
%*              For internal use only!                                  *
%*                                                                      *
%************************************************************************

\begin{code}
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = do { ty' <- zonkTcType (idType id)
       ; return (Id.setIdType id ty') }

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--      type variable and zonks the kind too

zonkTcType :: TcType -> TcM TcType
zonkTcType ty
  = go ty
  where
    go (TyConApp tc tys) = do tys' <- mapM go tys
                              return (TyConApp tc tys')
                -- Do NOT establish Type invariants, because
                -- doing so is strict in the TyCOn.
                -- See Note [Zonking inside the knot] in TcHsType

    go (LitTy n)         = return (LitTy n)

    go (FunTy arg res)   = do arg' <- go arg
                              res' <- go res
                              return (FunTy arg' res')

    go (AppTy fun arg)   = do fun' <- go fun
                              arg' <- go arg
                              return (mkAppTy fun' arg')
                -- NB the mkAppTy; we might have instantiated a
                -- type variable to a type constructor, so we need
                -- to pull the TyConApp to the top.
                -- OK to do this because only strict in the structure
                -- not in the TyCon.
                -- See Note [Zonking inside the knot] in TcHsType

        -- The two interesting cases!
    go (TyVarTy tyvar) | isTcTyVar tyvar = zonkTcTyVar tyvar
                       | otherwise       = TyVarTy <$> updateTyVarKindM go tyvar
                -- Ordinary (non Tc) tyvars occur inside quantified types

    go (ForAllTy tv ty) = do { tv' <- zonkTcTyVarBndr tv
                             ; ty' <- go ty
                             ; return (ForAllTy tv' ty') }

zonkTcTyVarBndr :: TcTyVar -> TcM TcTyVar
-- A tyvar binder is never a unification variable (MetaTv),
-- rather it is always a skolems.  BUT it may have a kind
-- that has not yet been zonked, and may include kind
-- unification variables.
zonkTcTyVarBndr tyvar
  = ASSERT2( isImmutableTyVar tyvar, ppr tyvar ) do
    updateTyVarKindM zonkTcType tyvar

zonkTcTyVar :: TcTyVar -> TcM TcType
-- Simply look through all Flexis
zonkTcTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv ) do
    case tcTyVarDetails tv of
      SkolemTv {}   -> zonk_kind_and_return
      RuntimeUnk {} -> zonk_kind_and_return
      FlatSkol ty   -> zonkTcType ty
      MetaTv { mtv_ref = ref }
         -> do { cts <- readMutVar ref
               ; case cts of
                    Flexi       -> zonk_kind_and_return
                    Indirect ty -> zonkTcType ty }
  where
    zonk_kind_and_return = do { z_tv <- zonkTyVarKind tv
                              ; return (TyVarTy z_tv) }
\end{code}



%************************************************************************
%*                                                                      *
                        Zonking kinds
%*                                                                      *
%************************************************************************

\begin{code}
zonkTcKind :: TcKind -> TcM TcKind
zonkTcKind k = zonkTcType k
\end{code}



