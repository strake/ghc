{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}

module BuildTyCl (
        buildSynonymTyCon,
        buildFamilyTyCon,
        buildAlgTyCon,
        buildDataCon,
        buildPatSyn,
        TcMethInfo, buildClass,
        distinctAbstractTyConRhs, totallyAbstractTyConRhs,
        mkNewTyConRhs, mkDataTyConRhs,
        newImplicitBinder, newTyConRepName
    ) where

#include "HsVersions.h"

import IfaceEnv
import FamInstEnv( FamInstEnvs )
import TysWiredIn( isCTupleTyConName )
import PrelNames( tyConRepModOcc )
import DataCon
import PatSyn
import Var
import VarSet
import BasicTypes
import Name
import MkId
import Class
import TyCon
import Type
import Id
import Coercion
import TcType

import SrcLoc( noSrcSpan )
import DynFlags
import TcRnMonad
import UniqSupply
import Util
import Outputable

------------------------------------------------------
buildSynonymTyCon :: Name -> [TyVar] -> [Role]
                  -> Type
                  -> Kind                   -- ^ Kind of the RHS
                  -> TyCon
buildSynonymTyCon tc_name tvs roles rhs rhs_kind
  = mkSynonymTyCon tc_name kind tvs roles rhs
  where
    kind = mkPiKinds tvs rhs_kind


buildFamilyTyCon :: Name         -- ^ Type family name
                 -> [TyVar]      -- ^ Type variables
                 -> Maybe Name   -- ^ Result variable name
                 -> FamTyConFlav -- ^ Open, closed or in a boot file?
                 -> Kind         -- ^ Kind of the RHS
                 -> Maybe Class  -- ^ Parent, if exists
                 -> Injectivity  -- ^ Injectivity annotation
                                 -- See [Injectivity annotation] in HsDecls
                 -> TyCon
buildFamilyTyCon tc_name tvs res_tv rhs rhs_kind parent injectivity
  = mkFamilyTyCon tc_name kind tvs res_tv rhs parent injectivity
  where kind = mkPiKinds tvs rhs_kind


------------------------------------------------------
distinctAbstractTyConRhs, totallyAbstractTyConRhs :: AlgTyConRhs
distinctAbstractTyConRhs = AbstractTyCon True
totallyAbstractTyConRhs  = AbstractTyCon False

mkDataTyConRhs :: [DataCon] -> AlgTyConRhs
mkDataTyConRhs cons
  = DataTyCon {
        data_cons = cons,
        is_enum = not (null cons) && all is_enum_con cons
                  -- See Note [Enumeration types] in TyCon
    }
  where
    is_enum_con con
       | (_tvs, theta, arg_tys, _res) <- dataConSig con
       = null theta && null arg_tys


mkNewTyConRhs :: Name -> TyCon -> DataCon -> TcRnIf m n AlgTyConRhs
-- ^ Monadic because it makes a Name for the coercion TyCon
--   We pass the Name of the parent TyCon, as well as the TyCon itself,
--   because the latter is part of a knot, whereas the former is not.
mkNewTyConRhs tycon_name tycon con
  = do  { co_tycon_name <- newImplicitBinder tycon_name mkNewTyCoOcc
        ; let co_tycon = mkNewTypeCo co_tycon_name tycon etad_tvs etad_roles etad_rhs
        ; traceIf (text "mkNewTyConRhs" <+> ppr co_tycon)
        ; return (NewTyCon { data_con    = con,
                             nt_rhs      = rhs_ty,
                             nt_etad_rhs = (etad_tvs, etad_rhs),
                             nt_co       = co_tycon } ) }
                             -- Coreview looks through newtypes with a Nothing
                             -- for nt_co, or uses explicit coercions otherwise
  where
    tvs    = tyConTyVars tycon
    roles  = tyConRoles tycon
    inst_con_ty = applyTys (dataConUserType con) (mkTyVarTys tvs)
    rhs_ty = ASSERT( isFunTy inst_con_ty ) funArgTy inst_con_ty
        -- Instantiate the data con with the
        -- type variables from the tycon
        -- NB: a newtype DataCon has a type that must look like
        --        forall tvs.  <arg-ty> -> T tvs
        -- Note that we *can't* use dataConInstOrigArgTys here because
        -- the newtype arising from   class Foo a => Bar a where {}
        -- has a single argument (Foo a) that is a *type class*, so
        -- dataConInstOrigArgTys returns [].

    etad_tvs   :: [TyVar]  -- Matched lazily, so that mkNewTypeCo can
    etad_roles :: [Role]   -- return a TyCon without pulling on rhs_ty
    etad_rhs   :: Type     -- See Note [Tricky iface loop] in LoadIface
    (etad_tvs, etad_roles, etad_rhs) = eta_reduce (reverse tvs) (reverse roles) rhs_ty

    eta_reduce :: [TyVar]       -- Reversed
               -> [Role]        -- also reversed
               -> Type          -- Rhs type
               -> ([TyVar], [Role], Type)  -- Eta-reduced version
                                           -- (tyvars in normal order)
    eta_reduce (a:as) (_:rs) ty | Just (fun, arg) <- splitAppTy_maybe ty,
                                  Just tv <- getTyVar_maybe arg,
                                  tv == a,
                                  not (a `elemVarSet` tyVarsOfType fun)
                                = eta_reduce as rs fun
    eta_reduce tvs rs ty = (reverse tvs, reverse rs, ty)


------------------------------------------------------
buildDataCon :: FamInstEnvs
            -> Name
            -> Bool                     -- Declared infix
            -> Promoted TyConRepName    -- Promotable
            -> [HsSrcBang]
            -> Maybe [HsImplBang]
                -- See Note [Bangs on imported data constructors] in MkId
           -> [FieldLabel]             -- Field labels
           -> [TyVar] -> [TyVar]       -- Univ and ext
           -> [(TyVar,Type)]           -- Equality spec
           -> ThetaType                -- Does not include the "stupid theta"
                                       -- or the GADT equalities
           -> [Type] -> Type           -- Argument and result types
           -> TyCon                    -- Rep tycon
           -> TcRnIf m n DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--      allocating its unique (hence monadic)
buildDataCon fam_envs src_name declared_infix prom_info src_bangs impl_bangs field_lbls
             univ_tvs ex_tvs eq_spec ctxt arg_tys res_ty rep_tycon
  = do  { wrap_name <- newImplicitBinder src_name mkDataConWrapperOcc
        ; work_name <- newImplicitBinder src_name mkDataConWorkerOcc
        -- This last one takes the name of the data constructor in the source
        -- code, which (for Haskell source anyway) will be in the DataName name
        -- space, and puts it into the VarName name space

        ; traceIf (text "buildDataCon 1" <+> ppr src_name)
        ; us <- newUniqueSupply
        ; dflags <- getDynFlags
        ; let
                stupid_ctxt = mkDataConStupidTheta rep_tycon arg_tys univ_tvs
                data_con = mkDataCon src_name declared_infix prom_info
                                     src_bangs field_lbls
                                     univ_tvs ex_tvs eq_spec ctxt
                                     arg_tys res_ty rep_tycon
                                     stupid_ctxt dc_wrk dc_rep
                dc_wrk = mkDataConWorkId work_name data_con
                dc_rep = initUs_ us (mkDataConRep dflags fam_envs wrap_name
                                                  impl_bangs data_con)

        ; traceIf (text "buildDataCon 2" <+> ppr src_name)
        ; return data_con }


-- The stupid context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
-- ToDo: Or functionally dependent on?
--       This whole stupid theta thing is, well, stupid.
mkDataConStupidTheta :: TyCon -> [Type] -> [TyVar] -> [PredType]
mkDataConStupidTheta tycon arg_tys univ_tvs
  | null stupid_theta = []      -- The common case
  | otherwise         = filter in_arg_tys stupid_theta
  where
    tc_subst     = zipTopTvSubst (tyConTyVars tycon) (mkTyVarTys univ_tvs)
    stupid_theta = substTheta tc_subst (tyConStupidTheta tycon)
        -- Start by instantiating the master copy of the
        -- stupid theta, taken from the TyCon

    arg_tyvars      = tyVarsOfTypes arg_tys
    in_arg_tys pred = not $ isEmptyVarSet $
                      tyVarsOfType pred `intersectVarSet` arg_tyvars


------------------------------------------------------
buildPatSyn :: Name -> Bool
            -> (Id,Bool) -> Maybe (Id, Bool)
            -> ([TyVar], ThetaType) -- ^ Univ and req
            -> ([TyVar], ThetaType) -- ^ Ex and prov
            -> [Type]               -- ^ Argument types
            -> Type                 -- ^ Result type
            -> [FieldLabel]         -- ^ Field labels for
                                    --   a record pattern synonym
            -> PatSyn
buildPatSyn src_name declared_infix matcher@(matcher_id,_) builder
            (univ_tvs, req_theta) (ex_tvs, prov_theta) arg_tys
            pat_ty field_labels
  = ASSERT((and [ univ_tvs == univ_tvs'
                , ex_tvs == ex_tvs'
                , pat_ty `eqType` pat_ty'
                , prov_theta `eqTypes` prov_theta'
                , req_theta `eqTypes` req_theta'
                , arg_tys `eqTypes` arg_tys'
                ]))
    mkPatSyn src_name declared_infix
             (univ_tvs, req_theta) (ex_tvs, prov_theta)
             arg_tys pat_ty
             matcher builder field_labels
  where
    ((_:univ_tvs'), req_theta', tau) = tcSplitSigmaTy $ idType matcher_id
    ([pat_ty', cont_sigma, _], _) = tcSplitFunTys tau
    (ex_tvs', prov_theta', cont_tau) = tcSplitSigmaTy cont_sigma
    (arg_tys', _) = tcSplitFunTys cont_tau

-- ------------------------------------------------------

type TcMethInfo = (Name, DefMethSpec, Type)
        -- A temporary intermediate, to communicate between
        -- tcClassSigs and buildClass.

buildClass :: Name     -- Name of the class/tycon (they have the same Name)
           -> [TyVar] -> [Role] -> ThetaType
           -> [FunDep TyVar]               -- Functional dependencies
           -> [ClassATItem]                -- Associated types
           -> [TcMethInfo]                 -- Method info
           -> ClassMinimalDef              -- Minimal complete definition
           -> RecFlag                      -- Info for type constructor
           -> TcRnIf m n Class

buildClass tycon_name tvs roles sc_theta fds at_items sig_stuff mindef tc_isrec
  = fixM  $ \ rec_clas ->       -- Only name generation inside loop
    do  { traceIf (text "buildClass")

        ; datacon_name <- newImplicitBinder tycon_name mkClassDataConOcc
        ; tc_rep_name  <- newTyConRepName tycon_name

        ; op_items <- mapM (mk_op_item rec_clas) sig_stuff
                        -- Build the selector id and default method id

              -- Make selectors for the superclasses
        ; sc_sel_names <- mapM  (newImplicitBinder tycon_name . mkSuperDictSelOcc)
                                (takeList sc_theta [fIRST_TAG..])
        ; let sc_sel_ids = [ mkDictSelId sc_name rec_clas
                           | sc_name <- sc_sel_names]
              -- We number off the Dict superclass selectors, 1, 2, 3 etc so that we
              -- can construct names for the selectors. Thus
              --      class (C a, C b) => D a b where ...
              -- gives superclass selectors
              --      D_sc1, D_sc2
              -- (We used to call them D_C, but now we can have two different
              --  superclasses both called C!)

        ; let use_newtype = isSingleton arg_tys
                -- Use a newtype if the data constructor
                --   (a) has exactly one value field
                --       i.e. exactly one operation or superclass taken together
                --   (b) that value is of lifted type (which they always are, because
                --       we box equality superclasses)
                -- See note [Class newtypes and equality predicates]

                -- We treat the dictionary superclasses as ordinary arguments.
                -- That means that in the case of
                --     class C a => D a
                -- we don't get a newtype with no arguments!
              args      = sc_sel_names ++ op_names
              op_tys    = [ty | (_,_,ty) <- sig_stuff]
              op_names  = [op | (op,_,_) <- sig_stuff]
              arg_tys   = sc_theta ++ op_tys
              rec_tycon = classTyCon rec_clas

        ; dict_con <- buildDataCon (panic "buildClass: FamInstEnvs")
                                   datacon_name
                                   False        -- Not declared infix
                                   NotPromoted  -- Class tycons are not promoted
                                   (map (const no_bang) args)
                                   (Just (map (const HsLazy) args))
                                   [{- No fields -}]
                                   tvs [{- no existentials -}]
                                   [{- No GADT equalities -}]
                                   [{- No theta -}]
                                   arg_tys
                                   (mkTyConApp rec_tycon (mkTyVarTys tvs))
                                   rec_tycon

        ; rhs <- if use_newtype
                 then mkNewTyConRhs tycon_name rec_tycon dict_con
                 else if isCTupleTyConName tycon_name
                 then return (TupleTyCon { data_con = dict_con
                                         , tup_sort = ConstraintTuple })
                 else return (mkDataTyConRhs [dict_con])

        ; let { clas_kind = mkPiKinds tvs constraintKind
              ; tycon     = mkClassTyCon tycon_name clas_kind tvs roles
                                         rhs rec_clas tc_isrec tc_rep_name
                -- A class can be recursive, and in the case of newtypes
                -- this matters.  For example
                --      class C a where { op :: C b => a -> b -> Int }
                -- Because C has only one operation, it is represented by
                -- a newtype, and it should be a *recursive* newtype.
                -- [If we don't make it a recursive newtype, we'll expand the
                -- newtype like a synonym, but that will lead to an infinite
                -- type]

              ; result = mkClass tvs fds
                                 sc_theta sc_sel_ids at_items
                                 op_items mindef tycon
              }
        ; traceIf (text "buildClass" <+> ppr tycon)
        ; return result }
  where
    no_bang = HsSrcBang Nothing NoSrcUnpack NoSrcStrict

    mk_op_item :: Class -> TcMethInfo -> TcRnIf n m ClassOpItem
    mk_op_item rec_clas (op_name, dm_spec, _)
      = do { dm_info <- case dm_spec of
                          NoDM      -> return NoDefMeth
                          GenericDM -> do { dm_name <- newImplicitBinder op_name mkGenDefMethodOcc
                                          ; return (GenDefMeth dm_name) }
                          VanillaDM -> do { dm_name <- newImplicitBinder op_name mkDefaultMethodOcc
                                          ; return (DefMeth dm_name) }
           ; return (mkDictSelId op_name rec_clas, dm_info) }

{-
Note [Class newtypes and equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        class (a ~ F b) => C a b where
          op :: a -> b

We cannot represent this by a newtype, even though it's not
existential, because there are two value fields (the equality
predicate and op. See Trac #2238

Moreover,
          class (a ~ F b) => C a b where {}
Here we can't use a newtype either, even though there is only
one field, because equality predicates are unboxed, and classes
are boxed.
-}

newImplicitBinder :: Name                       -- Base name
                  -> (OccName -> OccName)       -- Occurrence name modifier
                  -> TcRnIf m n Name            -- Implicit name
-- Called in BuildTyCl to allocate the implicit binders of type/class decls
-- For source type/class decls, this is the first occurrence
-- For iface ones, the LoadIface has alrady allocated a suitable name in the cache
newImplicitBinder base_name mk_sys_occ
  | Just mod <- nameModule_maybe base_name
  = newGlobalBinder mod occ loc
  | otherwise           -- When typechecking a [d| decl bracket |],
                        -- TH generates types, classes etc with Internal names,
                        -- so we follow suit for the implicit binders
  = do  { uniq <- newUnique
        ; return (mkInternalName uniq occ loc) }
  where
    occ = mk_sys_occ (nameOccName base_name)
    loc = nameSrcSpan base_name

-- | Make the 'TyConRepName' for this 'TyCon'
newTyConRepName :: Name -> TcRnIf gbl lcl TyConRepName
newTyConRepName tc_name
  | Just mod <- nameModule_maybe tc_name
  , (mod, occ) <- tyConRepModOcc mod (nameOccName tc_name)
  = newGlobalBinder mod occ noSrcSpan
  | otherwise
  = newImplicitBinder tc_name mkTyConRepUserOcc
