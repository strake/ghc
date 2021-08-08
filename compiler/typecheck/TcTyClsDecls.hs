{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcTyClsDecls: Typecheck type and class declarations
-}

{-# LANGUAGE CPP, TupleSections #-}

module TcTyClsDecls (
        tcTyAndClassDecls, tcAddImplicits,

        -- Functions used by TcInstDcls to check
        -- data/type family instance declarations
        kcDataDefn, tcConDecls, dataDeclChecks, checkValidTyCon,
        tcFamTyPats, tcTyFamInstEqn, famTyConShape,
        tcAddTyFamInstCtxt, tcMkDataFamInstCtxt, tcAddDataFamInstCtxt,
        wrongKindOfFamily, dataConCtxt, badDataConTyCon, mkOneRecordSelector
    ) where

#include "HsVersions.h"

import HsSyn
import HscTypes
import BuildTyCl
import TcRnMonad
import TcEnv
import TcValidity
import TcHsSyn
import TcTyDecls
import TcClassDcl
import TcHsType
import TcMType
import TcType
import FamInst
import FamInstEnv
import Coercion( ltRole )
import Type
import TypeRep   -- for checkValidRoles
import Kind
import Class
import CoAxiom
import TyCon
import DataCon
import ConLike
import Id
import IdInfo
import Var
import VarEnv
import VarSet
import Module
import Name
import NameSet
import NameEnv
import RdrName
import RnEnv
import Outputable
import Maybes
import Unify
import Util
import SrcLoc
import ListSetOps
import Digraph
import DynFlags
import FastString
import Unique           ( mkBuiltinUnique )
import BasicTypes

import Bag
import Control.Monad
import Data.List

{-
************************************************************************
*                                                                      *
\subsection{Type checking for type and class declarations}
*                                                                      *
************************************************************************

Note [Grouping of type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcTyAndClassDecls is called on a list of `TyClGroup`s. Each group is a strongly
connected component of mutually dependent types and classes. We kind check and
type check each group separately to enhance kind polymorphism. Take the
following example:

  type Id a = a
  data X = X (Id Int)

If we were to kind check the two declarations together, we would give Id the
kind * -> *, since we apply it to an Int in the definition of X. But we can do
better than that, since Id really is kind polymorphic, and should get kind
forall (k::BOX). k -> k. Since it does not depend on anything else, it can be
kind-checked by itself, hence getting the most general kind. We then kind check
X, which works fine because we then know the polymorphic kind of Id, and simply
instantiate k to *.

Note [Check role annotations in a second pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Role inference potentially depends on the types of all of the datacons declared
in a mutually recursive group. The validity of a role annotation, in turn,
depends on the result of role inference. Because the types of datacons might
be ill-formed (see #7175 and Note [Checking GADT return types]) we must check
*all* the tycons in a group for validity before checking *any* of the roles.
Thus, we take two passes over the resulting tycons, first checking for general
validity and then checking for valid role annotations.
-}

tcTyAndClassDecls :: [TyClGroup Name]   -- Mutually-recursive groups in dependency order
                  -> TcM TcGblEnv       -- Input env extended by types and classes
                                        -- and their implicit Ids,DataCons
-- Fails if there are any errors
tcTyAndClassDecls tyclds_s
  = checkNoErrs $       -- The code recovers internally, but if anything gave rise to
                        -- an error we'd better stop now, to avoid a cascade
    fold_env tyclds_s   -- Type check each group in dependency order folding the global env
  where
    fold_env :: [TyClGroup Name] -> TcM TcGblEnv
    fold_env [] = getGblEnv
    fold_env (tyclds:tyclds_s)
      = do { tcg_env <- tcTyClGroup tyclds
           ; setGblEnv tcg_env $ fold_env tyclds_s }
             -- remaining groups are typecheck in the extended global env

tcTyClGroup :: TyClGroup Name -> TcM TcGblEnv
-- Typecheck one strongly-connected component of type and class decls
tcTyClGroup tyclds
  = do {    -- Step 1: kind-check this group and returns the final
            -- (possibly-polymorphic) kind of each TyCon and Class
            -- See Note [Kind checking for type and class decls]
         names_w_poly_kinds <- kcTyClGroup tyclds
       ; traceTc "tcTyAndCl generalized kinds" (ppr names_w_poly_kinds)

            -- Step 2: type-check all groups together, returning
            -- the final TyCons and Classes
       ; let role_annots = extractRoleAnnots tyclds
             decls = group_tyclds tyclds
       ; tyclss <- fixM $ \ ~rec_tyclss -> do
           { is_boot   <- tcIsHsBootOrSig
           ; self_boot <- tcSelfBootInfo
           ; let rec_flags = calcRecFlags self_boot is_boot
                                          role_annots rec_tyclss

                 -- Populate environment with knot-tied ATyCon for TyCons
                 -- NB: if the decls mention any ill-staged data cons
                 -- (see Note [Recusion and promoting data constructors])
                 -- we will have failed already in kcTyClGroup, so no worries here
           ; tcExtendRecEnv (zipRecTyClss names_w_poly_kinds rec_tyclss) $

                 -- Also extend the local type envt with bindings giving
                 -- the (polymorphic) kind of each knot-tied TyCon or Class
                 -- See Note [Type checking recursive type and class declarations]
             tcExtendKindEnv names_w_poly_kinds              $

                 -- Kind and type check declarations for this group
             concatMapM (tcTyClDecl rec_flags) decls }

           -- Step 3: Perform the validity check
           -- We can do this now because we are done with the recursive knot
           -- Do it before Step 4 (adding implicit things) because the latter
           -- expects well-formed TyCons
       ; tcExtendGlobalEnv tyclss $ do
       { traceTc "Starting validity check" (ppr tyclss)
       ; mapM_ (recoverM (return ()) . checkValidTyCl) tyclss
           -- We recover, which allows us to report multiple validity errors
       ; mapM_ (recoverM (return ()) . checkValidRoleAnnots role_annots) tyclss
           -- See Note [Check role annotations in a second pass]

           -- Step 4: Add the implicit things;
           -- we want them in the environment because
           -- they may be mentioned in interface files
       ; tcAddImplicits tyclss } }

zipRecTyClss :: [(Name, Kind)]
             -> [TyThing]           -- Knot-tied
             -> [(Name,TyThing)]
-- Build a name-TyThing mapping for the things bound by decls
-- being careful not to look at the [TyThing]
-- The TyThings in the result list must have a visible ATyCon,
-- because typechecking types (in, say, tcTyClDecl) looks at this outer constructor
zipRecTyClss kind_pairs rec_things
  = [ (name, ATyCon (get name)) | (name, _kind) <- kind_pairs ]
  where
    rec_type_env :: TypeEnv
    rec_type_env = mkTypeEnv rec_things

    get name = case lookupTypeEnv rec_type_env name of
                 Just (ATyCon tc) -> tc
                 other            -> pprPanic "zipRecTyClss" (ppr name <+> ppr other)

{-
************************************************************************
*                                                                      *
                Kind checking
*                                                                      *
************************************************************************

Note [Kind checking for type and class decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind checking is done thus:

   1. Make up a kind variable for each parameter of the *data* type, class,
      and closed type family decls, and extend the kind environment (which is
      in the TcLclEnv)

   2. Dependency-analyse the type *synonyms* (which must be non-recursive),
      and kind-check them in dependency order.  Extend the kind envt.

   3. Kind check the data type and class decls

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

  class C a where
     op :: D b => a -> b -> b

  class D c where
     bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

However type synonyms work differently.  They can have kinds which don't
just involve (->) and *:
        type R = Int#           -- Kind #
        type S a = Array# a     -- Kind * -> #
        type T a b = (# a,b #)  -- Kind * -> * -> (# a,b #)
and a kind variable can't unify with UnboxedTypeKind.

So we must infer the kinds of type synonyms from their right-hand
sides *first* and then use them, whereas for the mutually recursive
data types D we bring into scope kind bindings D -> k, where k is a
kind variable, and do inference.

NB: synonyms can be mutually recursive with data type declarations though!
   type T = D -> D
   data D = MkD Int T

Open type families
~~~~~~~~~~~~~~~~~~
This treatment of type synonyms only applies to Haskell 98-style synonyms.
General type functions can be recursive, and hence, appear in `alg_decls'.

The kind of an open type family is solely determinded by its kind signature;
hence, only kind signatures participate in the construction of the initial
kind environment (as constructed by `getInitialKind'). In fact, we ignore
instances of families altogether in the following. However, we need to include
the kinds of *associated* families into the construction of the initial kind
environment. (This is handled by `allDecls').
-}

kcTyClGroup :: TyClGroup Name -> TcM [(Name,Kind)]
-- Kind check this group, kind generalize, and return the resulting local env
-- This bindds the TyCons and Classes of the group, but not the DataCons
-- See Note [Kind checking for type and class decls]
kcTyClGroup (TyClGroup { group_tyclds = decls })
  = do  { mod <- getModule
        ; traceTc "kcTyClGroup" (ptext (sLit "module") <+> ppr mod $$ vcat (map ppr decls))

          -- Kind checking;
          --    1. Bind kind variables for non-synonyms
          --    2. Kind-check synonyms, and bind kinds of those synonyms
          --    3. Kind-check non-synonyms
          --    4. Generalise the inferred kinds
          -- See Note [Kind checking for type and class decls]

          -- Step 1: Bind kind variables for non-synonyms
        ; let (syn_decls, non_syn_decls) = partition (isSynDecl . unLoc) decls
        ; initial_kinds <- getInitialKinds non_syn_decls
        ; traceTc "kcTyClGroup: initial kinds" (ppr initial_kinds)

        -- Step 2: Set initial envt, kind-check the synonyms
        ; lcl_env <- tcExtendKindEnv2 initial_kinds $
                     kcSynDecls (calcSynCycles syn_decls)

        -- Step 3: Set extended envt, kind-check the non-synonyms
        ; setLclEnv lcl_env $
          mapM_ kcLTyClDecl non_syn_decls

             -- Step 4: generalisation
             -- Kind checking done for this group
             -- Now we have to kind generalize the flexis
        ; res <- concatMapM (generaliseTCD (tcl_env lcl_env)) decls

        ; traceTc "kcTyClGroup result" (ppr res)
        ; return res }

  where
    generalise :: TcTypeEnv -> Name -> TcM (Name, Kind)
    -- For polymorphic things this is a no-op
    generalise kind_env name
      = do { let kc_kind = case lookupNameEnv kind_env name of
                               Just (AThing k) -> k
                               _ -> pprPanic "kcTyClGroup" (ppr name $$ ppr kind_env)
           ; kvs <- kindGeneralize (tyVarsOfType kc_kind)
           ; kc_kind' <- zonkTcKind kc_kind    -- Make sure kc_kind' has the final,
                                               -- skolemised kind variables
           ; traceTc "Generalise kind" (vcat [ ppr name, ppr kc_kind, ppr kvs, ppr kc_kind' ])
           ; return (name, mkForAllTys kvs kc_kind') }

    generaliseTCD :: TcTypeEnv -> LTyClDecl Name -> TcM [(Name, Kind)]
    generaliseTCD kind_env (L _ decl)
      | ClassDecl { tcdLName = (L _ name), tcdATs = ats } <- decl
      = do { first <- generalise kind_env name
           ; rest <- mapM ((generaliseFamDecl kind_env) . unLoc) ats
           ; return (first : rest) }

      | FamDecl { tcdFam = fam } <- decl
      = do { res <- generaliseFamDecl kind_env fam
           ; return [res] }

      | otherwise
      = do { res <- generalise kind_env (tcdName decl)
           ; return [res] }

    generaliseFamDecl :: TcTypeEnv -> FamilyDecl Name -> TcM (Name, Kind)
    generaliseFamDecl kind_env (FamilyDecl { fdLName = L _ name })
      = generalise kind_env name

mk_thing_env :: [LTyClDecl Name] -> [(Name, TcTyThing)]
mk_thing_env [] = []
mk_thing_env (decl : decls)
  | L _ (ClassDecl { tcdLName = L _ nm, tcdATs = ats }) <- decl
  = (nm, APromotionErr ClassPE) :
    (map (, APromotionErr TyConPE) $ map (unLoc . fdLName . unLoc) ats) ++
    (mk_thing_env decls)

  | otherwise
  = (tcdName (unLoc decl), APromotionErr TyConPE) :
    (mk_thing_env decls)

getInitialKinds :: [LTyClDecl Name] -> TcM [(Name, TcTyThing)]
getInitialKinds decls
  = tcExtendKindEnv2 (mk_thing_env decls) $
    do { pairss <- mapM (addLocM getInitialKind) decls
       ; return (concat pairss) }

getInitialKind :: TyClDecl Name -> TcM [(Name, TcTyThing)]
-- Allocate a fresh kind variable for each TyCon and Class
-- For each tycon, return   (tc, AThing k)
--                 where k is the kind of tc, derived from the LHS
--                       of the definition (and probably including
--                       kind unification variables)
--      Example: data T a b = ...
--      return (T, kv1 -> kv2 -> kv3)
--
-- This pass deals with (ie incorporates into the kind it produces)
--   * The kind signatures on type-variable binders
--   * The result kinds signature on a TyClDecl
--
-- ALSO for each datacon, return (dc, APromotionErr RecDataConPE)
-- Note [ARecDataCon: Recursion and promoting data constructors]
--
-- No family instances are passed to getInitialKinds

getInitialKind decl@(ClassDecl { tcdLName = L _ name, tcdTyVars = ktvs, tcdATs = ats })
  = do { (cl_kind, inner_prs) <-
           kcHsTyVarBndrs (hsDeclHasCusk decl) ktvs $
           do { inner_prs <- getFamDeclInitialKinds ats
              ; return (constraintKind, inner_prs) }
       ; let main_pr = (name, AThing cl_kind)
       ; return (main_pr : inner_prs) }

getInitialKind decl@(DataDecl { tcdLName = L _ name
                              , tcdTyVars = ktvs
                              , tcdDataDefn = HsDataDefn { dd_kindSig = m_sig
                                                         , dd_cons = cons' } })
  = let cons = cons' -- AZ list monad coming
    in
     do { (decl_kind, _) <-
           kcHsTyVarBndrs (hsDeclHasCusk decl) ktvs $
           do { res_k <- case m_sig of
                           Just ksig -> tcLHsKind ksig
                           Nothing   -> return liftedTypeKind
              ; return (res_k, ()) }
       ; let main_pr = (name, AThing decl_kind)
             inner_prs = [ (unLoc con, APromotionErr RecDataConPE)
                         | L _ con' <- cons, con <- con_names con' ]
       ; return (main_pr : inner_prs) }

getInitialKind (FamDecl { tcdFam = decl })
  = getFamDeclInitialKind decl

getInitialKind decl@(SynDecl {})
  = pprPanic "getInitialKind" (ppr decl)

---------------------------------
getFamDeclInitialKinds :: [LFamilyDecl Name] -> TcM [(Name, TcTyThing)]
getFamDeclInitialKinds decls
  = tcExtendKindEnv2 [ (n, APromotionErr TyConPE)
                     | L _ (FamilyDecl { fdLName = L _ n }) <- decls] $
    concatMapM (addLocM getFamDeclInitialKind) decls

getFamDeclInitialKind :: FamilyDecl Name
                      -> TcM [(Name, TcTyThing)]
getFamDeclInitialKind decl@(FamilyDecl { fdLName     = L _ name
                                       , fdTyVars    = ktvs
                                       , fdResultSig = L _ resultSig })
  = do { (fam_kind, _) <-
           kcHsTyVarBndrs (famDeclHasCusk decl) ktvs $
           do { res_k <- case resultSig of
                      KindSig ki                        -> tcLHsKind ki
                      TyVarSig (L _ (KindedTyVar _ ki)) -> tcLHsKind ki
                      _ -- open type families have * return kind by default
                        | famDeclHasCusk decl      -> return liftedTypeKind
                        -- closed type families have their return kind inferred
                        -- by default
                        | otherwise                -> newMetaKindVar
              ; return (res_k, ()) }
       ; return [ (name, AThing fam_kind) ] }

----------------
kcSynDecls :: [SCC (LTyClDecl Name)]
           -> TcM TcLclEnv -- Kind bindings
kcSynDecls [] = getLclEnv
kcSynDecls (group : groups)
  = do  { (n,k) <- kcSynDecl1 group
        ; lcl_env <- tcExtendKindEnv [(n,k)] (kcSynDecls groups)
        ; return lcl_env }

kcSynDecl1 :: SCC (LTyClDecl Name)
           -> TcM (Name,TcKind) -- Kind bindings
kcSynDecl1 (AcyclicSCC (L _ decl)) = kcSynDecl decl
kcSynDecl1 (CyclicSCC decls)       = do { recSynErr decls; failM }
                                     -- Fail here to avoid error cascade
                                     -- of out-of-scope tycons

kcSynDecl :: TyClDecl Name -> TcM (Name, TcKind)
kcSynDecl decl@(SynDecl { tcdTyVars = hs_tvs, tcdLName = L _ name
                        , tcdRhs = rhs })
  -- Returns a possibly-unzonked kind
  = tcAddDeclCtxt decl $
    do { (syn_kind, _) <-
           kcHsTyVarBndrs (hsDeclHasCusk decl) hs_tvs $
           do { traceTc "kcd1" (ppr name <+> brackets (ppr hs_tvs))
              ; (_, rhs_kind) <- tcLHsType rhs
              ; traceTc "kcd2" (ppr name)
              ; return (rhs_kind, ()) }
       ; return (name, syn_kind) }
kcSynDecl decl = pprPanic "kcSynDecl" (ppr decl)

------------------------------------------------------------------------
kcLTyClDecl :: LTyClDecl Name -> TcM ()
  -- See Note [Kind checking for type and class decls]
kcLTyClDecl (L loc decl)
  = setSrcSpan loc $ tcAddDeclCtxt decl $ kcTyClDecl decl

kcTyClDecl :: TyClDecl Name -> TcM ()
-- This function is used solely for its side effect on kind variables
-- NB kind signatures on the type variables and
--    result kind signature have already been dealt with
--    by getInitialKind, so we can ignore them here.

kcTyClDecl (DataDecl { tcdLName = L _ name, tcdTyVars = hs_tvs, tcdDataDefn = defn })
  | HsDataDefn { dd_cons = cons, dd_kindSig = Just _ } <- defn
  = mapM_ (wrapLocM kcConDecl) cons
    -- hs_tvs and dd_kindSig already dealt with in getInitialKind
    -- If dd_kindSig is Just, this must be a GADT-style decl,
    --        (see invariants of DataDefn declaration)
    -- so (a) we don't need to bring the hs_tvs into scope, because the
    --        ConDecls bind all their own variables
    --    (b) dd_ctxt is not allowed for GADT-style decls, so we can ignore it

  | HsDataDefn { dd_ctxt = ctxt, dd_cons = cons } <- defn
  = kcTyClTyVars name hs_tvs $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kcConDecl) cons }

kcTyClDecl decl@(SynDecl {}) = pprPanic "kcTyClDecl" (ppr decl)

kcTyClDecl (ClassDecl { tcdLName = L _ name, tcdTyVars = hs_tvs
                       , tcdCtxt = ctxt, tcdSigs = sigs })
  = kcTyClTyVars name hs_tvs $
    do  { _ <- tcHsContext ctxt
        ; mapM_ (wrapLocM kc_sig)     sigs }
  where
    kc_sig (TypeSig _ op_ty _)  = discardResult (tcHsLiftedType op_ty)
    kc_sig (GenericSig _ op_ty) = discardResult (tcHsLiftedType op_ty)
    kc_sig _                    = return ()

-- closed type families look at their equations, but other families don't
-- do anything here
kcTyClDecl (FamDecl (FamilyDecl { fdLName  = L _ fam_tc_name
                                , fdTyVars = hs_tvs
                                , fdInfo   = ClosedTypeFamily (Just eqns) }))
  = do { tc_kind <- kcLookupKind fam_tc_name
       ; let fam_tc_shape = ( fam_tc_name, length (hsQTvBndrs hs_tvs), tc_kind)
       ; mapM_ (kcTyFamInstEqn fam_tc_shape) eqns }
kcTyClDecl (FamDecl {})    = return ()

-------------------
kcConDecl :: ConDecl Name -> TcM ()
kcConDecl (ConDecl { con_names = names, con_qvars = ex_tvs
                   , con_cxt = ex_ctxt, con_details = details
                   , con_res = res })
  = addErrCtxt (dataConCtxtName names) $
         -- the 'False' says that the existentials don't have a CUSK, as the
         -- concept doesn't really apply here. We just need to bring the variables
         -- into scope!
    do { _ <- kcHsTyVarBndrs False ex_tvs $
              do { _ <- tcHsContext ex_ctxt
                 ; mapM_ (tcHsOpenType . getBangType) (hsConDeclArgTys details)
                 ; _ <- tcConRes res
                 ; return (panic "kcConDecl", ()) }
       ; return () }

{-
Note [Recursion and promoting data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to allow promotion in a strongly connected component
when kind checking.

Consider:
  data T f = K (f (K Any))

When kind checking the `data T' declaration the local env contains the
mappings:
  T -> AThing <some initial kind>
  K -> ARecDataCon

ANothing is only used for DataCons, and only used during type checking
in tcTyClGroup.


************************************************************************
*                                                                      *
\subsection{Type checking}
*                                                                      *
************************************************************************

Note [Type checking recursive type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At this point we have completed *kind-checking* of a mutually
recursive group of type/class decls (done in kcTyClGroup). However,
we discarded the kind-checked types (eg RHSs of data type decls);
note that kcTyClDecl returns ().  There are two reasons:

  * It's convenient, because we don't have to rebuild a
    kinded HsDecl (a fairly elaborate type)

  * It's necessary, because after kind-generalisation, the
    TyCons/Classes may now be kind-polymorphic, and hence need
    to be given kind arguments.

Example:
       data T f a = MkT (f a) (T f a)
During kind-checking, we give T the kind T :: k1 -> k2 -> *
and figure out constraints on k1, k2 etc. Then we generalise
to get   T :: forall k. (k->*) -> k -> *
So now the (T f a) in the RHS must be elaborated to (T k f a).

However, during tcTyClDecl of T (above) we will be in a recursive
"knot". So we aren't allowed to look at the TyCon T itself; we are only
allowed to put it (lazily) in the returned structures.  But when
kind-checking the RHS of T's decl, we *do* need to know T's kind (so
that we can correctly elaboarate (T k f a).  How can we get T's kind
without looking at T?  Delicate answer: during tcTyClDecl, we extend

  *Global* env with T -> ATyCon (the (not yet built) TyCon for T)
  *Local*  env with T -> AThing (polymorphic kind of T)

Then:

  * During TcHsType.kcTyVar we look in the *local* env, to get the
    known kind for T.

  * But in TcHsType.ds_type (and ds_var_app in particular) we look in
    the *global* env to get the TyCon. But we must be careful not to
    force the TyCon or we'll get a loop.

This fancy footwork (with two bindings for T) is only necessary for the
TyCons or Classes of this recursive group.  Earlier, finished groups,
live in the global env only.

Note [Declarations for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For wired-in things we simply ignore the declaration
and take the wired-in information.  That avoids complications.
e.g. the need to make the data constructor worker name for
     a constraint tuple match the wired-in one
-}

tcTyClDecl :: RecTyInfo -> LTyClDecl Name -> TcM [TyThing]
tcTyClDecl rec_info (L loc decl)
  | Just thing <- wiredInNameTyThing_maybe (tcdName decl)
  = return [thing]  -- See Note [Declarations for wired-in things]

  | otherwise
  = setSrcSpan loc $ tcAddDeclCtxt decl $
    do { traceTc "tcTyAndCl-x" (ppr decl)
       ; tcTyClDecl1 NoParentTyCon rec_info decl }

  -- "type family" declarations
tcTyClDecl1 :: TyConParent -> RecTyInfo -> TyClDecl Name -> TcM [TyThing]
tcTyClDecl1 parent _rec_info (FamDecl { tcdFam = fd })
  = tcFamDecl1 parent fd

  -- "type" synonym declaration
tcTyClDecl1 _parent rec_info
            (SynDecl { tcdLName = L _ tc_name, tcdTyVars = tvs, tcdRhs = rhs })
  = ASSERT( isNoParent _parent )
    tcTyClTyVars tc_name tvs $ \ tvs' kind ->
    tcTySynRhs rec_info tc_name tvs' kind rhs

  -- "data/newtype" declaration
tcTyClDecl1 _parent rec_info
            (DataDecl { tcdLName = L _ tc_name, tcdTyVars = tvs, tcdDataDefn = defn })
  = ASSERT( isNoParent _parent )
    tcTyClTyVars tc_name tvs $ \ tvs' kind ->
    tcDataDefn rec_info tc_name tvs' kind defn

tcTyClDecl1 _parent rec_info
            (ClassDecl { tcdLName = L _ class_name, tcdTyVars = tvs
            , tcdCtxt = ctxt, tcdMeths = meths
            , tcdFDs = fundeps, tcdSigs = sigs
            , tcdATs = ats, tcdATDefs = at_defs })
  = ASSERT( isNoParent _parent )
    do { (clas, tvs', gen_dm_env) <- fixM $ \ ~(clas,_,_) ->
            tcTyClTyVars class_name tvs $ \ tvs' kind ->
            do { MASSERT( isConstraintKind kind )
                 -- This little knot is just so we can get
                 -- hold of the name of the class TyCon, which we
                 -- need to look up its recursiveness
               ; traceTc "tcClassDecl 1" (ppr class_name $$ ppr tvs' $$ ppr kind)
               ; let tycon_name = tyConName (classTyCon clas)
                     tc_isrec = rti_is_rec rec_info tycon_name
                     roles = rti_roles rec_info tycon_name

               ; ctxt' <- tcHsContext ctxt
               ; ctxt' <- zonkTcTypeToTypes emptyZonkEnv ctxt'
                       -- Squeeze out any kind unification variables
               ; fds'  <- mapM (addLocM tc_fundep) fundeps
               ; (sig_stuff, gen_dm_env) <- tcClassSigs class_name sigs meths
               ; at_stuff <- tcClassATs class_name (AssocFamilyTyCon clas) ats at_defs
               ; mindef <- tcClassMinimalDef class_name sigs sig_stuff
               ; clas <- buildClass
                            class_name tvs' roles ctxt' fds' at_stuff
                            sig_stuff mindef tc_isrec
               ; traceTc "tcClassDecl" (ppr fundeps $$ ppr tvs' $$ ppr fds')
               ; return (clas, tvs', gen_dm_env) }

       ; let { gen_dm_ids = [ AnId (mkExportedLocalId VanillaId gen_dm_name gen_dm_ty)
                            | (sel_id, GenDefMeth gen_dm_name) <- classOpItems clas
                            , let gen_dm_tau = expectJust "tcTyClDecl1" $
                                               lookupNameEnv gen_dm_env (idName sel_id)
                            , let gen_dm_ty = mkSigmaTy tvs'
                                                      [mkClassPred clas (mkTyVarTys tvs')]
                                                      gen_dm_tau
                            ]
             ; class_ats = map ATyCon (classATs clas) }

       ; return (ATyCon (classTyCon clas) : gen_dm_ids ++ class_ats ) }
         -- NB: Order is important due to the call to `mkGlobalThings' when
         --     tying the the type and class declaration type checking knot.
  where
    tc_fundep (tvs1, tvs2) = do { tvs1' <- mapM tcFdTyVar tvs1
                                ; tvs2' <- mapM tcFdTyVar tvs2
                                ; return (tvs1', tvs2') }

tcFdTyVar :: Located Name -> TcM TcTyVar
-- Look up a type/kind variable in a functional dependency
-- or injectivity annotation.  In the case of kind variables,
-- the environment contains a binding of the kind var to a
-- a SigTv unification variables, which has now fixed.
-- So we must zonk to get the real thing.  Ugh!
tcFdTyVar (L _ name)
  = do { tv <- tcLookupTyVar name
       ; ty <- zonkTyVarOcc emptyZonkEnv tv
       ; case getTyVar_maybe ty of
           Just tv' -> return tv'
           Nothing  -> pprPanic "tcFdTyVar" (ppr name $$ ppr tv $$ ppr ty) }

tcFamDecl1 :: TyConParent -> FamilyDecl Name -> TcM [TyThing]
tcFamDecl1 parent (FamilyDecl { fdInfo = OpenTypeFamily, fdLName = L _ tc_name
                              , fdTyVars = tvs, fdResultSig = L _ sig
                              , fdInjectivityAnn = inj })
  = tcTyClTyVars tc_name tvs $ \ tvs' kind -> do
  { traceTc "open type family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; inj' <- tcInjectivity tvs' inj
  ; let tycon = buildFamilyTyCon tc_name tvs' (resultVariableName sig)
                                 OpenSynFamilyTyCon kind parent inj'
  ; return [ATyCon tycon] }

tcFamDecl1 parent
            (FamilyDecl { fdInfo = ClosedTypeFamily mb_eqns
                        , fdLName = L _ tc_name, fdTyVars = tvs
                        , fdResultSig = L _ sig, fdInjectivityAnn = inj })
-- Closed type families are a little tricky, because they contain the definition
-- of both the type family and the equations for a CoAxiom.
  = do { traceTc "Closed type family:" (ppr tc_name)
         -- the variables in the header scope only over the injectivity
         -- declaration but this is not involved here
       ; (tvs', inj', kind) <- tcTyClTyVars tc_name tvs $ \ tvs' kind ->
                               do { inj' <- tcInjectivity tvs' inj
                                  ; return (tvs', inj', kind) }

       ; checkFamFlag tc_name -- make sure we have -XTypeFamilies

         -- If Nothing, this is an abstract family in a hs-boot file;
         -- but eqns might be empty in the Just case as well
       ; case mb_eqns of
           Nothing   ->
               return [ATyCon $ buildFamilyTyCon tc_name tvs' Nothing
                                     AbstractClosedSynFamilyTyCon kind parent
                                     NotInjective ]
           Just eqns -> do {

         -- Process the equations, creating CoAxBranches
       ; tc_kind <- kcLookupKind tc_name
       ; let fam_tc_shape = (tc_name, length (hsQTvBndrs tvs), tc_kind)

       ; branches <- mapM (tcTyFamInstEqn fam_tc_shape Nothing) eqns
         -- Do not attempt to drop equations dominated by earlier
         -- ones here; in the case of mutual recursion with a data
         -- type, we get a knot-tying failure.  Instead we check
         -- for this afterwards, in TcValidity.checkValidCoAxiom
         -- Example: tc265

         -- Create a CoAxiom, with the correct src location. It is Vitally
         -- Important that we do not pass the branches into
         -- newFamInstAxiomName. They have types that have been zonked inside
         -- the knot and we will die if we look at them. This is OK here
         -- because there will only be one axiom, so we don't need to
         -- differentiate names.
         -- See [Zonking inside the knot] in TcHsType
       ; loc <- getSrcSpanM
       ; co_ax_name <- newFamInstAxiomName loc tc_name []

       ; let mb_co_ax
              | null eqns = Nothing   -- mkBranchedCoAxiom fails on empty list
              | otherwise = Just (mkBranchedCoAxiom co_ax_name fam_tc branches)

             fam_tc = buildFamilyTyCon tc_name tvs' (resultVariableName sig)
                      (ClosedSynFamilyTyCon mb_co_ax) kind parent inj'

       ; return $ ATyCon fam_tc : maybeToList (fmap ACoAxiom mb_co_ax) } }

-- We check for instance validity later, when doing validity checking for
-- the tycon. Exception: checking equations overlap done by dropDominatedAxioms

tcFamDecl1 parent
           (FamilyDecl {fdInfo = DataFamily, fdLName = L _ tc_name, fdTyVars = tvs})
  = tcTyClTyVars tc_name tvs $ \ tvs' kind -> do
  { traceTc "data family:" (ppr tc_name)
  ; checkFamFlag tc_name
  ; extra_tvs <- tcDataKindSig kind
  ; let final_tvs = tvs' ++ extra_tvs    -- we may not need these
        roles     = map (const Nominal) final_tvs
        tycon = buildAlgTyCon tc_name final_tvs roles Nothing []
                              DataFamilyTyCon Recursive
                              False   -- Not promotable to the kind level
                              True    -- GADT syntax
                              parent
  ; return [ATyCon tycon] }

-- | Maybe return a list of Bools that say whether a type family was declared
-- injective in the corresponding type arguments. Length of the list is equal to
-- the number of arguments (including implicit kind arguments). True on position
-- N means that a function is injective in its Nth argument. False means it is
-- not.
tcInjectivity :: [TyVar] -> Maybe (LInjectivityAnn Name)
              -> TcM Injectivity
tcInjectivity _ Nothing
  = return NotInjective

  -- User provided an injectivity annotation, so for each tyvar argument we
  -- check whether a type family was declared injective in that argument. We
  -- return a list of Bools, where True means that corresponding type variable
  -- was mentioned in lInjNames (type family is injective in that argument) and
  -- False means that it was not mentioned in lInjNames (type family is not
  -- injective in that type variable). We also extend injectivity information to
  -- kind variables, so if a user declares:
  --
  --   type family F (a :: k1) (b :: k2) = (r :: k3) | r -> a
  --
  -- then we mark both `a` and `k1` as injective.
  -- NB: the return kind is considered to be *input* argument to a type family.
  -- Since injectivity allows to infer input arguments from the result in theory
  -- we should always mark the result kind variable (`k3` in this example) as
  -- injective.  The reason is that result type has always an assigned kind and
  -- therefore we can always infer the result kind if we know the result type.
  -- But this does not seem to be useful in any way so we don't do it.  (Another
  -- reason is that the implementation would not be straightforward.)
tcInjectivity tvs (Just (L loc (InjectivityAnn _ lInjNames)))
  = setSrcSpan loc $
    do { inj_tvs <- mapM tcFdTyVar lInjNames
       ; let inj_ktvs = closeOverKinds (mkVarSet inj_tvs)
       ; let inj_bools = map (`elemVarSet` inj_ktvs) tvs
       ; traceTc "tcInjectivity" (vcat [ ppr tvs, ppr lInjNames, ppr inj_tvs
                                       , ppr inj_ktvs, ppr inj_bools ])
       ; return $ Injective inj_bools }

tcTySynRhs :: RecTyInfo
           -> Name
           -> [TyVar] -> Kind
           -> LHsType Name -> TcM [TyThing]
tcTySynRhs rec_info tc_name tvs kind hs_ty
  = do { env <- getLclEnv
       ; traceTc "tc-syn" (ppr tc_name $$ ppr (tcl_env env))
       ; rhs_ty <- tcCheckLHsType hs_ty kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; let roles = rti_roles rec_info tc_name
             tycon = buildSynonymTyCon tc_name tvs roles rhs_ty kind
       ; return [ATyCon tycon] }

tcDataDefn :: RecTyInfo -> Name
           -> [TyVar] -> Kind
           -> HsDataDefn Name -> TcM [TyThing]
  -- NB: not used for newtype/data instances (whether associated or not)
tcDataDefn rec_info tc_name tvs kind
         (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                     , dd_ctxt = ctxt, dd_kindSig = mb_ksig
                     , dd_cons = cons' })
 = let cons = cons' -- AZ List monad coming
   in do { extra_tvs <- tcDataKindSig kind
       ; let final_tvs  = tvs ++ extra_tvs
             roles      = rti_roles rec_info tc_name
       ; stupid_tc_theta <- tcHsContext ctxt
       ; stupid_theta    <- zonkTcTypeToTypes emptyZonkEnv stupid_tc_theta
       ; kind_signatures <- xoptM Opt_KindSignatures
       ; is_boot         <- tcIsHsBootOrSig -- Are we compiling an hs-boot file?

             -- Check that we don't use kind signatures without Glasgow extensions
       ; case mb_ksig of
           Nothing   -> return ()
           Just hs_k -> do { checkTc (kind_signatures) (badSigTyDecl tc_name)
                           ; tc_kind <- tcLHsKind hs_k
                           ; checkKind kind tc_kind
                           ; return () }

       ; gadt_syntax <- dataDeclChecks tc_name new_or_data stupid_theta cons

       ; tycon <- fixM $ \ tycon -> do
             { let res_ty = mkTyConApp tycon (mkTyVarTys final_tvs)
             ; data_cons <- tcConDecls new_or_data tycon (final_tvs, res_ty) cons
             ; tc_rhs <-
                 if null cons && is_boot              -- In a hs-boot file, empty cons means
                 then return totallyAbstractTyConRhs  -- "don't know"; hence totally Abstract
                 else case new_or_data of
                   DataType -> return (mkDataTyConRhs data_cons)
                   NewType  -> ASSERT( not (null data_cons) )
                               mkNewTyConRhs tc_name tycon (head data_cons)
             ; return (buildAlgTyCon tc_name final_tvs roles (fmap unLoc cType)
                                     stupid_theta tc_rhs
                                     (rti_is_rec rec_info tc_name)
                                     (rti_promotable rec_info)
                                     gadt_syntax NoParentTyCon) }
       ; return [ATyCon tycon] }

{-
************************************************************************
*                                                                      *
               Typechecking associated types (in class decls)
               (including the associated-type defaults)
*                                                                      *
************************************************************************

Note [Associated type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is an example of associated type defaults:
             class C a where
               data D a

               type F a b :: *
               type F a b = [a]        -- Default

Note that we can get default definitions only for type families, not data
families.
-}

tcClassATs :: Name                  -- The class name (not knot-tied)
           -> TyConParent           -- The class parent of this associated type
           -> [LFamilyDecl Name]    -- Associated types.
           -> [LTyFamDefltEqn Name] -- Associated type defaults.
           -> TcM [ClassATItem]
tcClassATs class_name parent ats at_defs
  = do {  -- Complain about associated type defaults for non associated-types
         sequence_ [ failWithTc (badATErr class_name n)
                   | n <- map at_def_tycon at_defs
                   , not (n `elemNameSet` at_names) ]
       ; mapM tc_at ats }
  where
    at_def_tycon :: LTyFamDefltEqn Name -> Name
    at_def_tycon (L _ eqn) = unLoc (tfe_tycon eqn)

    at_fam_name :: LFamilyDecl Name -> Name
    at_fam_name (L _ decl) = unLoc (fdLName decl)

    at_names = mkNameSet (map at_fam_name ats)

    at_defs_map :: NameEnv [LTyFamDefltEqn Name]
    -- Maps an AT in 'ats' to a list of all its default defs in 'at_defs'
    at_defs_map = foldr (\at_def nenv -> extendNameEnv_C (++) nenv
                                          (at_def_tycon at_def) [at_def])
                        emptyNameEnv at_defs

    tc_at at = do { [ATyCon fam_tc] <- addLocM (tcFamDecl1 parent) at
                  ; let at_defs = lookupNameEnv at_defs_map (at_fam_name at)
                                  `orElse` []
                  ; atd <- tcDefaultAssocDecl fam_tc at_defs
                  ; return (ATI fam_tc atd) }

-------------------------
tcDefaultAssocDecl :: TyCon                         -- ^ Family TyCon
                   -> [LTyFamDefltEqn Name]         -- ^ Defaults
                   -> TcM (Maybe (Type, SrcSpan))   -- ^ Type checked RHS
tcDefaultAssocDecl _ []
  = return Nothing  -- No default declaration

tcDefaultAssocDecl _ (d1:_:_)
  = failWithTc (ptext (sLit "More than one default declaration for")
                <+> ppr (tfe_tycon (unLoc d1)))

tcDefaultAssocDecl fam_tc [L loc (TyFamEqn { tfe_tycon = L _ tc_name
                                           , tfe_pats = hs_tvs
                                           , tfe_rhs = rhs })]
  = setSrcSpan loc $
    tcAddFamInstCtxt (ptext (sLit "default type instance")) tc_name $
    tcTyClTyVars tc_name hs_tvs $ \ tvs rhs_kind ->
    do { traceTc "tcDefaultAssocDecl" (ppr tc_name)
       ; checkTc (isTypeFamilyTyCon fam_tc) (wrongKindOfFamily fam_tc)
       ; let (fam_name, fam_pat_arity, _) = famTyConShape fam_tc
       ; ASSERT( fam_name == tc_name )
         checkTc (length (hsQTvBndrs hs_tvs) == fam_pat_arity)
                 (wrongNumberOfParmsErr fam_pat_arity)
       ; rhs_ty <- tcCheckLHsType rhs rhs_kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; let fam_tc_tvs = tyConTyVars fam_tc
             subst = zipTopTvSubst tvs (mkTyVarTys fam_tc_tvs)
       ; return ( ASSERT( equalLength fam_tc_tvs tvs )
                  Just (substTy subst rhs_ty, loc) ) }
    -- We check for well-formedness and validity later, in checkValidClass

-------------------------
kcTyFamInstEqn :: FamTyConShape -> LTyFamInstEqn Name -> TcM ()
kcTyFamInstEqn fam_tc_shape
    (L loc (TyFamEqn { tfe_pats = pats, tfe_rhs = hs_ty }))
  = setSrcSpan loc $
    discardResult $
    tc_fam_ty_pats fam_tc_shape Nothing -- not an associated type
                   pats (discardResult . (tcCheckLHsType hs_ty))

tcTyFamInstEqn :: FamTyConShape -> Maybe ClsInfo -> LTyFamInstEqn Name -> TcM CoAxBranch
-- Needs to be here, not in TcInstDcls, because closed families
-- (typechecked here) have TyFamInstEqns
tcTyFamInstEqn fam_tc_shape@(fam_tc_name,_,_) mb_clsinfo
    (L loc (TyFamEqn { tfe_tycon = L _ eqn_tc_name
                     , tfe_pats = pats
                     , tfe_rhs = hs_ty }))
  = setSrcSpan loc $
    tcFamTyPats fam_tc_shape mb_clsinfo pats (discardResult . (tcCheckLHsType hs_ty)) $
       \tvs' pats' res_kind ->
    do { checkTc (fam_tc_name == eqn_tc_name)
                 (wrongTyFamName fam_tc_name eqn_tc_name)
       ; rhs_ty <- tcCheckLHsType hs_ty res_kind
       ; rhs_ty <- zonkTcTypeToType emptyZonkEnv rhs_ty
       ; traceTc "tcTyFamInstEqn" (ppr fam_tc_name <+> ppr tvs')
          -- don't print out the pats here, as they might be zonked inside the knot
       ; return (mkCoAxBranch tvs' pats' rhs_ty loc) }

kcDataDefn :: HsDataDefn Name -> TcKind -> TcM ()
-- Used for 'data instance' only
-- Ordinary 'data' is handled by kcTyClDec
kcDataDefn (HsDataDefn { dd_ctxt = ctxt, dd_cons = cons, dd_kindSig = mb_kind }) res_k
  = do  { _ <- tcHsContext ctxt
        ; checkNoErrs $ mapM_ (wrapLocM kcConDecl) cons
          -- See Note [Failing early in kcDataDefn]
        ; kcResultKind mb_kind res_k }

------------------
kcResultKind :: Maybe (LHsKind Name) -> Kind -> TcM ()
kcResultKind Nothing res_k
  = checkKind res_k liftedTypeKind
      --             type family F a
      -- defaults to type family F a :: *
kcResultKind (Just k) res_k
  = do { k' <- tcLHsKind k
       ; checkKind  k' res_k }

{-
Kind check type patterns and kind annotate the embedded type variables.
     type instance F [a] = rhs

 * Here we check that a type instance matches its kind signature, but we do
   not check whether there is a pattern for each type index; the latter
   check is only required for type synonym instances.

Note [tc_fam_ty_pats vs tcFamTyPats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tc_fam_ty_pats does the type checking of the patterns, but it doesn't
zonk or generate any desugaring. It is used when kind-checking closed
type families.

tcFamTyPats type checks the patterns, zonks, and then calls thing_inside
to generate a desugaring. It is used during type-checking (not kind-checking).

Note [Type-checking type patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking the patterns of a family instance declaration, we can't
rely on using the family TyCon, because this is sometimes called
from within a type-checking knot. (Specifically for closed type families.)
The type FamTyConShape gives just enough information to do the job.

The "arity" field of FamTyConShape is the *visible* arity of the family
type constructor, i.e. what the users sees and writes, not including kind
arguments.

See also Note [tc_fam_ty_pats vs tcFamTyPats]

Note [Failing early in kcDataDefn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to use checkNoErrs when calling kcConDecl. This is because kcConDecl
calls tcConDecl, which checks that the return type of a GADT-like constructor
is actually an instance of the type head. Without the checkNoErrs, potentially
two bad things could happen:

 1) Duplicate error messages, because tcConDecl will be called again during
    *type* checking (as opposed to kind checking)
 2) If we just keep blindly forging forward after both kind checking and type
    checking, we can get a panic in rejigConRes. See Trac #8368.
-}

-----------------
type FamTyConShape = (Name, Arity, Kind) -- See Note [Type-checking type patterns]

famTyConShape :: TyCon -> FamTyConShape
famTyConShape fam_tc
  = ( tyConName fam_tc
    , length (filterOut isKindVar (tyConTyVars fam_tc))
    , tyConKind fam_tc )

tc_fam_ty_pats :: FamTyConShape
               -> Maybe ClsInfo
               -> HsWithBndrs Name [LHsType Name] -- Patterns
               -> (TcKind -> TcM ())              -- Kind checker for RHS
                                                  -- result is ignored
               -> TcM ([Kind], [Type], Kind)
-- Check the type patterns of a type or data family instance
--     type instance F <pat1> <pat2> = <type>
-- The 'tyvars' are the free type variables of pats
--
-- NB: The family instance declaration may be an associated one,
-- nested inside an instance decl, thus
--        instance C [a] where
--          type F [a] = ...
-- In that case, the type variable 'a' will *already be in scope*
-- (and, if C is poly-kinded, so will its kind parameter).

tc_fam_ty_pats (name, arity, kind) mb_clsinfo
               (HsWB { hswb_cts = arg_pats, hswb_kvs = kvars
                     , hswb_tvs = tvars, hswb_wcs = wcs })
               kind_checker
  = do { let (fam_kvs, fam_body) = splitForAllTys kind

         -- The splitKindFunTysN below will panic
         -- if there are too many patterns. So, we do a validity check here.
       ; checkTc (length arg_pats == arity) $
         wrongNumberOfParmsErr arity

         -- Instantiate with meta kind vars (or instance kinds)
       ; fam_arg_kinds <- case mb_clsinfo of
           Nothing            -> mapM (const newMetaKindVar) fam_kvs
           Just (_, mini_env) -> mapM mk_arg_kind fam_kvs
             where
               mk_arg_kind kv
                 | Just kind <- lookupVarEnv mini_env kv
                 = return kind
                 | otherwise
                 = newMetaKindVar

       ; loc <- getSrcSpanM
       ; let (arg_kinds, res_kind)
                 = splitKindFunTysN (length arg_pats) $
                   substKiWith fam_kvs fam_arg_kinds fam_body
             -- Treat (anonymous) wild cards as type variables without a name.
             -- See Note [Wild cards in family instances]
             anon_tvs = [L (nameSrcSpan wc) (UserTyVar wc) | wc <- wcs]
             hs_tvs = HsQTvs { hsq_kvs = kvars
                             , hsq_tvs = anon_tvs ++ userHsTyVarBndrs loc tvars }

         -- Kind-check and quantify
         -- See Note [Quantifying over family patterns]
       ; typats <- tcHsTyVarBndrs hs_tvs $ \ _ ->
                   do { kind_checker res_kind
                      ; tcHsArgTys (quotes (ppr name)) arg_pats arg_kinds }

       ; return (fam_arg_kinds, typats, res_kind) }

-- See Note [tc_fam_ty_pats vs tcFamTyPats]
tcFamTyPats :: FamTyConShape
            -> Maybe ClsInfo
            -> HsWithBndrs Name [LHsType Name] -- patterns
            -> (TcKind -> TcM ())              -- kind-checker for RHS
            -> ([TKVar]              -- Kind and type variables
                -> [TcType]          -- Kind and type arguments
                -> Kind -> TcM a)
            -> TcM a
tcFamTyPats fam_shape@(name,_,_) mb_clsinfo pats kind_checker thing_inside
  = do { (fam_arg_kinds, typats, res_kind)
            <- tc_fam_ty_pats fam_shape mb_clsinfo pats kind_checker
       ; let all_args = fam_arg_kinds ++ typats

            -- Find free variables (after zonking) and turn
            -- them into skolems, so that we don't subsequently
            -- replace a meta kind var with AnyK
            -- Very like kindGeneralize
       ; qtkvs <- quantifyTyVars emptyVarSet (tyVarsOfTypes all_args)

            -- Zonk the patterns etc into the Type world
       ; (ze, qtkvs') <- zonkTyBndrsX emptyZonkEnv qtkvs
       ; all_args'    <- zonkTcTypeToTypes ze all_args
       ; res_kind'    <- zonkTcTypeToType  ze res_kind

       ; traceTc "tcFamTyPats" (ppr name)
            -- don't print out too much, as we might be in the knot
       ; tcExtendTyVarEnv qtkvs' $
         thing_inside qtkvs' all_args' res_kind' }

{-
Note [Quantifying over family patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to quantify over two different lots of kind variables:

First, the ones that come from the kinds of the tyvar args of
tcTyVarBndrsKindGen, as usual
  data family Dist a

  -- Proxy :: forall k. k -> *
  data instance Dist (Proxy a) = DP
  -- Generates  data DistProxy = DP
  --            ax8 k (a::k) :: Dist * (Proxy k a) ~ DistProxy k a
  -- The 'k' comes from the tcTyVarBndrsKindGen (a::k)

Second, the ones that come from the kind argument of the type family
which we pick up using the (tyVarsOfTypes typats) in the result of
the thing_inside of tcHsTyvarBndrsGen.
  -- Any :: forall k. k
  data instance Dist Any = DA
  -- Generates  data DistAny k = DA
  --            ax7 k :: Dist k (Any k) ~ DistAny k
  -- The 'k' comes from kindGeneralizeKinds (Any k)

Note [Quantified kind variables of a family pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   type family KindFam (p :: k1) (q :: k1)
           data T :: Maybe k1 -> k2 -> *
           type instance KindFam (a :: Maybe k) b = T a b -> Int
The HsBSig for the family patterns will be ([k], [a])

Then in the family instance we want to
  * Bring into scope [ "k" -> k:BOX, "a" -> a:k ]
  * Kind-check the RHS
  * Quantify the type instance over k and k', as well as a,b, thus
       type instance [k, k', a:Maybe k, b:k']
                     KindFam (Maybe k) k' a b = T k k' a b -> Int

Notice that in the third step we quantify over all the visibly-mentioned
type variables (a,b), but also over the implicitly mentioned kind variables
(k, k').  In this case one is bound explicitly but often there will be
none. The role of the kind signature (a :: Maybe k) is to add a constraint
that 'a' must have that kind, and to bring 'k' into scope.


Note [Wild cards in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Wild cards can be used in type/data family instance declarations to indicate
that the name of a type variable doesn't matter. Each wild card will be
replaced with a new unique type variable. For instance:

    type family F a b :: *
    type instance F Int _ = Int

is the same as

    type family F a b :: *
    type instance F Int b = Int

This is implemented as follows: during renaming anonymous wild cards are given
freshly generated names. These names are collected after renaming
(rnFamInstDecl) and used to make new type variables during type checking
(tc_fam_ty_pats). One should not confuse these wild cards with the ones from
partial type signatures. The latter generate fresh meta-variables whereas the
former generate fresh skolems.

Named and extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699 and #10586.

************************************************************************
*                                                                      *
               Data types
*                                                                      *
************************************************************************
-}

dataDeclChecks :: Name -> NewOrData -> ThetaType -> [LConDecl Name] -> TcM Bool
dataDeclChecks tc_name new_or_data stupid_theta cons
  = do {   -- Check that we don't use GADT syntax in H98 world
         gadtSyntax_ok <- xoptM Opt_GADTSyntax
       ; let gadt_syntax = consUseGadtSyntax cons
       ; checkTc (gadtSyntax_ok || not gadt_syntax) (badGadtDecl tc_name)

           -- Check that the stupid theta is empty for a GADT-style declaration
       ; checkTc (null stupid_theta || not gadt_syntax) (badStupidTheta tc_name)

         -- Check that a newtype has exactly one constructor
         -- Do this before checking for empty data decls, so that
         -- we don't suggest -XEmptyDataDecls for newtypes
       ; checkTc (new_or_data == DataType || isSingleton cons)
                (newtypeConError tc_name (length cons))

                -- Check that there's at least one condecl,
         -- or else we're reading an hs-boot file, or -XEmptyDataDecls
       ; empty_data_decls <- xoptM Opt_EmptyDataDecls
       ; is_boot <- tcIsHsBootOrSig  -- Are we compiling an hs-boot file?
       ; checkTc (not (null cons) || empty_data_decls || is_boot)
                 (emptyConDeclsErr tc_name)
       ; return gadt_syntax }


-----------------------------------
consUseGadtSyntax :: [LConDecl a] -> Bool
consUseGadtSyntax (L _ (ConDecl { con_res = ResTyGADT _ _ }) : _) = True
consUseGadtSyntax _                                               = False
                 -- All constructors have same shape

-----------------------------------
tcConDecls :: NewOrData -> TyCon -> ([TyVar], Type)
           -> [LConDecl Name] -> TcM [DataCon]
tcConDecls new_or_data rep_tycon (tmpl_tvs, res_tmpl) cons
  = concatMapM (addLocM  $ tcConDecl new_or_data rep_tycon tmpl_tvs res_tmpl)
               cons

tcConDecl :: NewOrData
          -> TyCon             -- Representation tycon
          -> [TyVar] -> Type   -- Return type template (with its template tyvars)
                               --    (tvs, T tys), where T is the family TyCon
          -> ConDecl Name
          -> TcM [DataCon]

tcConDecl new_or_data rep_tycon tmpl_tvs res_tmpl        -- Data types
          (ConDecl { con_names = names
                   , con_qvars = hs_tvs, con_cxt = hs_ctxt
                   , con_details = hs_details, con_res = hs_res_ty })
  = addErrCtxt (dataConCtxtName names) $
    do { traceTc "tcConDecl 1" (ppr names)
       ; (ctxt, arg_tys, res_ty, field_lbls, stricts)
           <- tcHsTyVarBndrs hs_tvs $ \ _ ->
              do { ctxt    <- tcHsContext hs_ctxt
                 ; btys    <- tcConArgs new_or_data hs_details
                 ; res_ty  <- tcConRes hs_res_ty
                 ; field_lbls <- lookupConstructorFields (unLoc $ head names)
                 ; let (arg_tys, stricts) = unzip btys
                 ; return (ctxt, arg_tys, res_ty, field_lbls, stricts)
                 }

             -- Generalise the kind variables (returning quantified TcKindVars)
             -- and quantify the type variables (substituting their kinds)
             -- REMEMBER: 'tkvs' are:
             --    ResTyH98:  the *existential* type variables only
             --    ResTyGADT: *all* the quantified type variables
             -- c.f. the comment on con_qvars in HsDecls
       ; tkvs <- case res_ty of
                   ResTyH98           -> quantifyTyVars (mkVarSet tmpl_tvs)
                                                 (tyVarsOfTypes (ctxt++arg_tys))
                   ResTyGADT _ res_ty -> quantifyTyVars emptyVarSet
                                          (tyVarsOfTypes (res_ty:ctxt++arg_tys))

             -- Zonk to Types
       ; (ze, qtkvs) <- zonkTyBndrsX emptyZonkEnv tkvs
       ; arg_tys <- zonkTcTypeToTypes ze arg_tys
       ; ctxt    <- zonkTcTypeToTypes ze ctxt
       ; res_ty  <- case res_ty of
                      ResTyH98        -> return ResTyH98
                      ResTyGADT ls ty -> ResTyGADT ls <$> zonkTcTypeToType ze ty

       ; let (univ_tvs, ex_tvs, eq_preds, res_ty') = rejigConRes tmpl_tvs res_tmpl qtkvs res_ty
             -- NB: this is a /lazy/ binding, so we pass four thunks to buildDataCon
             --     without yet forcing the guards in rejigConRes
             -- See Note [Checking GADT return types]

       ; fam_envs <- tcGetFamInstEnvs
       ; let
           buildOneDataCon (L _ name) = do
             { is_infix <- tcConIsInfix name hs_details res_ty
             ; buildDataCon fam_envs name is_infix
                            stricts Nothing field_lbls
                            univ_tvs ex_tvs eq_preds ctxt arg_tys
                            res_ty' rep_tycon
                  -- NB:  we put data_tc, the type constructor gotten from the
                  --      constructor type signature into the data constructor;
                  --      that way checkValidDataCon can complain if it's wrong.
             }
       ; mapM buildOneDataCon names
       }


tcConIsInfix :: Name
             -> HsConDetails (LHsType Name) (Located [LConDeclField Name])
             -> ResType Type
             -> TcM Bool
tcConIsInfix _   details ResTyH98
  = case details of
           InfixCon {}  -> return True
           _            -> return False
tcConIsInfix con details (ResTyGADT _ _)
  = case details of
           InfixCon {}  -> return True
           RecCon {}    -> return False
           PrefixCon arg_tys           -- See Note [Infix GADT cons]
               | isSymOcc (getOccName con)
               , [_ty1,_ty2] <- arg_tys
                  -> do { fix_env <- getFixityEnv
                        ; return (con `elemNameEnv` fix_env) }
               | otherwise -> return False



tcConArgs :: NewOrData -> HsConDeclDetails Name
          -> TcM [(TcType, HsSrcBang)]
tcConArgs new_or_data (PrefixCon btys)
  = mapM (tcConArg new_or_data) btys
tcConArgs new_or_data (InfixCon bty1 bty2)
  = do { bty1' <- tcConArg new_or_data bty1
       ; bty2' <- tcConArg new_or_data bty2
       ; return [bty1', bty2'] }
tcConArgs new_or_data (RecCon fields)
  = mapM (tcConArg new_or_data) btys
  where
    -- We need a one-to-one mapping from field_names to btys
    combined = map (\(L _ f) -> (cd_fld_names f,cd_fld_type f)) (unLoc fields)
    explode (ns,ty) = zip ns (repeat ty)
    exploded = concatMap explode combined
    (_,btys) = unzip exploded


tcConArg :: NewOrData -> LHsType Name -> TcM (TcType, HsSrcBang)
tcConArg new_or_data bty
  = do  { traceTc "tcConArg 1" (ppr bty)
        ; arg_ty <- tcHsConArgType new_or_data bty
        ; traceTc "tcConArg 2" (ppr bty)
        ; return (arg_ty, getBangStrictness bty) }

tcConRes :: ResType (LHsType Name) -> TcM (ResType Type)
tcConRes ResTyH98           = return ResTyH98
tcConRes (ResTyGADT ls res_ty) = do { res_ty' <- tcHsLiftedType res_ty
                                    ; return (ResTyGADT ls res_ty') }

{-
Note [Infix GADT constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not currently have syntax to declare an infix constructor in GADT syntax,
but it makes a (small) difference to the Show instance.  So as a slightly
ad-hoc solution, we regard a GADT data constructor as infix if
  a) it is an operator symbol
  b) it has two arguments
  c) there is a fixity declaration for it
For example:
   infix 6 (:--:)
   data T a where
     (:--:) :: t1 -> t2 -> T Int


Note [Checking GADT return types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a delicacy around checking the return types of a datacon. The
central problem is dealing with a declaration like

  data T a where
    MkT :: T a -> Q a

Note that the return type of MkT is totally bogus. When creating the T
tycon, we also need to create the MkT datacon, which must have a "rejigged"
return type. That is, the MkT datacon's type must be transformed to have
a uniform return type with explicit coercions for GADT-like type parameters.
This rejigging is what rejigConRes does. The problem is, though, that checking
that the return type is appropriate is much easier when done over *Type*,
not *HsType*, and doing a call to tcMatchTy will loop because T isn't fully
defined yet.

So, we want to make rejigConRes lazy and then check the validity of
the return type in checkValidDataCon.  To do this we /always/ return a
4-tuple from rejigConRes (so that we can extract ret_ty from it, which
checkValidDataCon needs), but the first three fields may be bogus if
the return type isn't valid (the last equation for rejigConRes).

This is better than an earlier solution which reduced the number of
errors reported in one pass.  See Trac #7175, and #10836.
-}

-- Example
--   data instance T (b,c) where
--      TI :: forall e. e -> T (e,e)
--
-- The representation tycon looks like this:
--   data :R7T b c where
--      TI :: forall b1 c1. (b1 ~ c1) => b1 -> :R7T b1 c1
-- In this case orig_res_ty = T (e,e)

rejigConRes :: [TyVar] -> Type  -- Template for result type; e.g.
                                -- data instance T [a] b c = ...
                                --      gives template ([a,b,c], T [a] b c)
             -> [TyVar]         -- where MkT :: forall x y z. ...
             -> ResType Type
             -> ([TyVar],               -- Universal
                 [TyVar],               -- Existential (distinct OccNames from univs)
                 [(TyVar,Type)],        -- Equality predicates
                 Type)          -- Typechecked return type
        -- We don't check that the TyCon given in the ResTy is
        -- the same as the parent tycon, because checkValidDataCon will do it

rejigConRes tmpl_tvs res_ty dc_tvs ResTyH98
  = (tmpl_tvs, dc_tvs, [], res_ty)
        -- In H98 syntax the dc_tvs are the existential ones
        --      data T a b c = forall d e. MkT ...
        -- The universals {a,b,c} are tc_tvs, and the existentials {d,e} are dc_tvs

rejigConRes tmpl_tvs res_tmpl dc_tvs (ResTyGADT _ res_ty)
        -- E.g.  data T [a] b c where
        --         MkT :: forall x y z. T [(x,y)] z z
        -- The {a,b,c} are the tmpl_tvs, and the {x,y,z} are the dc_tvs
        --     (NB: unlike the H98 case, the dc_tvs are not all existential)
        -- Then we generate
        --      Univ tyvars     Eq-spec
        --          a              a~(x,y)
        --          b              b~z
        --          z
        -- Existentials are the leftover type vars: [x,y]
        -- So we return ([a,b,z], [x,y], [a~(x,y),b~z], T [(x,y)] z z)
  | Just subst <- tcMatchTy (mkVarSet tmpl_tvs) res_tmpl res_ty
  , (univ_tvs, eq_spec) <- foldr (choose subst) ([], []) tmpl_tvs
  , let ex_tvs = dc_tvs `minusList` univ_tvs
  = (univ_tvs, ex_tvs, eq_spec, res_ty)

  | otherwise
        -- If the return type of the data constructor doesn't match the parent
        -- type constructor, or the arity is wrong, the tcMatchTy will fail
        --    e.g   data T a b where
        --            T1 :: Maybe a   -- Wrong tycon
        --            T2 :: T [a]     -- Wrong arity
        -- We are detect that later, in checkValidDataCon, but meanwhile
        -- we must do *something*, not just crash.  So we do something simple
        -- albeit bogus, relying on checkValidDataCon to check the
        --  bad-result-type error before seeing that the other fields look odd
        -- See Note [Checking GADT return types]
  = (tmpl_tvs, dc_tvs `minusList` tmpl_tvs, [], res_ty)
  where
    -- Figure out the univ_tvs etc
    -- Each univ_tv is either a dc_tv or a tmpl_tv
    choose subst tmpl (univs, eqs)
      | Just ty <- lookupTyVar subst tmpl
      = case tcGetTyVar_maybe ty of
          Just tv | not (tv `elem` univs)
            -> (tv:univs,   eqs)
          _other  -> (new_tmpl:univs, (new_tmpl,ty):eqs)
                     where  -- see Note [Substitution in template variables kinds]
                       new_tmpl = updateTyVarKind (substTy subst) tmpl
      | otherwise = pprPanic "tcResultType" (ppr res_ty)

{-
Note [Substitution in template variables kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data List a = Nil | Cons a (List a)
data SList s as where
  SNil :: SList s Nil

We call tcResultType with
  tmpl_tvs = [(k :: BOX), (s :: k -> *), (as :: List k)]
  res_tmpl = SList k s as
  res_ty = ResTyGADT (SList k1 (s1 :: k1 -> *) (Nil k1))

We get subst:
  k -> k1
  s -> s1
  as -> Nil k1

Now we want to find out the universal variables and the equivalences
between some of them and types (GADT).

In this example, k and s are mapped to exactly variables which are not
already present in the universal set, so we just add them without any
coercion.

But 'as' is mapped to 'Nil k1', so we add 'as' to the universal set,
and add the equivalence with 'Nil k1' in 'eqs'.

The problem is that with kind polymorphism, as's kind may now contain
kind variables, and we have to apply the template substitution to it,
which is why we create new_tmpl.

The template substitution only maps kind variables to kind variables,
since GADTs are not kind indexed.

************************************************************************
*                                                                      *
                Validity checking
*                                                                      *
************************************************************************

Validity checking is done once the mutually-recursive knot has been
tied, so we can look at things freely.
-}

checkClassCycleErrs :: Class -> TcM ()
checkClassCycleErrs cls = mapM_ recClsErr (calcClassCycles cls)

checkValidTyCl :: TyThing -> TcM ()
checkValidTyCl thing
  = setSrcSpan (getSrcSpan thing) $
    addTyThingCtxt thing $
    case thing of
      ATyCon tc -> checkValidTyCon tc
      AnId _    -> return ()  -- Generic default methods are checked
                              -- with their parent class
      ACoAxiom _ -> return () -- Axioms checked with their parent
                              -- closed family tycon
      _         -> pprTrace "checkValidTyCl" (ppr thing) $ return ()

-------------------------
-- For data types declared with record syntax, we require
-- that each constructor that has a field 'f'
--      (a) has the same result type
--      (b) has the same type for 'f'
-- module alpha conversion of the quantified type variables
-- of the constructor.
--
-- Note that we allow existentials to match because the
-- fields can never meet. E.g
--      data T where
--        T1 { f1 :: b, f2 :: a, f3 ::Int } :: T
--        T2 { f1 :: c, f2 :: c, f3 ::Int } :: T
-- Here we do not complain about f1,f2 because they are existential

checkValidTyCon :: TyCon -> TcM ()
checkValidTyCon tc
  | isPrimTyCon tc   -- Happens when Haddock'ing GHC.Prim
  = return ()

  | Just cl <- tyConClass_maybe tc
  = checkValidClass cl

  | Just syn_rhs <- synTyConRhs_maybe tc
  = checkValidType syn_ctxt syn_rhs

  | Just fam_flav <- famTyConFlav_maybe tc
  = case fam_flav of
    { ClosedSynFamilyTyCon (Just ax) -> tcAddClosedTypeFamilyDeclCtxt tc $
                                        checkValidCoAxiom ax
    ; ClosedSynFamilyTyCon Nothing   -> return ()
    ; AbstractClosedSynFamilyTyCon ->
      do { hsBoot <- tcIsHsBootOrSig
         ; checkTc hsBoot $
           ptext (sLit "You may define an abstract closed type family") $$
           ptext (sLit "only in a .hs-boot file") }
    ; OpenSynFamilyTyCon           -> return ()
    ; BuiltInSynFamTyCon _         -> return () }

  | otherwise
  = do { -- Check the context on the data decl
         traceTc "cvtc1" (ppr tc)
       ; checkValidTheta (DataTyCtxt name) (tyConStupidTheta tc)

       ; traceTc "cvtc2" (ppr tc)

       ; dflags          <- getDynFlags
       ; existential_ok  <- xoptM Opt_ExistentialQuantification
       ; gadt_ok         <- xoptM Opt_GADTs
       ; let ex_ok = existential_ok || gadt_ok  -- Data cons can have existential context
       ; mapM_ (checkValidDataCon dflags ex_ok tc) data_cons

        -- Check that fields with the same name share a type
       ; mapM_ check_fields groups }

  where
    syn_ctxt  = TySynCtxt name
    name      = tyConName tc
    data_cons = tyConDataCons tc

    groups = equivClasses cmp_fld (concatMap get_fields data_cons)
    cmp_fld (f1,_) (f2,_) = flLabel f1 `compare` flLabel f2
    get_fields con = dataConFieldLabels con `zip` repeat con
        -- dataConFieldLabels may return the empty list, which is fine

    -- See Note [GADT record selectors] in MkId.hs
    -- We must check (a) that the named field has the same
    --                   type in each constructor
    --               (b) that those constructors have the same result type
    --
    -- However, the constructors may have differently named type variable
    -- and (worse) we don't know how the correspond to each other.  E.g.
    --     C1 :: forall a b. { f :: a, g :: b } -> T a b
    --     C2 :: forall d c. { f :: c, g :: c } -> T c d
    --
    -- So what we do is to ust Unify.tcMatchTys to compare the first candidate's
    -- result type against other candidates' types BOTH WAYS ROUND.
    -- If they magically agrees, take the substitution and
    -- apply them to the latter ones, and see if they match perfectly.
    check_fields ((label, con1) : other_fields)
        -- These fields all have the same name, but are from
        -- different constructors in the data type
        = recoverM (return ()) $ mapM_ checkOne other_fields
                -- Check that all the fields in the group have the same type
                -- NB: this check assumes that all the constructors of a given
                -- data type use the same type variables
        where
        (tvs1, _, _, res1) = dataConSig con1
        ts1 = mkVarSet tvs1
        fty1 = dataConFieldType con1 lbl
        lbl = flLabel label

        checkOne (_, con2)    -- Do it bothways to ensure they are structurally identical
            = do { checkFieldCompat lbl con1 con2 ts1 res1 res2 fty1 fty2
                 ; checkFieldCompat lbl con2 con1 ts2 res2 res1 fty2 fty1 }
            where
                (tvs2, _, _, res2) = dataConSig con2
                ts2 = mkVarSet tvs2
                fty2 = dataConFieldType con2 lbl
    check_fields [] = panic "checkValidTyCon/check_fields []"

checkFieldCompat :: FieldLabelString -> DataCon -> DataCon -> TyVarSet
                 -> Type -> Type -> Type -> Type -> TcM ()
checkFieldCompat fld con1 con2 tvs1 res1 res2 fty1 fty2
  = do  { checkTc (isJust mb_subst1) (resultTypeMisMatch fld con1 con2)
        ; checkTc (isJust mb_subst2) (fieldTypeMisMatch fld con1 con2) }
  where
    mb_subst1 = tcMatchTy tvs1 res1 res2
    mb_subst2 = tcMatchTyX tvs1 (expectJust "checkFieldCompat" mb_subst1) fty1 fty2

-------------------------------
checkValidDataCon :: DynFlags -> Bool -> TyCon -> DataCon -> TcM ()
checkValidDataCon dflags existential_ok tc con
  = setSrcSpan (srcLocSpan (getSrcLoc con))     $
    addErrCtxt (dataConCtxt con)                $
    do  { -- Check that the return type of the data constructor
          -- matches the type constructor; eg reject this:
          --   data T a where { MkT :: Bogus a }
          -- It's important to do this first:
          --  see Note [Checking GADT return types]
          --  and c.f. Note [Check role annotations in a second pass]
          let tc_tvs      = tyConTyVars tc
              res_ty_tmpl = mkFamilyTyConApp tc (mkTyVarTys tc_tvs)
              orig_res_ty = dataConOrigResTy con
        ; traceTc "checkValidDataCon" (vcat
              [ ppr con, ppr tc, ppr tc_tvs
              , ppr res_ty_tmpl <+> dcolon <+> ppr (typeKind res_ty_tmpl)
              , ppr orig_res_ty <+> dcolon <+> ppr (typeKind orig_res_ty)])

        ; checkTc (isJust (tcMatchTy (mkVarSet tc_tvs)
                                     res_ty_tmpl
                                     orig_res_ty))
                  (badDataConTyCon con res_ty_tmpl orig_res_ty)

          -- Check that the result type is a *monotype*
          --  e.g. reject this:   MkT :: T (forall a. a->a)
          -- Reason: it's really the argument of an equality constraint
        ; checkValidMonoType orig_res_ty

          -- Check all argument types for validity
        ; checkValidType ctxt (dataConUserType con)

          -- Extra checks for newtype data constructors
        ; when (isNewTyCon tc) (checkNewDataCon con)

          -- Check that UNPACK pragmas and bangs work out
          -- E.g.  reject   data T = MkT {-# UNPACK #-} Int     -- No "!"
          --                data T = MkT {-# UNPACK #-} !a      -- Can't unpack
        ; mapM_ check_bang (zip3 (dataConSrcBangs con) (dataConImplBangs con) [1..])

          -- Check that existentials are allowed if they are used
        ; checkTc (existential_ok || isVanillaDataCon con)
                  (badExistential con)

          -- Check that we aren't doing GADT type refinement on kind variables
          -- e.g reject    data T (a::k) where
          --                  T1 :: T Int
          --                  T2 :: T Maybe
        ; checkTc (not (any (isKindVar . fst) (dataConEqSpec con)))
                  (badGadtKindCon con)

        ; traceTc "Done validity of data con" (ppr con <+> ppr (dataConRepType con))
    }
  where
    ctxt = ConArgCtxt (dataConName con)

    check_bang (HsSrcBang _ _ SrcLazy, _, n)
      | not (xopt Opt_StrictData dflags)
      = addErrTc
          (bad_bang n (ptext (sLit "Lazy annotation (~) without StrictData")))
    check_bang (HsSrcBang _ want_unpack strict_mark, rep_bang, n)
      | isSrcUnpacked want_unpack, not is_strict
      = addWarnTc (bad_bang n (ptext (sLit "UNPACK pragma lacks '!'")))
      | isSrcUnpacked want_unpack
      , case rep_bang of { HsUnpack {} -> False; _ -> True }
      , not (gopt Opt_OmitInterfacePragmas dflags)
           -- If not optimising, se don't unpack, so don't complain!
           -- See MkId.dataConArgRep, the (HsBang True) case
      = addWarnTc (bad_bang n (ptext (sLit "Ignoring unusable UNPACK pragma")))
      where
        is_strict = case strict_mark of
                      NoSrcStrict -> xopt Opt_StrictData dflags
                      bang        -> isSrcStrict bang

    check_bang _
      = return ()

    bad_bang n herald
      = hang herald 2 (ptext (sLit "on the") <+> speakNth n
                       <+> ptext (sLit "argument of") <+> quotes (ppr con))
-------------------------------
checkNewDataCon :: DataCon -> TcM ()
-- Further checks for the data constructor of a newtype
checkNewDataCon con
  = do  { checkTc (isSingleton arg_tys) (newtypeFieldErr con (length arg_tys))
              -- One argument

        ; check_con (null eq_spec) $
          ptext (sLit "A newtype constructor must have a return type of form T a1 ... an")
                -- Return type is (T a b c)

        ; check_con (null theta) $
          ptext (sLit "A newtype constructor cannot have a context in its type")

        ; check_con (null ex_tvs) $
          ptext (sLit "A newtype constructor cannot have existential type variables")
                -- No existentials

        ; checkTc (all ok_bang (dataConSrcBangs con))
                  (newtypeStrictError con)
                -- No strictness annotations
    }
  where
    (_univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty) = dataConFullSig con

    check_con what msg
       = checkTc what (msg $$ ppr con <+> dcolon <+> ppr (dataConUserType con))

    ok_bang (HsSrcBang _ _ SrcStrict) = False
    ok_bang (HsSrcBang _ _ SrcLazy)   = False
    ok_bang _                         = True

-------------------------------
checkValidClass :: Class -> TcM ()
checkValidClass cls
  = do  { constrained_class_methods <- xoptM Opt_ConstrainedClassMethods
        ; multi_param_type_classes <- xoptM Opt_MultiParamTypeClasses
        ; nullary_type_classes <- xoptM Opt_NullaryTypeClasses
        ; fundep_classes <- xoptM Opt_FunctionalDependencies

        -- Check that the class is unary, unless multiparameter type classes
        -- are enabled; also recognize deprecated nullary type classes
        -- extension (subsumed by multiparameter type classes, Trac #8993)
        ; checkTc (multi_param_type_classes || cls_arity == 1 ||
                    (nullary_type_classes && cls_arity == 0))
                  (classArityErr cls_arity cls)
        ; checkTc (fundep_classes || null fundeps) (classFunDepsErr cls)

        -- Check the super-classes
        ; checkValidTheta (ClassSCCtxt (className cls)) theta

          -- Now check for cyclic superclasses
          -- If there are superclass cycles, checkClassCycleErrs bails.
        ; checkClassCycleErrs cls

        -- Check the class operations.
        -- But only if there have been no earlier errors
        -- See Note [Abort when superclass cycle is detected]
        ; whenNoErrs $
          mapM_ (check_op constrained_class_methods) op_stuff

        -- Check the associated type defaults are well-formed and instantiated
        ; mapM_ check_at at_stuff  }
  where
    (tyvars, fundeps, theta, _, at_stuff, op_stuff) = classExtraBigSig cls
    cls_arity = count isTypeVar tyvars    -- Ignore kind variables
    cls_tv_set = mkVarSet tyvars

    check_op constrained_class_methods (sel_id, dm)
      = setSrcSpan (getSrcSpan sel_id) $
        addErrCtxt (classOpCtxt sel_id op_ty) $ do
        { traceTc "class op type" (ppr op_ty)
        ; checkValidType ctxt op_ty
                -- This implements the ambiguity check, among other things
                -- Example: tc223
                --   class Error e => Game b mv e | b -> mv e where
                --      newBoard :: MonadState b m => m ()
                -- Here, MonadState has a fundep m->b, so newBoard is fine

        ; unless constrained_class_methods $
          mapM_ check_constraint (tail (theta1 ++ theta2))

        ; case dm of
            GenDefMeth dm_name -> do { dm_id <- tcLookupId dm_name
                                     ; checkValidType ctxt (idType dm_id) }
            _                  -> return ()
        }
        where
          ctxt    = FunSigCtxt op_name True -- Report redundant class constraints
          op_name = idName sel_id
          op_ty   = idType sel_id
          (_,theta1,tau1) = tcSplitSigmaTy op_ty
          (_,theta2,_)    = tcSplitSigmaTy tau1

          check_constraint :: TcPredType -> TcM ()
          check_constraint pred
            = when (tyVarsOfType pred `subVarSet` cls_tv_set)
                   (addErrTc (badMethPred sel_id pred))

    check_at (ATI fam_tc m_dflt_rhs)
      = do { checkTc (cls_arity == 0 || any (`elemVarSet` cls_tv_set) fam_tvs)
                     (noClassTyVarErr cls fam_tc)
                        -- Check that the associated type mentions at least
                        -- one of the class type variables
                        -- The check is disabled for nullary type classes,
                        -- since there is no possible ambiguity (Trac #10020)
           ; whenIsJust m_dflt_rhs $ \ (rhs, loc) ->
             checkValidTyFamEqn (Just (cls, mini_env)) fam_tc
                                fam_tvs (mkTyVarTys fam_tvs) rhs loc }
        where
          fam_tvs = tyConTyVars fam_tc
    mini_env = zipVarEnv tyvars (mkTyVarTys tyvars)

checkFamFlag :: Name -> TcM ()
-- Check that we don't use families without -XTypeFamilies
-- The parser won't even parse them, but I suppose a GHC API
-- client might have a go!
checkFamFlag tc_name
  = do { idx_tys <- xoptM Opt_TypeFamilies
       ; checkTc idx_tys err_msg }
  where
    err_msg = hang (ptext (sLit "Illegal family declaration for") <+> quotes (ppr tc_name))
                 2 (ptext (sLit "Use TypeFamilies to allow indexed type families"))

{-
Note [Abort when superclass cycle is detected]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must avoid doing the ambiguity check for the methods (in
checkValidClass.check_op) when there are already errors accumulated.
This is because one of the errors may be a superclass cycle, and
superclass cycles cause canonicalization to loop. Here is a
representative example:

  class D a => C a where
    meth :: D a => ()
  class C a => D a

This fixes Trac #9415, #9739

************************************************************************
*                                                                      *
                Checking role validity
*                                                                      *
************************************************************************
-}

checkValidRoleAnnots :: RoleAnnots -> TyThing -> TcM ()
checkValidRoleAnnots role_annots thing
  = case thing of
    { ATyCon tc
        | isTypeSynonymTyCon tc -> check_no_roles
        | isFamilyTyCon tc      -> check_no_roles
        | isAlgTyCon tc         -> check_roles
        where
          name                   = tyConName tc

     -- Role annotations are given only on *type* variables, but a tycon stores
     -- roles for all variables. So, we drop the kind roles (which are all
     -- Nominal, anyway).
          tyvars                 = tyConTyVars tc
          roles                  = tyConRoles tc
          (kind_vars, type_vars) = span isKindVar tyvars
          type_roles             = dropList kind_vars roles
          role_annot_decl_maybe  = lookupRoleAnnots role_annots name

          check_roles
            = whenIsJust role_annot_decl_maybe $
                \decl@(L loc (RoleAnnotDecl _ the_role_annots)) ->
                addRoleAnnotCtxt name $
                setSrcSpan loc $ do
                { role_annots_ok <- xoptM Opt_RoleAnnotations
                ; checkTc role_annots_ok $ needXRoleAnnotations tc
                ; checkTc (type_vars `equalLength` the_role_annots)
                          (wrongNumberOfRoles type_vars decl)
                ; _ <- zipWith3M checkRoleAnnot type_vars the_role_annots type_roles
                -- Representational or phantom roles for class parameters
                -- quickly lead to incoherence. So, we require
                -- IncoherentInstances to have them. See #8773.
                ; incoherent_roles_ok <- xoptM Opt_IncoherentInstances
                ; checkTc (  incoherent_roles_ok
                          || (not $ isClassTyCon tc)
                          || (all (== Nominal) type_roles))
                          incoherentRoles

                ; lint <- goptM Opt_DoCoreLinting
                ; when lint $ checkValidRoles tc }

          check_no_roles
            = whenIsJust role_annot_decl_maybe illegalRoleAnnotDecl
    ; _ -> return () }

checkRoleAnnot :: TyVar -> Located (Maybe Role) -> Role -> TcM ()
checkRoleAnnot _  (L _ Nothing)   _  = return ()
checkRoleAnnot tv (L _ (Just r1)) r2
  = when (r1 /= r2) $
    addErrTc $ badRoleAnnot (tyVarName tv) r1 r2

-- This is a double-check on the role inference algorithm. It is only run when
-- -dcore-lint is enabled. See Note [Role inference] in TcTyDecls
checkValidRoles :: TyCon -> TcM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in CoreLint
checkValidRoles tc
  | isAlgTyCon tc
    -- tyConDataCons returns an empty list for data families
  = mapM_ check_dc_roles (tyConDataCons tc)
  | Just rhs <- synTyConRhs_maybe tc
  = check_ty_roles (zipVarEnv (tyConTyVars tc) (tyConRoles tc)) Representational rhs
  | otherwise
  = return ()
  where
    check_dc_roles datacon
      = do { traceTc "check_dc_roles" (ppr datacon <+> ppr (tyConRoles tc))
           ; mapM_ (check_ty_roles role_env Representational) $
                    eqSpecPreds eq_spec ++ theta ++ arg_tys }
                    -- See Note [Role-checking data constructor arguments] in TcTyDecls
      where
        (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty) = dataConFullSig datacon
        univ_roles = zipVarEnv univ_tvs (tyConRoles tc)
              -- zipVarEnv uses zipEqual, but we don't want that for ex_tvs
        ex_roles   = mkVarEnv (zip ex_tvs (repeat Nominal))
        role_env   = univ_roles `plusVarEnv` ex_roles

    check_ty_roles env role (TyVarTy tv)
      = case lookupVarEnv env tv of
          Just role' -> unless (role' `ltRole` role || role' == role) $
                        report_error $ ptext (sLit "type variable") <+> quotes (ppr tv) <+>
                                       ptext (sLit "cannot have role") <+> ppr role <+>
                                       ptext (sLit "because it was assigned role") <+> ppr role'
          Nothing    -> report_error $ ptext (sLit "type variable") <+> quotes (ppr tv) <+>
                                       ptext (sLit "missing in environment")

    check_ty_roles env Representational (TyConApp tc tys)
      = let roles' = tyConRoles tc in
        zipWithM_ (maybe_check_ty_roles env) roles' tys

    check_ty_roles env Nominal (TyConApp _ tys)
      = mapM_ (check_ty_roles env Nominal) tys

    check_ty_roles _   Phantom ty@(TyConApp {})
      = pprPanic "check_ty_roles" (ppr ty)

    check_ty_roles env role (AppTy ty1 ty2)
      =  check_ty_roles env role    ty1
      >> check_ty_roles env Nominal ty2

    check_ty_roles env role (FunTy ty1 ty2)
      =  check_ty_roles env role ty1
      >> check_ty_roles env role ty2

    check_ty_roles env role (ForAllTy tv ty)
      = check_ty_roles (extendVarEnv env tv Nominal) role ty

    check_ty_roles _   _    (LitTy {}) = return ()

    maybe_check_ty_roles env role ty
      = when (role == Nominal || role == Representational) $
        check_ty_roles env role ty

    report_error doc
      = addErrTc $ vcat [ptext (sLit "Internal error in role inference:"),
                         doc,
                         ptext (sLit "Please report this as a GHC bug: http://www.haskell.org/ghc/reportabug")]

{-
************************************************************************
*                                                                      *
                Error messages
*                                                                      *
************************************************************************
-}

tcAddTyFamInstCtxt :: TyFamInstDecl Name -> TcM a -> TcM a
tcAddTyFamInstCtxt decl
  = tcAddFamInstCtxt (ptext (sLit "type instance")) (tyFamInstDeclName decl)

tcMkDataFamInstCtxt :: DataFamInstDecl Name -> SDoc
tcMkDataFamInstCtxt decl
  = tcMkFamInstCtxt (pprDataFamInstFlavour decl <+> text "instance")
                    (unLoc (dfid_tycon decl))

tcAddDataFamInstCtxt :: DataFamInstDecl Name -> TcM a -> TcM a
tcAddDataFamInstCtxt decl
  = addErrCtxt (tcMkDataFamInstCtxt decl)

tcMkFamInstCtxt :: SDoc -> Name -> SDoc
tcMkFamInstCtxt flavour tycon
  = hsep [ text "In the" <+> flavour <+> text "declaration for"
         , quotes (ppr tycon) ]

tcAddFamInstCtxt :: SDoc -> Name -> TcM a -> TcM a
tcAddFamInstCtxt flavour tycon thing_inside
  = addErrCtxt (tcMkFamInstCtxt flavour tycon) thing_inside

tcAddClosedTypeFamilyDeclCtxt :: TyCon -> TcM a -> TcM a
tcAddClosedTypeFamilyDeclCtxt tc
  = addErrCtxt ctxt
  where
    ctxt = ptext (sLit "In the equations for closed type family") <+>
           quotes (ppr tc)

resultTypeMisMatch :: FieldLabelString -> DataCon -> DataCon -> SDoc
resultTypeMisMatch field_name con1 con2
  = vcat [sep [ptext (sLit "Constructors") <+> ppr con1 <+> ptext (sLit "and") <+> ppr con2,
                ptext (sLit "have a common field") <+> quotes (ppr field_name) <> comma],
          nest 2 $ ptext (sLit "but have different result types")]

fieldTypeMisMatch :: FieldLabelString -> DataCon -> DataCon -> SDoc
fieldTypeMisMatch field_name con1 con2
  = sep [ptext (sLit "Constructors") <+> ppr con1 <+> ptext (sLit "and") <+> ppr con2,
         ptext (sLit "give different types for field"), quotes (ppr field_name)]

dataConCtxtName :: [Located Name] -> SDoc
dataConCtxtName [con]
   = ptext (sLit "In the definition of data constructor") <+> quotes (ppr con)
dataConCtxtName con
   = ptext (sLit "In the definition of data constructors") <+> interpp'SP con

dataConCtxt :: Outputable a => a -> SDoc
dataConCtxt con = ptext (sLit "In the definition of data constructor") <+> quotes (ppr con)

classOpCtxt :: Var -> Type -> SDoc
classOpCtxt sel_id tau = sep [ptext (sLit "When checking the class method:"),
                              nest 2 (pprPrefixOcc sel_id <+> dcolon <+> ppr tau)]

classArityErr :: Int -> Class -> SDoc
classArityErr n cls
    | n == 0 = mkErr "No" "no-parameter"
    | otherwise = mkErr "Too many" "multi-parameter"
  where
    mkErr howMany allowWhat =
        vcat [ptext (sLit $ howMany ++ " parameters for class") <+> quotes (ppr cls),
              parens (ptext (sLit $ "Use MultiParamTypeClasses to allow "
                                    ++ allowWhat ++ " classes"))]

classFunDepsErr :: Class -> SDoc
classFunDepsErr cls
  = vcat [ptext (sLit "Fundeps in class") <+> quotes (ppr cls),
          parens (ptext (sLit "Use FunctionalDependencies to allow fundeps"))]

badMethPred :: Id -> TcPredType -> SDoc
badMethPred sel_id pred
  = vcat [ hang (ptext (sLit "Constraint") <+> quotes (ppr pred)
                 <+> ptext (sLit "in the type of") <+> quotes (ppr sel_id))
              2 (ptext (sLit "constrains only the class type variables"))
         , ptext (sLit "Use ConstrainedClassMethods to allow it") ]

noClassTyVarErr :: Class -> TyCon -> SDoc
noClassTyVarErr clas fam_tc
  = sep [ ptext (sLit "The associated type") <+> quotes (ppr fam_tc)
        , ptext (sLit "mentions none of the type or kind variables of the class") <+>
                quotes (ppr clas <+> hsep (map ppr (classTyVars clas)))]

recSynErr :: [LTyClDecl Name] -> TcRn ()
recSynErr syn_decls
  = setSrcSpan (getLoc (head sorted_decls)) $
    addErr (sep [ptext (sLit "Cycle in type synonym declarations:"),
                 nest 2 (vcat (map ppr_decl sorted_decls))])
  where
    sorted_decls = sortLocated syn_decls
    ppr_decl (L loc decl) = ppr loc <> colon <+> ppr decl

recClsErr :: [TyCon] -> TcRn ()
recClsErr cycles
  = addErr (sep [ptext (sLit "Cycle in class declaration (via superclasses):"),
                 nest 2 (hsep (intersperse (text "->") (map ppr cycles)))])

badDataConTyCon :: DataCon -> Type -> Type -> SDoc
badDataConTyCon data_con res_ty_tmpl actual_res_ty
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr data_con) <+>
                ptext (sLit "returns type") <+> quotes (ppr actual_res_ty))
       2 (ptext (sLit "instead of an instance of its parent type") <+> quotes (ppr res_ty_tmpl))

badGadtKindCon :: DataCon -> SDoc
badGadtKindCon data_con
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr data_con)
          <+> ptext (sLit "cannot be GADT-like in its *kind* arguments"))
       2 (ppr data_con <+> dcolon <+> ppr (dataConUserType data_con))

badGadtDecl :: Name -> SDoc
badGadtDecl tc_name
  = vcat [ ptext (sLit "Illegal generalised algebraic data declaration for") <+> quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use GADTs to allow GADTs")) ]

badExistential :: DataCon -> SDoc
badExistential con
  = hang (ptext (sLit "Data constructor") <+> quotes (ppr con) <+>
                ptext (sLit "has existential type variables, a context, or a specialised result type"))
       2 (vcat [ ppr con <+> dcolon <+> ppr (dataConUserType con)
               , parens $ ptext (sLit "Use ExistentialQuantification or GADTs to allow this") ])

badStupidTheta :: Name -> SDoc
badStupidTheta tc_name
  = ptext (sLit "A data type declared in GADT style cannot have a context:") <+> quotes (ppr tc_name)

newtypeConError :: Name -> Int -> SDoc
newtypeConError tycon n
  = sep [ptext (sLit "A newtype must have exactly one constructor,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr tycon) <+> ptext (sLit "has") <+> speakN n ]

newtypeStrictError :: DataCon -> SDoc
newtypeStrictError con
  = sep [ptext (sLit "A newtype constructor cannot have a strictness annotation,"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con) <+> ptext (sLit "does")]

newtypeFieldErr :: DataCon -> Int -> SDoc
newtypeFieldErr con_name n_flds
  = sep [ptext (sLit "The constructor of a newtype must have exactly one field"),
         nest 2 $ ptext (sLit "but") <+> quotes (ppr con_name) <+> ptext (sLit "has") <+> speakN n_flds]

badSigTyDecl :: Name -> SDoc
badSigTyDecl tc_name
  = vcat [ ptext (sLit "Illegal kind signature") <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use KindSignatures to allow kind signatures")) ]

emptyConDeclsErr :: Name -> SDoc
emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext (sLit "has no constructors"),
         nest 2 $ ptext (sLit "(EmptyDataDecls permits this)")]

wrongKindOfFamily :: TyCon -> SDoc
wrongKindOfFamily family
  = ptext (sLit "Wrong category of family instance; declaration was for a")
    <+> kindOfFamily
  where
    kindOfFamily | isTypeFamilyTyCon family = text "type family"
                 | isDataFamilyTyCon family = text "data family"
                 | otherwise = pprPanic "wrongKindOfFamily" (ppr family)

wrongNumberOfParmsErr :: Arity -> SDoc
wrongNumberOfParmsErr max_args
  = ptext (sLit "Number of parameters must match family declaration; expected")
    <+> ppr max_args

wrongTyFamName :: Name -> Name -> SDoc
wrongTyFamName fam_tc_name eqn_tc_name
  = hang (ptext (sLit "Mismatched type name in type family instance."))
       2 (vcat [ ptext (sLit "Expected:") <+> ppr fam_tc_name
               , ptext (sLit "  Actual:") <+> ppr eqn_tc_name ])

badRoleAnnot :: Name -> Role -> Role -> SDoc
badRoleAnnot var annot inferred
  = hang (ptext (sLit "Role mismatch on variable") <+> ppr var <> colon)
       2 (sep [ ptext (sLit "Annotation says"), ppr annot
              , ptext (sLit "but role"), ppr inferred
              , ptext (sLit "is required") ])

wrongNumberOfRoles :: [a] -> LRoleAnnotDecl Name -> SDoc
wrongNumberOfRoles tyvars d@(L _ (RoleAnnotDecl _ annots))
  = hang (ptext (sLit "Wrong number of roles listed in role annotation;") $$
          ptext (sLit "Expected") <+> (ppr $ length tyvars) <> comma <+>
          ptext (sLit "got") <+> (ppr $ length annots) <> colon)
       2 (ppr d)

illegalRoleAnnotDecl :: LRoleAnnotDecl Name -> TcM ()
illegalRoleAnnotDecl (L loc (RoleAnnotDecl tycon _))
  = setErrCtxt [] $
    setSrcSpan loc $
    addErrTc (ptext (sLit "Illegal role annotation for") <+> ppr tycon <> char ';' $$
              ptext (sLit "they are allowed only for datatypes and classes."))

needXRoleAnnotations :: TyCon -> SDoc
needXRoleAnnotations tc
  = ptext (sLit "Illegal role annotation for") <+> ppr tc <> char ';' $$
    ptext (sLit "did you intend to use RoleAnnotations?")

incoherentRoles :: SDoc
incoherentRoles = (text "Roles other than" <+> quotes (text "nominal") <+>
                   text "for class parameters can lead to incoherence.") $$
                  (text "Use IncoherentInstances to allow this; bad role found")

addTyThingCtxt :: TyThing -> TcM a -> TcM a
addTyThingCtxt thing
  = addErrCtxt ctxt
  where
    name = getName thing
    flav = case thing of
             ATyCon tc -> text (tyConFlavour tc)
             _ -> pprTrace "addTyThingCtxt strange" (ppr thing)
                  Outputable.empty

    ctxt = hsep [ ptext (sLit "In the"), flav
                , ptext (sLit "declaration for"), quotes (ppr name) ]

addRoleAnnotCtxt :: Name -> TcM a -> TcM a
addRoleAnnotCtxt name
  = addErrCtxt $
    text "while checking a role annotation for" <+> quotes (ppr name)
