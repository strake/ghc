-- (c) The University of Glasgow 2006
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance MonadThings is necessarily an
                                       -- orphan
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE TypeFamilies #-}

module GHC.Tc.Utils.Env(
        TyThing(..), TcTyThing(..), TcId,

        -- Instance environment, and InstInfo type
        InstInfo(..), iDFunId, pprInstInfoDetails,
        simpleInstInfoClsTy, simpleInstInfoTy, simpleInstInfoTyCon,
        InstBindings(..),

        -- Global environment
        tcExtendGlobalEnv, tcExtendTyConEnv,
        tcExtendGlobalEnvImplicit, setGlobalTypeEnv,
        tcExtendGlobalValEnv,
        tcLookupGlobal, tcLookupGlobalOnly,
        tcLookupTyCon, tcLookupClass,
        tcLookupDataCon, tcLookupPatSyn, tcLookupConLike, tcLookupAxiom,
        lookupGlobal, ioLookupDataCon,
        addTypecheckedBinds,

        -- Local environment
        tcExtendKindEnv, tcExtendKindEnvList,
        tcExtendTyVarEnv, tcExtendNameTyVarEnv,
        tcExtendLetEnv, tcExtendSigIds, tcExtendRecIds,
        tcExtendIdEnv, tcExtendIdEnv1, tcExtendIdEnv2,
        tcExtendBinderStack,
        isTypeClosedLetBndr,

        tcLookup, tcLookupLocated, tcLookupLocalIds,
        tcLookupId, tcLookupIdMaybe, tcLookupTyVar,
        tcLookupTcTyCon,
        tcLookupLcl_maybe,
        getInLocalScope,
        wrongThingErr, pprBinders,

        tcAddDataFamConPlaceholders, tcAddPatSynPlaceholders,
        getTypeSigNames,
        tcExtendRecEnv,         -- For knot-tying

        -- Tidying
        tcInitTidyEnv, tcInitOpenTidyEnv,

        -- Instances
        tcLookupInstance, tcGetInstEnvs,

        -- Rules
        tcExtendRules,

        -- Defaults
        tcGetDefaultTys,

        -- Template Haskell stuff
        checkWellStaged, tcMetaTy, thLevel,
        topIdLvl, isBrackStage,

        -- New Ids
        newDFunName, newFamInstTyConName,
        newFamInstAxiomName,
        mkStableIdFromString, mkStableIdFromName,
        mkWrapperName
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session

import GHC.Builtin.Names
import GHC.Builtin.Types

import GHC.Runtime.Context

import GHC.Hs

import GHC.Iface.Env
import GHC.Iface.Load

import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType

import GHC.Core.InstEnv
import GHC.Core.DataCon ( DataCon )
import GHC.Core.PatSyn  ( PatSyn )
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion.Axiom
import GHC.Core.Class

import GHC.Unit.Module
import GHC.Unit.External

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Encoding
import GHC.Utils.Error
import GHC.Utils.Lens.Monad
import GHC.Utils.Misc ( HasDebugCallStack )

import GHC.Data.FastString
import GHC.Data.List.SetOps
import GHC.Data.Maybe( MaybeErr(..), orElse )

import GHC.Types.SrcLoc
import GHC.Types.Basic hiding( SuccessFlag(..) )
import GHC.Types.TypeEnv
import GHC.Types.SourceFile
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Id
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Name.Reader
import GHC.Types.TyThing
import qualified GHC.LanguageExtensions as LangExt

import Data.IORef
import Data.List (intercalate)
import Control.Monad
import Data.Functor.Reader.Class

{- *********************************************************************
*                                                                      *
            An IO interface to looking up globals
*                                                                      *
********************************************************************* -}

lookupGlobal :: HscEnv -> Name -> IO TyThing
-- A variant of lookupGlobal_maybe for the clients which are not
-- interested in recovering from lookup failure and accept panic.
lookupGlobal hsc_env name = lookupGlobal_maybe hsc_env name >>= \ case
            Succeeded thing -> return thing
            Failed msg      -> pprPanic "lookupGlobal" msg

lookupGlobal_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc TyThing)
-- This may look up an Id that one has previously looked up.
-- If so, we are going to read its interface file, and add its bindings
-- to the ExternalPackageTable.
lookupGlobal_maybe hsc_env name
  = do  {    -- Try local envt
          let mod = icInteractiveModule (hsc_IC hsc_env)
              dflags = hsc_dflags hsc_env
              tcg_semantic_mod = canonicalizeModuleIfHome dflags mod

        ; if nameIsLocalOrFrom tcg_semantic_mod name
              then (return
                (Failed (text "Can't find local name: " <+> ppr name)))
                  -- Internal names can happen in GHCi
              else
           -- Try home package table and external package table
          lookupImported_maybe hsc_env name
        }

lookupImported_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc TyThing)
-- Returns (Failed err) if we can't find the interface file for the thing
lookupImported_maybe hsc_env name = lookupType hsc_env name >>= \ case
            Just thing -> return (Succeeded thing)
            Nothing    -> importDecl_maybe hsc_env name

importDecl_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc TyThing)
importDecl_maybe hsc_env name
  | Just thing <- wiredInNameTyThing_maybe name
  = do  { when (needWiredInHomeIface thing)
               (initIfaceLoad hsc_env (loadWiredInHomeIface name))
                -- See Note [Loading instances for wired-in things]
        ; return (Succeeded thing) }
  | otherwise
  = initIfaceLoad hsc_env (importDecl name)

ioLookupDataCon :: HscEnv -> Name -> IO DataCon
ioLookupDataCon hsc_env name = ioLookupDataCon_maybe hsc_env name >>= \ case
    Succeeded thing -> return thing
    Failed msg      -> pprPanic "lookupDataConIO" msg

ioLookupDataCon_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc DataCon)
ioLookupDataCon_maybe hsc_env name = lookupGlobal hsc_env name <???> \ thing -> case thing of
        AConLike (RealDataCon con) -> Succeeded con
        _                          -> Failed $
          pprTcTyThingCategory (AGlobal thing) <+> quotes (ppr name) <+>
                text "used as a data constructor"

addTypecheckedBinds :: TcGblEnv -> [LHsBinds GhcTc] -> TcGblEnv
addTypecheckedBinds tcg_env binds
  | isHsBootOrSig (tcg_src tcg_env) = tcg_env
    -- Do not add the code for record-selector bindings
    -- when compiling hs-boot files
  | otherwise = over tcg_bindsL (foldr (<|>) `flip` binds) tcg_env

{-
************************************************************************
*                                                                      *
*                      tcLookupGlobal                                  *
*                                                                      *
************************************************************************

Using the Located versions (eg. tcLookupLocatedGlobal) is preferred,
unless you know that the SrcSpan in the monad is already set to the
span of the Name.
-}


tcLookupGlobal :: Name -> TcM TyThing
-- The Name is almost always an ExternalName, but not always
-- In GHCi, we may make command-line bindings (ghci> let x = True)
-- that bind a GlobalId, but with an InternalName
tcLookupGlobal name
  = do  {    -- Try local envt
          env <- getGblEnv
        ; case lookupNameEnv (tcg_type_env env) name of {
                Just thing -> return thing ;
                Nothing    ->

                -- Should it have been in the local envt?
                -- (NB: use semantic mod here, since names never use
                -- identity module, see Note [Identity versus semantic module].)
          if nameIsLocalOrFrom (tcg_semantic_mod env) name
          then notFound name  -- Internal names can happen in GHCi
          else

           -- Try home package table and external package table
    tcLookupImported_maybe name >>= \ case
            Succeeded thing -> return thing
            Failed msg      -> failWithTc msg
        }}

-- Look up only in this module's global env't. Don't look in imports, etc.
-- Panic if it's not there.
tcLookupGlobalOnly :: Name -> TcM TyThing
tcLookupGlobalOnly name = getGblEnv <???> \ env -> case lookupNameEnv (tcg_type_env env) name of
                    Just thing -> thing
                    Nothing    -> pprPanic "tcLookupGlobalOnly" (ppr name)

tcLookupDataCon :: Name -> TcM DataCon
tcLookupDataCon name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike (RealDataCon con) -> return con
        _                          -> wrongThingErr "data constructor" (AGlobal thing) name

tcLookupPatSyn :: Name -> TcM PatSyn
tcLookupPatSyn name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike (PatSynCon ps) -> return ps
        _                       -> wrongThingErr "pattern synonym" (AGlobal thing) name

tcLookupConLike :: Name -> TcM ConLike
tcLookupConLike name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike cl -> return cl
        _           -> wrongThingErr "constructor-like thing" (AGlobal thing) name

tcLookupClass :: Name -> TcM Class
tcLookupClass name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc | Just cls <- tyConClass_maybe tc -> return cls
        _                                           -> wrongThingErr "class" (AGlobal thing) name

tcLookupTyCon :: Name -> TcM TyCon
tcLookupTyCon name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc -> return tc
        _         -> wrongThingErr "type constructor" (AGlobal thing) name

tcLookupAxiom :: Name -> TcM (CoAxiom Branched)
tcLookupAxiom name = do
    thing <- tcLookupGlobal name
    case thing of
        ACoAxiom ax -> return ax
        _           -> wrongThingErr "axiom" (AGlobal thing) name

-- Find the instance that exactly matches a type class application.  The class arguments must be precisely
-- the same as in the instance declaration (modulo renaming & casts).
--
tcLookupInstance :: Class -> [Type] -> TcM ClsInst
tcLookupInstance cls tys
  = do { instEnv <- tcGetInstEnvs
       ; case lookupUniqueInstEnv instEnv cls tys of
           Left err             -> failWithTc $ text "Couldn't match instance:" <+> err
           Right (inst, tys)
             | uniqueTyVars tys -> return inst
             | otherwise        -> failWithTc errNotExact
       }
  where
    errNotExact = text "Not an exact match (i.e., some variables get instantiated)"

    uniqueTyVars tys = all isTyVarTy tys
                    && hasNoDups (map (getTyVar "tcLookupInstance") tys)

tcGetInstEnvs :: TcM InstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetInstEnvs =
  [ InstEnvs
      { ie_global  = eps_inst_env eps
      , ie_local   = tcg_inst_env env
      , ie_visible = tcVisibleOrphanMods env }
  | eps <- getEps
  , env <- getGblEnv ]

instance MonadThings (IOEnv (Env TcGblEnv TcLclEnv)) where
    lookupThing = tcLookupGlobal

{-
************************************************************************
*                                                                      *
                Extending the global environment
*                                                                      *
************************************************************************
-}

setGlobalTypeEnv :: TcGblEnv -> TypeEnv -> TcM TcGblEnv
-- Use this to update the global type env
-- It updates both  * the normal tcg_type_env field
--                  * the tcg_type_env_var field seen by interface files
setGlobalTypeEnv = flip $ modifyGlobalTypeEnv . pure

modifyGlobalTypeEnv :: (TypeEnv -> TypeEnv) -> TcGblEnv -> TcM TcGblEnv
-- Use this to update the global type env
-- It updates both  * the normal tcg_type_env field
--                  * the tcg_type_env_var field seen by interface files
modifyGlobalTypeEnv f tcg_env
  = -- Sync the type-envt variable seen by interface files
    tcg_env { tcg_type_env = new_type_env } <$ writeMutVar (tcg_type_env_var tcg_env) new_type_env
  where
    new_type_env = f (tcg_type_env tcg_env)

tcExtendGlobalEnvImplicit :: [TyThing] -> TcM r -> TcM r
  -- Just extend the global environment with some TyThings
  -- Do not extend tcg_tcs, tcg_patsyns etc
tcExtendGlobalEnvImplicit things thing_inside
  = do { tcg_env' <- modifyGlobalTypeEnv (`extendTypeEnvList` things) =<< getGblEnv
       ; setGblEnv tcg_env' thing_inside }

tcExtendGlobalEnv :: [TyThing] -> TcM r -> TcM r
  -- Given a mixture of Ids, TyCons, Classes, all defined in the
  -- module being compiled, extend the global environment
tcExtendGlobalEnv things = tcExtendGlobalEnvImplicit things & locally env_gblL \ env -> env
  { tcg_tcs = [tc | ATyCon tc <- things] ++ tcg_tcs env
  , tcg_patsyns = [ps | AConLike (PatSynCon ps) <- things] ++ tcg_patsyns env }

tcExtendTyConEnv :: [TyCon] -> TcM r -> TcM r
  -- Given a mixture of Ids, TyCons, Classes, all defined in the
  -- module being compiled, extend the global environment
tcExtendTyConEnv tycons =
  locally (env_gblL . tcg_tcsL) (tycons ++) . tcExtendGlobalEnvImplicit (ATyCon <$> tycons)

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
  -- Same deal as tcExtendGlobalEnv, but for Ids
tcExtendGlobalValEnv ids = tcExtendGlobalEnvImplicit (AnId <$> ids)

tcExtendRecEnv :: [(Name,TyThing)] -> TcM r -> TcM r
-- Extend the global environments for the type/class knot tying game
-- Just like tcExtendGlobalEnv, except the argument is a list of pairs
tcExtendRecEnv = locally (env_gblL . tcg_type_envL) . flip extendNameEnvList
         -- No need for setGlobalTypeEnv (which side-effects the
         -- tcg_type_env_var); tcExtendRecEnv is used just
         -- when kind-check a group of type/class decls. It would
         -- in any case be wrong for an interface-file decl to end up
         -- with a TcTyCon in it!

{-
************************************************************************
*                                                                      *
\subsection{The local environment}
*                                                                      *
************************************************************************
-}

tcLookupLocated :: Located Name -> TcM TcTyThing
tcLookupLocated = addLocM tcLookup

tcLookupLcl_maybe :: Name -> TcM (Maybe TcTyThing)
tcLookupLcl_maybe name = getLclTypeEnv <???> \ local_env -> lookupNameEnv local_env name

tcLookup :: Name -> TcM TcTyThing
tcLookup name = do
    local_env <- getLclTypeEnv
    case lookupNameEnv local_env name of
        Just thing -> return thing
        Nothing    -> AGlobal <$> tcLookupGlobal name

tcLookupTyVar :: Name -> TcM TcTyVar
tcLookupTyVar name = tcLookup name >>= \ case
           ATyVar _ tv -> return tv
           _           -> pprPanic "tcLookupTyVar" (ppr name)

tcLookupId :: Name -> TcM Id
-- Used when we aren't interested in the binding level, nor refinement.
-- The "no refinement" part means that we return the un-refined Id regardless
--
-- The Id is never a DataCon. (Why does that matter? see GHC.Tc.Gen.Expr.tcId)
tcLookupId name = tcLookupIdMaybe name >>= \ case
        Just id -> return id
        _       -> pprPanic "tcLookupId" (ppr name)

tcLookupIdMaybe :: Name -> TcM (Maybe Id)
tcLookupIdMaybe = tcLookup & fmap \ case
           ATcId { tct_id = id} -> Just id
           AGlobal (AnId id)    -> Just id
           _                    -> Nothing

tcLookupLocalIds :: Functor f => f Name -> TcM (f TcId)
-- We expect the variables to all be bound, and all at
-- the same level as the lookup.  Only used in one place...
tcLookupLocalIds ns = getLclEnv <???> \ env -> lookup (tcl_env env) <$> ns
  where
    lookup lenv name
        = case lookupNameEnv lenv name of
                Just (ATcId { tct_id = id }) ->  id
                _ -> pprPanic "tcLookupLocalIds" (ppr name)

-- inferInitialKind has made a suitably-shaped kind for the type or class
-- Look it up in the local environment. This is used only for tycons
-- that we're currently type-checking, so we're sure to find a TcTyCon.
tcLookupTcTyCon :: HasDebugCallStack => Name -> TcM TcTyCon
tcLookupTcTyCon name = tcLookup name >>= \ case
        ATcTyCon tc -> return tc
        _           -> pprPanic "tcLookupTcTyCon" (ppr name)

getInLocalScope :: TcM (Name -> Bool)
getInLocalScope = flip elemNameEnv <$> getLclTypeEnv

tcExtendKindEnvList :: [(Name, TcTyThing)] -> TcM r -> TcM r
-- Used only during kind checking, for TcThings that are
--      ATcTyCon or APromotionErr
-- No need to update the global tyvars, or tcl_th_bndrs, or tcl_rdr
tcExtendKindEnvList things thing_inside
  = do { traceTc "tcExtendKindEnvList" (ppr things)
       ; locally (env_lclL . tcl_envL) (`extendNameEnvList` things) thing_inside }

tcExtendKindEnv :: NameEnv TcTyThing -> TcM r -> TcM r
-- A variant of tcExtendKindEvnList
tcExtendKindEnv extra_env thing_inside
  = do { traceTc "tcExtendKindEnv" (ppr extra_env)
       ; locally (env_lclL . tcl_envL) (`plusNameEnv` extra_env) thing_inside }

-----------------------
-- Scoped type and kind variables
tcExtendTyVarEnv :: [TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tvs = tcExtendNameTyVarEnv (mkTyVarNamePairs tvs)

tcExtendNameTyVarEnv :: [(Name,TcTyVar)] -> TcM r -> TcM r
tcExtendNameTyVarEnv binds
  -- this should be used only for explicitly mentioned scoped variables.
  -- thus, no coercion variables
  = tc_extend_local_env NotTopLevel [(name, ATyVar name tv) | (name, tv) <- binds] .
    tcExtendBinderStack tv_binds
  where
    tv_binds :: [TcBinder]
    tv_binds = [TcTvBndr name tv | (name,tv) <- binds]

isTypeClosedLetBndr :: Id -> Bool
-- See Note [Bindings with closed types] in GHC.Tc.Types
isTypeClosedLetBndr = noFreeVarsOfType . idType

tcExtendRecIds :: [(Name, TcId)] -> TcM a -> TcM a
-- Used for binding the recursive uses of Ids in a binding
-- both top-level value bindings and nested let/where-bindings
-- Does not extend the TcBinderStack
tcExtendRecIds pairs
  = tc_extend_local_env NotTopLevel
          [ (name, ATcId { tct_id   = let_id
                         , tct_info = NonClosedLet emptyNameSet False })
          | (name, let_id) <- pairs ]

tcExtendSigIds :: TopLevelFlag -> [TcId] -> TcM a -> TcM a
-- Used for binding the Ids that have a complete user type signature
-- Does not extend the TcBinderStack
tcExtendSigIds top_lvl sig_ids
  = tc_extend_local_env top_lvl
          [ (idName id, ATcId { tct_id   = id
                              , tct_info = info })
          | id <- sig_ids
          , let closed = isTypeClosedLetBndr id
                info   = NonClosedLet emptyNameSet closed ]


tcExtendLetEnv :: TopLevelFlag -> TcSigFun -> IsGroupClosed
                  -> [TcId] -> TcM a -> TcM a
-- Used for both top-level value bindings and nested let/where-bindings
-- Adds to the TcBinderStack too
tcExtendLetEnv top_lvl sig_fn (IsGroupClosed fvs fv_type_closed)
               ids thing_inside
  = tcExtendBinderStack [TcIdBndr id top_lvl | id <- ids] $
    tc_extend_local_env top_lvl
          [ (idName id, ATcId { tct_id   = id
                              , tct_info = mk_tct_info id })
          | id <- ids ]
    thing_inside
  where
    mk_tct_info id
      | type_closed && isEmptyNameSet rhs_fvs = ClosedLet
      | otherwise                             = NonClosedLet rhs_fvs type_closed
      where
        name        = idName id
        rhs_fvs     = lookupNameEnv fvs name `orElse` emptyNameSet
        type_closed = isTypeClosedLetBndr id &&
                      (fv_type_closed || hasCompleteSig sig_fn name)

tcExtendIdEnv :: [TcId] -> TcM a -> TcM a
-- For lambda-bound and case-bound Ids
-- Extends the TcBinderStack as well
tcExtendIdEnv ids = tcExtendIdEnv2 [(idName id, id) | id <- ids]

tcExtendIdEnv1 :: Name -> TcId -> TcM a -> TcM a
-- Exactly like tcExtendIdEnv2, but for a single (name,id) pair
tcExtendIdEnv1 name id = tcExtendIdEnv2 [(name,id)]

tcExtendIdEnv2 :: [(Name,TcId)] -> TcM a -> TcM a
tcExtendIdEnv2 names_w_ids
  = tcExtendBinderStack [ TcIdBndr mono_id NotTopLevel
                        | (_,mono_id) <- names_w_ids ] .
    tc_extend_local_env NotTopLevel
            [ (name, ATcId { tct_id = id
                           , tct_info    = NotLetBound })
            | (name,id) <- names_w_ids]

tc_extend_local_env :: TopLevelFlag -> [(Name, TcTyThing)] -> TcM a -> TcM a
tc_extend_local_env top_lvl extra_env thing_inside
-- Precondition: the argument list extra_env has TcTyThings
--               that ATcId or ATyVar, but nothing else
--
-- Invariant: the ATcIds are fully zonked. Reasons:
--      (a) The kinds of the forall'd type variables are defaulted
--          (see Kind.defaultKind, done in skolemiseQuantifiedTyVar)
--      (b) There are no via-Indirect occurrences of the bound variables
--          in the types, because instantiation does not look through such things
--      (c) The call to tyCoVarsOfTypes is ok without looking through refs

-- The second argument of type TyVarSet is a set of type variables
-- that are bound together with extra_env and should not be regarded
-- as free in the types of extra_env.
  = do  { traceTc "tc_extend_local_env" (ppr extra_env)
        ; locally env_lclL (\ env0 ->
          let env1 = over tcl_envL (flip extendNameEnvList extra_env) env0
              stage = tcl_th_ctxt env0
              env2 = extend_local_env (top_lvl, thLevel stage) extra_env env1
          in env2) thing_inside }
  where
    extend_local_env :: (TopLevelFlag, ThLevel) -> [(Name, TcTyThing)] -> TcLclEnv -> TcLclEnv
    -- Extend the local LocalRdrEnv and Template Haskell staging env simultaneously
    -- Reason for extending LocalRdrEnv: after running a TH splice we need
    -- to do renaming.
    extend_local_env thlvl pairs env@(TcLclEnv { tcl_rdr = rdr_env
                                               , tcl_th_bndrs = th_bndrs })
      = env { tcl_rdr      = extendLocalRdrEnvList rdr_env
                                [ n | (n, _) <- pairs, isInternalName n ]
                                -- The LocalRdrEnv contains only non-top-level names
                                -- (GlobalRdrEnv handles the top level)
            , tcl_th_bndrs = extendNameEnvList th_bndrs  -- We only track Ids in tcl_th_bndrs
                                 [(n, thlvl) | (n, ATcId {}) <- pairs] }

{- *********************************************************************
*                                                                      *
             The TcBinderStack
*                                                                      *
********************************************************************* -}

tcExtendBinderStack :: [TcBinder] -> TcM a -> TcM a
tcExtendBinderStack bndrs thing_inside
  = do { traceTc "tcExtendBinderStack" (ppr bndrs)
       ; locally (env_lclL . tcl_bndrsL) (bndrs ++) thing_inside }

tcInitTidyEnv :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => m TidyEnv
-- We initialise the "tidy-env", used for tidying types before printing,
-- by building a reverse map from the in-scope type variables to the
-- OccName that the programmer originally used for them
tcInitTidyEnv
  = do  { lcl_env <- env_lcl <$> ask
        ; go emptyTidyEnv (tcl_bndrs lcl_env) }
  where
    go (env, subst) []
      = return (env, subst)
    go (env, subst) (b : bs)
      | TcTvBndr name tyvar <- b
       = do { let (env', occ') = tidyOccName env (nameOccName name)
                  name'  = tidyNameOcc name occ'
                  tyvar1 = set tyVarNameL name' tyvar
            ; tyvar2 <- zonkTcTyVarToTyVar tyvar1
              -- Be sure to zonk here!  Tidying applies to zonked
              -- types, so if we don't zonk we may create an
              -- ill-kinded type (#14175)
            ; go (env', extendVarEnv subst tyvar tyvar2) bs }
      | otherwise
      = go (env, subst) bs

-- | Get a 'TidyEnv' that includes mappings for all vars free in the given
-- type. Useful when tidying open types.
tcInitOpenTidyEnv :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => [TyCoVar] -> m TidyEnv
tcInitOpenTidyEnv tvs = flip tidyFreeTyCoVars tvs <$> tcInitTidyEnv



{- *********************************************************************
*                                                                      *
             Adding placeholders
*                                                                      *
********************************************************************* -}

tcAddDataFamConPlaceholders :: [LInstDecl GhcRn] -> TcM a -> TcM a
-- See Note [AFamDataCon: not promoting data family constructors]
tcAddDataFamConPlaceholders inst_decls thing_inside
  = tcExtendKindEnvList [ (con, APromotionErr FamDataConPE)
                        | lid <- inst_decls, con <- get_cons lid ]
      thing_inside
      -- Note [AFamDataCon: not promoting data family constructors]
  where
    -- get_cons extracts the *constructor* bindings of the declaration
    get_cons :: LInstDecl GhcRn -> [Name]
    get_cons (L _ (TyFamInstD {}))                     = []
    get_cons (L _ (DataFamInstD { dfid_inst = fid }))  = get_fi_cons fid
    get_cons (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fids } }))
      = concatMap (get_fi_cons . unLoc) fids

    get_fi_cons :: DataFamInstDecl GhcRn -> [Name]
    get_fi_cons (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                  FamEqn { feqn_rhs = HsDataDefn { dd_cons = cons } }}})
      = map unLoc $ concatMap (getConNames . unLoc) cons


tcAddPatSynPlaceholders :: [PatSynBind GhcRn GhcRn] -> TcM a -> TcM a
-- See Note [Don't promote pattern synonyms]
tcAddPatSynPlaceholders pat_syns thing_inside
  = tcExtendKindEnvList [ (name, APromotionErr PatSynPE)
                        | PSB{ psb_id = L _ name } <- pat_syns ]
       thing_inside

getTypeSigNames :: [LSig GhcRn] -> NameSet
-- Get the names that have a user type sig
getTypeSigNames sigs
  = foldr get_type_sig emptyNameSet sigs
  where
    get_type_sig :: LSig GhcRn -> NameSet -> NameSet
    get_type_sig sig ns =
      case sig of
        L _ (TypeSig _ names _) -> extendNameSetList ns (map unLoc names)
        L _ (PatSynSig _ names _) -> extendNameSetList ns (map unLoc names)
        _ -> ns


{- Note [AFamDataCon: not promoting data family constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family T a
  data instance T Int = MkT
  data Proxy (a :: k)
  data S = MkS (Proxy 'MkT)

Is it ok to use the promoted data family instance constructor 'MkT' in
the data declaration for S (where both declarations live in the same module)?
No, we don't allow this. It *might* make sense, but at least it would mean that
we'd have to interleave typechecking instances and data types, whereas at
present we do data types *then* instances.

So to check for this we put in the TcLclEnv a binding for all the family
constructors, bound to AFamDataCon, so that if we trip over 'MkT' when
type checking 'S' we'll produce a decent error message.

#12088 describes this limitation. Of course, when MkT and S live in
different modules then all is well.

Note [Don't promote pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never promote pattern synonyms.

Consider this (#11265):
  pattern A = True
  instance Eq A
We want a civilised error message from the occurrence of 'A'
in the instance, yet 'A' really has not yet been type checked.

Similarly (#9161)
  {-# LANGUAGE PatternSynonyms, DataKinds #-}
  pattern A = ()
  b :: A
  b = undefined
Here, the type signature for b mentions A.  But A is a pattern
synonym, which is typechecked as part of a group of bindings (for very
good reasons; a view pattern in the RHS may mention a value binding).
It is entirely reasonable to reject this, but to do so we need A to be
in the kind environment when kind-checking the signature for B.

Hence tcAddPatSynPlaceholers adds a binding
    A -> APromotionErr PatSynPE
to the environment. Then GHC.Tc.Gen.HsType.tcTyVar will find A in the kind
environment, and will give a 'wrongThingErr' as a result.  But the
lookup of A won't fail.


************************************************************************
*                                                                      *
\subsection{Rules}
*                                                                      *
************************************************************************
-}

tcExtendRules :: (IsLocal f f, EnvType f ~ Env TcGblEnv lcl) => [LRuleDecl GhcTc] -> f a -> f a
        -- Just pop the new rules into the EPS and envt resp
        -- All the rules come from an interface file, not source
        -- Nevertheless, some may be for this module, if we read
        -- its interface instead of its source code
tcExtendRules = locally (env_gblL . tcg_rulesL) . (++)

{-
************************************************************************
*                                                                      *
                Meta level
*                                                                      *
************************************************************************
-}

checkWellStaged :: SDoc         -- What the stage check is for
                -> ThLevel      -- Binding level (increases inside brackets)
                -> ThLevel      -- Use stage
                -> TcM ()       -- Fail if badly staged, adding an error
checkWellStaged pp_thing bind_lvl use_lvl
  | use_lvl >= bind_lvl         -- OK! Used later than bound
  = return ()                   -- E.g.  \x -> [| $(f x) |]

  | bind_lvl == outerLevel      -- GHC restriction on top level splices
  = stageRestrictionError pp_thing

  | otherwise                   -- Badly staged
  = failWithTc $                -- E.g.  \x -> $(f x)
    text "Stage error:" <+> pp_thing <+>
        hsep   [text "is bound at stage" <+> ppr bind_lvl,
                text "but used at stage" <+> ppr use_lvl]

stageRestrictionError :: SDoc -> TcM a
stageRestrictionError pp_thing
  = failWithTc $
    sep [ text "GHC stage restriction:"
        , nest 2 (vcat [ pp_thing <+> text "is used in a top-level splice, quasi-quote, or annotation,"
                       , text "and must be imported, not defined locally"])]

topIdLvl :: Id -> ThLevel
-- Globals may either be imported, or may be from an earlier "chunk"
-- (separated by declaration splices) of this module.  The former
--  *can* be used inside a top-level splice, but the latter cannot.
-- Hence we give the former impLevel, but the latter topLevel
-- E.g. this is bad:
--      x = [| foo |]
--      $( f x )
-- By the time we are processing the $(f x), the binding for "x"
-- will be in the global env, not the local one.
topIdLvl id | isLocalId id = outerLevel
            | otherwise    = impLevel

tcMetaTy :: Name -> TcM Type
-- Given the name of a Template Haskell data type,
-- return the type
-- E.g. given the name "Expr" return the type "Expr"
tcMetaTy tc_name = mkTyConTy <$> tcLookupTyCon tc_name

isBrackStage :: ThStage -> Bool
isBrackStage (Brack {}) = True
isBrackStage _other     = False

{-
************************************************************************
*                                                                      *
                 getDefaultTys
*                                                                      *
************************************************************************
-}

tcGetDefaultTys :: TcM ([Type], -- Default types
                        (Bool,  -- True <=> Use overloaded strings
                         Bool)) -- True <=> Use extended defaulting rules
tcGetDefaultTys
  = do  { dflags <- getDynFlags
        ; let ovl_strings = xopt LangExt.OverloadedStrings dflags
              extended_defaults = xopt LangExt.ExtendedDefaultRules dflags
                                        -- See also #1974
              flags = (ovl_strings, extended_defaults)

        ; mb_defaults <- getDeclaredDefaultTys
        ; case mb_defaults of {
           Just tys -> return (tys, flags) ;
                                -- User-supplied defaults
           Nothing  -> do

        -- No use-supplied default
        -- Use [Integer, Double], plus modifications
        { integer_ty <- tcMetaTy integerTyConName
        ; list_ty <- tcMetaTy listTyConName
        ; checkWiredInTyCon doubleTyCon
        ; let deflt_tys = opt_deflt extended_defaults [unitTy, list_ty]
                          -- Note [Extended defaults]
                          ++ [integer_ty, doubleTy]
                          ++ opt_deflt ovl_strings [stringTy]
        ; return (deflt_tys, flags) } } }
  where
    opt_deflt True  xs = xs
    opt_deflt False _  = []

{-
Note [Extended defaults]
~~~~~~~~~~~~~~~~~~~~~
In interactive mode (or with -XExtendedDefaultRules) we add () as the first type we
try when defaulting.  This has very little real impact, except in the following case.
Consider:
        Text.Printf.printf "hello"
This has type (forall a. IO a); it prints "hello", and returns 'undefined'.  We don't
want the GHCi repl loop to try to print that 'undefined'.  The neatest thing is to
default the 'a' to (), rather than to Integer (which is what would otherwise happen;
and then GHCi doesn't attempt to print the ().  So in interactive mode, we add
() to the list of defaulting types.  See #1200.

Additionally, the list type [] is added as a default specialization for
Traversable and Foldable. As such the default default list now has types of
varying kinds, e.g. ([] :: * -> *)  and (Integer :: *).

************************************************************************
*                                                                      *
\subsection{The InstInfo type}
*                                                                      *
************************************************************************

The InstInfo type summarises the information in an instance declaration

    instance c => k (t tvs) where b

It is used just for *local* instance decls (not ones from interface files).
But local instance decls includes
        - derived ones
        - generic ones
as well as explicit user written ones.
-}

data InstInfo a
  = InstInfo
      { iSpec   :: ClsInst          -- Includes the dfun id
      , iBinds  :: InstBindings a
      }

iDFunId :: InstInfo a -> DFunId
iDFunId info = instanceDFunId (iSpec info)

data InstBindings a
  = InstBindings
      { ib_tyvars  :: [Name]   -- Names of the tyvars from the instance head
                               -- that are lexically in scope in the bindings
                               -- Must correspond 1-1 with the forall'd tyvars
                               -- of the dfun Id.  When typechecking, we are
                               -- going to extend the typechecker's envt with
                               --     ib_tyvars -> dfun_forall_tyvars

      , ib_binds   :: LHsBinds a    -- Bindings for the instance methods

      , ib_pragmas :: [LSig a]      -- User pragmas recorded for generating
                                    -- specialised instances

      , ib_extensions :: [LangExt.Extension] -- Any extra extensions that should
                                             -- be enabled when type-checking
                                             -- this instance; needed for
                                             -- GeneralizedNewtypeDeriving

      , ib_derived :: Bool
           -- True <=> This code was generated by GHC from a deriving clause
           --          or standalone deriving declaration
           --          Used only to improve error messages
      }

instance (OutputableBndrId a)
       => Outputable (InstInfo (GhcPass a)) where
    ppr = pprInstInfoDetails

pprInstInfoDetails :: (OutputableBndrId a)
                   => InstInfo (GhcPass a) -> SDoc
pprInstInfoDetails info
   = hang (pprInstanceHdr (iSpec info) <+> text "where")
        2 (details (iBinds info))
  where
    details (InstBindings { ib_pragmas = p, ib_binds = b }) =
      pprDeclList (pprLHsBindsForUser b p)

simpleInstInfoClsTy :: InstInfo a -> (Class, Type)
simpleInstInfoClsTy info = case instanceHead (iSpec info) of
                           (_, cls, [ty]) -> (cls, ty)
                           _ -> panic "simpleInstInfoClsTy"

simpleInstInfoTy :: InstInfo a -> Type
simpleInstInfoTy info = snd (simpleInstInfoClsTy info)

simpleInstInfoTyCon :: InstInfo a -> TyCon
  -- Gets the type constructor for a simple instance declaration,
  -- i.e. one of the form       instance (...) => C (T a b c) where ...
simpleInstInfoTyCon inst = tcTyConAppTyCon (simpleInstInfoTy inst)

-- | Make a name for the dict fun for an instance decl.  It's an *external*
-- name, like other top-level names, and hence must be made with
-- newGlobalBinder.
newDFunName :: Class -> [Type] -> SrcSpan -> TcM Name
newDFunName clas tys loc
  = do  { is_boot <- tcIsHsBootOrSig
        ; mod     <- getModule
        ; let info_string = occNameString (getOccName clas) ++
                            concatMap (occNameString.getDFunTyKey) tys
        ; dfun_occ <- chooseUniqueOccTc (mkDFunOcc info_string is_boot)
        ; newGlobalBinder mod dfun_occ loc }

newFamInstTyConName :: Located Name -> [Type] -> TcM Name
newFamInstTyConName (L loc name) tys = mk_fam_inst_name id loc name [tys]

newFamInstAxiomName :: Located Name -> [[Type]] -> TcM Name
newFamInstAxiomName (L loc name) branches
  = mk_fam_inst_name mkInstTyCoOcc loc name branches

mk_fam_inst_name :: (OccName -> OccName) -> SrcSpan -> Name -> [[Type]] -> TcM Name
mk_fam_inst_name adaptOcc loc tc_name tyss
  = do  { mod   <- getModule
        ; let info_string = occNameString (getOccName tc_name) ++
                            intercalate "|" ty_strings
        ; occ   <- chooseUniqueOccTc (mkInstTyTcOcc info_string)
        ; newGlobalBinder mod (adaptOcc occ) loc }
  where
    ty_strings = map (concatMap (occNameString . getDFunTyKey)) tyss

{-
Stable names used for foreign exports and annotations.
For stable names, the name must be unique (see #1533).  If the
same thing has several stable Ids based on it, the
top-level bindings generated must not have the same name.
Hence we create an External name (doesn't change), and we
append a Unique to the string right here.
-}

mkStableIdFromString :: String -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromString str sig_ty loc occ_wrapper = do
    uniq <- newUnique
    mod <- getModule
    name <- mkWrapperName "stable" str
    let occ = mkVarOccFS name :: OccName
        gnm = mkExternalName uniq mod (occ_wrapper occ) loc :: Name
        id  = mkExportedVanillaId gnm sig_ty :: Id
    return id

mkStableIdFromName :: Name -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromName nm = mkStableIdFromString (getOccString nm)

mkWrapperName :: (MonadIO m, HasDynFlags m, HasModule m)
              => String -> String -> m FastString
mkWrapperName what nameBase
    = do dflags <- getDynFlags
         thisMod <- getModule
         let -- Note [Generating fresh names for ccall wrapper]
             wrapperRef = nextWrapperNum dflags
             pkg = unitString  (moduleUnit thisMod)
             mod = moduleNameString (moduleName      thisMod)
         wrapperNum <- liftIO $ atomicModifyIORef' wrapperRef $ \mod_env ->
             let num = lookupWithDefaultModuleEnv mod_env 0 thisMod
                 mod_env' = extendModuleEnv mod_env thisMod (num+1)
             in (mod_env', num)
         let components = [what, show wrapperNum, pkg, mod, nameBase]
         return $ mkFastString $ zEncodeString $ intercalate ":" components

{-
Note [Generating fresh names for FFI wrappers]

We used to use a unique, rather than nextWrapperNum, to distinguish
between FFI wrapper functions. However, the wrapper names that we
generate are external names. This means that if a call to them ends up
in an unfolding, then we can't alpha-rename them, and thus if the
unique randomly changes from one compile to another then we get a
spurious ABI change (#4012).

The wrapper counter has to be per-module, not global, so that the number we end
up using is not dependent on the modules compiled before the current one.
-}

{-
************************************************************************
*                                                                      *
\subsection{Errors}
*                                                                      *
************************************************************************
-}

pprBinders :: [Name] -> SDoc
-- Used in error messages
-- Use quotes for a single one; they look a bit "busy" for several
pprBinders [bndr] = quotes (ppr bndr)
pprBinders bndrs  = pprWithCommas ppr bndrs

notFound :: Name -> TcM TyThing
notFound name
  = do { lcl_env <- getLclEnv
       ; let stage = tcl_th_ctxt lcl_env
       ; case stage of   -- See Note [Out of scope might be a staging error]
           Splice {}
             | isUnboundName name -> failM  -- If the name really isn't in scope
                                            -- don't report it again (#11941)
             | otherwise -> stageRestrictionError (quotes (ppr name))
           _ -> failWithTc $
                vcat[text "GHC internal error:" <+> quotes (ppr name) <+>
                     text "is not in scope during type checking, but it passed the renamer",
                     text "tcl_env of environment:" <+> ppr (tcl_env lcl_env)]
                       -- Take care: printing the whole gbl env can
                       -- cause an infinite loop, in the case where we
                       -- are in the middle of a recursive TyCon/Class group;
                       -- so let's just not print it!  Getting a loop here is
                       -- very unhelpful, because it hides one compiler bug with another
       }

wrongThingErr :: String -> TcTyThing -> Name -> TcM a
-- It's important that this only calls pprTcTyThingCategory, which in
-- turn does not look at the details of the TcTyThing.
-- See Note [Placeholder PatSyn kinds] in GHC.Tc.Gen.Bind
wrongThingErr expected thing name
  = failWithTc (pprTcTyThingCategory thing <+> quotes (ppr name) <+>
                text "used as a" <+> text expected)

{- Note [Out of scope might be a staging error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  x = 3
  data T = MkT $(foo x)

where 'foo' is imported from somewhere.

This is really a staging error, because we can't run code involving 'x'.
But in fact the type checker processes types first, so 'x' won't even be
in the type envt when we look for it in $(foo x).  So inside splices we
report something missing from the type env as a staging error.
See #5752 and #5795.
-}
