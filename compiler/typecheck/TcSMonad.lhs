\begin{code}
{-# LANGUAGE CPP, TypeFamilies #-}

-- Type definitions for the constraint solver
module TcSMonad (

       -- Canonical constraints, definition is now in TcRnTypes

    WorkList(..), isEmptyWorkList, emptyWorkList,
    extendWorkListFunEq,
    extendWorkListNonEq, extendWorkListCt,
    extendWorkListCts, appendWorkList, selectWorkItem,
    workListSize,

    updWorkListTcS, updWorkListTcS_return, getWorkListImplics,

    updInertCans, updInertDicts, updInertIrreds, updInertFunEqs,

    Ct(..), Xi, tyVarsOfCt, tyVarsOfCts,
    emitInsoluble, emitWorkNC,

    isWanted, isDerived,
    isGivenCt, isWantedCt, isDerivedCt,

    mkGivenLoc,

    TcS, runTcS, runTcSWithEvBinds, failTcS, panicTcS, traceTcS, -- Basic functionality
    traceFireTcS, bumpStepCountTcS, csTraceTcS,
    tryTcS, nestTcS, nestImplicTcS, recoverTcS,
    wrapErrTcS, wrapWarnTcS,

    -- Getting and setting the flattening cache
    addSolvedDict, 

    -- Marking stuff as used
    addUsedRdrNamesTcS,

    deferTcSForAllEq,

    setEvBind,
    XEvTerm(..),
    Freshness(..), freshGoals, 

    StopOrContinue(..), continueWith, stopWith, andWhenContinue,

    xCtEvidence,        -- Transform a CtEvidence during a step
    rewriteEvidence,    -- Specialized version of xCtEvidence for coercions
    rewriteEqEvidence,  -- Yet more specialised, for equality coercions
    maybeSym,

    newTcEvBinds, newWantedEvVar, newWantedEvVarNC, newWantedEvVarNonrec, 
    newEvVar, newGivenEvVar, newDerived, 
    emitNewDerived, emitNewDerivedEq,
    instDFunConstraints,

       -- Creation of evidence variables
    setWantedTyBind, reportUnifications,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getTcEvBinds, getUntouchables,
    getTcEvBindsMap, 

    lookupFlatCache, newFlattenSkolem,            -- Flatten skolems

        -- Deque
    Deque(..), insertDeque, emptyDeque,

        -- Inerts
    InertSet(..), InertCans(..),
    getNoGivenEqs, setInertCans, getInertEqs, getInertCans,
    emptyInert, getTcSInerts, setTcSInerts, 
    getUnsolvedInerts, checkAllSolved,
    prepareInertsForImplications,
    addInertCan, insertInertItemTcS, insertFunEq,
    EqualCtList,
    lookupSolvedDict, extendFlatCache,

    findDict, findDictsByClass, addDict, addDictsByClass, delDict, partitionDicts,

    findFunEq, findTyEqs, 
    findFunEqsByTyCon, findFunEqs, partitionFunEqs,
    sizeFunEqMap,

    instDFunType,                              -- Instantiation
    newFlexiTcSTy, instFlexiTcS, instFlexiTcSHelperTcS,
    cloneMetaTyVar, demoteUnfilledFmv,

    Untouchables, isTouchableMetaTyVarTcS,
    isFilledMetaTyVar_maybe, isFilledMetaTyVar,
    zonkTyVarsAndFV, zonkTcType, zonkTcTyVar, zonkFlats,

    getDefaultInfo, getDynFlags, getGlobalRdrEnvTcS,

    matchFam,
    checkWellStagedDFun,
    pprEq                                    -- Smaller utils, re-exported from TcM
                                             -- TODO (DV): these are only really used in the
                                             -- instance matcher in TcSimplify. I am wondering
                                             -- if the whole instance matcher simply belongs
                                             -- here
) where

#include "HsVersions.h"

import HscTypes

import Inst
import InstEnv
import FamInst
import FamInstEnv

import qualified TcRnMonad as TcM
import qualified TcMType as TcM
import qualified TcEnv as TcM
       ( checkWellStaged, topIdLvl, tcGetDefaultTys )
import Kind
import TcType
import DynFlags
import Type
import CoAxiom(sfMatchFam)

import TcEvidence
import Class
import TyCon

import Name
import RdrName (RdrName, GlobalRdrEnv)
import RnEnv (addUsedRdrNames)
import Var
import VarEnv
import VarSet
import Outputable
import Bag
import UniqSupply

import FastString
import Util
import Id
import TcRnTypes

import BasicTypes
import Unique
import UniqFM
import Maybes ( orElse, firstJusts )

import TrieMap
import Control.Monad( ap, when, unless )
import MonadUtils
import Data.IORef
import Pair

#ifdef DEBUG
import Digraph
#endif
\end{code}

%************************************************************************
%*                                                                      *
%*                            Worklists                                *
%*  Canonical and non-canonical constraints that the simplifier has to  *
%*  work on. Including their simplification depths.                     *
%*                                                                      *
%*                                                                      *
%************************************************************************

Note [WorkList priorities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A WorkList contains canonical and non-canonical items (of all flavors).
Notice that each Ct now has a simplification depth. We may
consider using this depth for prioritization as well in the future.

As a simple form of priority queue, our worklist separates out
equalities (wl_eqs) from the rest of the canonical constraints,
so that it's easier to deal with them first, but the separation
is not strictly necessary. Notice that non-canonical constraints
are also parts of the worklist.


\begin{code}
data Deque a = DQ [a] [a]   -- Insert in RH field, remove from LH field
                            -- First to remove is at head of LH field

instance Outputable a => Outputable (Deque a) where
  ppr q = ppr (dequeList q)

dequeList :: Deque a -> [a]
dequeList (DQ as bs) = as ++ reverse bs  -- First one to come out at the start

emptyDeque :: Deque a
emptyDeque = DQ [] []

isEmptyDeque :: Deque a -> Bool
isEmptyDeque (DQ as bs) = null as && null bs

dequeSize :: Deque a -> Int
dequeSize (DQ as bs) = length as + length bs

insertDeque :: a -> Deque a -> Deque a
insertDeque b (DQ as bs) = DQ as (b:bs)

appendDeque :: Deque a -> Deque a -> Deque a
appendDeque (DQ as1 bs1) (DQ as2 bs2) = DQ (as1 ++ reverse bs1 ++ as2) bs2

extractDeque :: Deque a -> Maybe (Deque a, a)
extractDeque (DQ [] [])     = Nothing
extractDeque (DQ (a:as) bs) = Just (DQ as bs, a)
extractDeque (DQ [] bs)     = case reverse bs of
                                (a:as) -> Just (DQ as [], a)
                                [] -> panic "extractDeque"

-- See Note [WorkList priorities]
data WorkList 
  = WL { wl_eqs     :: [Ct]
       , wl_funeqs  :: Deque Ct
       , wl_rest    :: [Ct]
       , wl_implics :: Bag Implication  -- See Note [Residual implications]
    }

appendWorkList :: WorkList -> WorkList -> WorkList
appendWorkList 
    (WL { wl_eqs = eqs1, wl_funeqs = funeqs1, wl_rest = rest1, wl_implics = implics1 })
    (WL { wl_eqs = eqs2, wl_funeqs = funeqs2, wl_rest = rest2, wl_implics = implics2 })
   = WL { wl_eqs     = eqs1     ++            eqs2
        , wl_funeqs  = funeqs1  `appendDeque` funeqs2
        , wl_rest    = rest1    ++            rest2
        , wl_implics = implics1 `unionBags`   implics2 }


workListSize :: WorkList -> Int
workListSize (WL { wl_eqs = eqs, wl_funeqs = funeqs, wl_rest = rest })
  = length eqs + dequeSize funeqs + length rest

extendWorkListEq :: Ct -> WorkList -> WorkList
extendWorkListEq ct wl 
  = wl { wl_eqs = ct : wl_eqs wl }

extendWorkListFunEq :: Ct -> WorkList -> WorkList
extendWorkListFunEq ct wl
  = wl { wl_funeqs = insertDeque ct (wl_funeqs wl) }

extendWorkListNonEq :: Ct -> WorkList -> WorkList
-- Extension by non equality
extendWorkListNonEq ct wl
  = wl { wl_rest = ct : wl_rest wl }

extendWorkListImplic :: Implication -> WorkList -> WorkList
extendWorkListImplic implic wl
  = wl { wl_implics = implic `consBag` wl_implics wl }

extendWorkListCt :: Ct -> WorkList -> WorkList
-- Agnostic
extendWorkListCt ct wl
 = case classifyPredType (ctPred ct) of
     EqPred ty1 _
       | Just (tc,_) <- tcSplitTyConApp_maybe ty1
       , isSynFamilyTyCon tc
       -> extendWorkListFunEq ct wl
       | otherwise
       -> extendWorkListEq ct wl

     _ -> extendWorkListNonEq ct wl

extendWorkListCts :: [Ct] -> WorkList -> WorkList
-- Agnostic
extendWorkListCts cts wl = foldr extendWorkListCt wl cts

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList (WL { wl_eqs = eqs, wl_funeqs = funeqs
                    , wl_rest = rest, wl_implics = implics })
  = null eqs && null rest && isEmptyDeque funeqs && isEmptyBag implics

emptyWorkList :: WorkList
emptyWorkList = WL { wl_eqs  = [], wl_rest = []
                   , wl_funeqs = emptyDeque, wl_implics = emptyBag }

selectWorkItem :: WorkList -> (Maybe Ct, WorkList)
selectWorkItem wl@(WL { wl_eqs = eqs, wl_funeqs = feqs, wl_rest = rest })
  = case (eqs,feqs,rest) of
      (ct:cts,_,_)     -> (Just ct, wl { wl_eqs    = cts })
      (_,fun_eqs,_)    | Just (fun_eqs', ct) <- extractDeque fun_eqs
                       -> (Just ct, wl { wl_funeqs = fun_eqs' })
      (_,_,(ct:cts))   -> (Just ct, wl { wl_rest   = cts })
      (_,_,_)          -> (Nothing,wl)

-- Pretty printing
instance Outputable WorkList where
  ppr (WL { wl_eqs = eqs, wl_funeqs = feqs
          , wl_rest = rest, wl_implics = implics })
   = text "WL" <+> (braces $
     vcat [ ppUnless (null eqs) $ 
            ptext (sLit "Eqs =") <+> vcat (map ppr eqs)
          , ppUnless (isEmptyDeque feqs) $
            ptext (sLit "Funeqs =") <+> vcat (map ppr (dequeList feqs))
          , ppUnless (null rest) $
            ptext (sLit "Eqs =") <+> vcat (map ppr rest)
          , ppUnless (isEmptyBag implics) $
            ptext (sLit "Implics =") <+> vcat (map ppr (bagToList implics))
          ])
\end{code}

%************************************************************************
%*                                                                      *
%*                            Inert Sets                                *
%*                                                                      *
%*                                                                      *
%************************************************************************

Note [Detailed InertCans Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The InertCans represents a collection of constraints with the following properties:

  * All canonical

  * No two dictionaries with the same head
  * No two CIrreds with the same type

  * Family equations inert wrt top-level family axioms

  * Dictionaries have no matching top-level instance

  * Given family or dictionary constraints don't mention touchable
    unification variables

  * Non-CTyEqCan constraints are fully rewritten with respect
    to the CTyEqCan equalities (modulo canRewrite of course;
    eg a wanted cannot rewrite a given)

  * CTyEqCan equalities _do_not_ form an idempotent substitution, but
    they are guaranteed to not have any occurs errors. Additional notes:

       - The lack of idempotence of the inert substitution implies
         that we must make sure that when we rewrite a constraint we
         apply the substitution /recursively/ to the types
         involved. Currently the one AND ONLY way in the whole
         constraint solver that we rewrite types and constraints wrt
         to the inert substitution is TcFlatten/flattenTyVar.

       - In the past we did try to have the inert substitution as
         idempotent as possible but this would only be true for
         constraints of the same flavor, so in total the inert
         substitution could not be idempotent, due to flavor-related
         issued.  Note [Non-idempotent inert substitution] in TcFlatten
         explains what is going on.

       - Whenever a constraint ends up in the worklist we do
         recursively apply exhaustively the inert substitution to it
         to check for occurs errors.  But if an equality is already in
         the inert set and we can guarantee that adding a new equality
         will not cause the first equality to have an occurs check
         then we do not rewrite the inert equality.  This happens in
         TcInteract, rewriteInertEqsFromInertEq.

         See Note [Delicate equality kick-out] to see which inert
         equalities can safely stay in the inert set and which must be
         kicked out to be rewritten and re-checked for occurs errors.

Note [Type family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type-family equations, of form (ev : F tys ~ ty), live in three places

  * The work-list, of course

  * The inert_flat_cache.  This is used when flattening, to get maximal
    sharing.  It contains lots of things that are still in the work-list.
    E.g Suppose we have (w1: F (G a) ~ Int), and (w2: H (G a) ~ Int) in the
        work list.  Then we flatten w1, dumping (w3: G a ~ f1) in the work
        list.  Now if we flatten w2 before we get to w3, we still want to
        share that (G a).

    Because it contains work-list things, DO NOT use the flat cache to solve
    a top-level goal.  Eg in the above example we don't want to solve w3
    using w3 itself!

  * The inert_funeqs are un-solved but fully processed and in the InertCans.

\begin{code}
-- All Given (fully known) or Wanted or Derived
-- See Note [Detailed InertCans Invariants] for more
data InertCans
  = IC { inert_eqs :: TyVarEnv EqualCtList
              -- All CTyEqCans; index is the LHS tyvar
              -- Some Refl equalities are also in tcs_ty_binds
              -- see Note [Spontaneously solved in TyBinds] in TcInteract

       , inert_funeqs :: FunEqMap Ct
              -- All CFunEqCans; index is the whole family head type.

       , inert_dicts :: DictMap Ct
              -- Dictionaries only, index is the class
              -- NB: index is /not/ the whole type because FD reactions
              -- need to match the class but not necessarily the whole type.

       , inert_irreds :: Cts
              -- Irreducible predicates

       , inert_insols :: Cts
              -- Frozen errors (as non-canonicals)
       }

type EqualCtList = [Ct]
-- EqualCtList invariants:
--    * All are equalities
--    * All these equalities have the same LHS
--    * The list is never empty
--    * No element of the list can rewrite any other
--
-- From the fourth invariant it follows that the list is
--   - A single Given, or
--   - Multiple Wanteds, or
--   - Multiple Deriveds

-- The Inert Set
data InertSet
  = IS { inert_cans :: InertCans
              -- Canonical Given, Wanted, Derived (no Solved)
              -- Sometimes called "the inert set"

       , inert_flat_cache :: FunEqMap (TcCoercion, TcTyVar)
              -- See Note [Type family equations]
              -- If    F tys :-> (co, fsk), 
              -- then  co :: F tys ~ fsk
              -- Just a hash-cons cache for use when flattening only
              -- These include entirely un-processed goals, so don't use
              -- them to solve a top-level goal, else you may end up solving
              -- (w:F ty ~ a) by setting w:=w!  We just use the flat-cache
              -- when allocating a new flatten-skolem.
              -- Not necessarily inert wrt top-level equations (or inert_cans)

       , inert_solved_dicts   :: DictMap CtEvidence
              -- Of form ev :: C t1 .. tn
              -- Always the result of using a top-level instance declaration
              -- See Note [Solved constraints]
              -- - Used to avoid creating a new EvVar when we have a new goal
              --   that we have solved in the past
              -- - Stored not necessarily as fully rewritten
              --   (ToDo: rewrite lazily when we lookup)
       }
\end{code}

\begin{code}
instance Outputable InertCans where
  ppr ics = vcat [ ptext (sLit "Equalities:")
                   <+> vcat (map ppr (varEnvElts (inert_eqs ics)))
                 , ptext (sLit "Type-function equalities:")
                   <+> vcat (map ppr (funEqsToList (inert_funeqs ics)))
                 , ptext (sLit "Dictionaries:")
                   <+> vcat (map ppr (Bag.bagToList $ dictsToBag (inert_dicts ics)))
                 , ptext (sLit "Irreds:")
                   <+> vcat (map ppr (Bag.bagToList $ inert_irreds ics))
                 , text "Insolubles =" <+> -- Clearly print frozen errors
                    braces (vcat (map ppr (Bag.bagToList $ inert_insols ics)))
                 ]

instance Outputable InertSet where
  ppr is = vcat [ ppr $ inert_cans is
                , text "Solved dicts"  <+> int (sizeDictMap (inert_solved_dicts is)) ]

emptyInert :: InertSet
emptyInert
  = IS { inert_cans = IC { inert_eqs     = emptyVarEnv
                         , inert_dicts   = emptyDicts
                         , inert_funeqs  = emptyFunEqs
                         , inert_irreds  = emptyCts
                         , inert_insols  = emptyCts
                         }
       , inert_flat_cache    = emptyFunEqs
       , inert_solved_dicts  = emptyDictMap }

---------------
addInertCan :: InertCans -> Ct -> InertCans
-- Precondition: item /is/ canonical
addInertCan ics item@(CTyEqCan {})
  = ics { inert_eqs = extendVarEnv_C (\eqs _ -> item : eqs)
                              (inert_eqs ics)
                              (cc_tyvar item) [item] }

addInertCan ics item@(CFunEqCan { cc_fun = tc, cc_tyargs = tys })
  = ics { inert_funeqs = insertFunEq (inert_funeqs ics) tc tys item }

addInertCan ics item@(CIrredEvCan {})
  = ics { inert_irreds = inert_irreds ics `Bag.snocBag` item }
       -- The 'False' is because the irreducible constraint might later instantiate
       -- to an equality.
       -- But since we try to simplify first, if there's a constraint function FC with
       --    type instance FC Int = Show
       -- we'll reduce a constraint (FC Int a) to Show a, and never add an inert irreducible

addInertCan ics item@(CDictCan { cc_class = cls, cc_tyargs = tys })
  = ics { inert_dicts = addDict (inert_dicts ics) cls tys item }

addInertCan _ item
  = pprPanic "upd_inert set: can't happen! Inserting " $
    ppr item   -- Can't be CNonCanonical, CHoleCan,
               -- because they only land in inert_insols

--------------
insertInertItemTcS :: Ct -> TcS ()
-- Add a new item in the inerts of the monad
insertInertItemTcS item
  = do { traceTcS "insertInertItemTcS {" $
         text "Trying to insert new inert item:" <+> ppr item

       ; updInertCans (\ics -> addInertCan ics item)

       ; traceTcS "insertInertItemTcS }" $ empty }

addSolvedDict :: CtEvidence -> Class -> [Type] -> TcS ()
-- Add a new item in the solved set of the monad
addSolvedDict item cls tys
  | isIPPred (ctEvPred item)    -- Never cache "solved" implicit parameters (not sure why!)
  = return ()
  | otherwise
  = do { traceTcS "updSolvedSetTcs:" $ ppr item
       ; updInertTcS $ \ ics ->
             ics { inert_solved_dicts = addDict (inert_solved_dicts ics) cls tys item } }

updInertTcS :: (InertSet -> InertSet) -> TcS ()
-- Modify the inert set with the supplied function
updInertTcS upd_fn
  = do { is_var <- getTcSInertsRef
       ; wrapTcS (do { curr_inert <- TcM.readTcRef is_var
                     ; TcM.writeTcRef is_var (upd_fn curr_inert) }) }

getInertCans :: TcS InertCans
getInertCans = do { inerts <- getTcSInerts; return (inert_cans inerts) }

setInertCans :: InertCans -> TcS ()
setInertCans ics = updInertTcS $ \ inerts -> inerts { inert_cans = ics }

updInertCans :: (InertCans -> InertCans) -> TcS ()
-- Modify the inert set with the supplied function
updInertCans upd_fn
  = updInertTcS $ \ inerts -> inerts { inert_cans = upd_fn (inert_cans inerts) }

updInertDicts :: (DictMap Ct -> DictMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertDicts upd_fn
  = updInertCans $ \ ics -> ics { inert_dicts = upd_fn (inert_dicts ics) }

updInertFunEqs :: (FunEqMap Ct -> FunEqMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertFunEqs upd_fn
  = updInertCans $ \ ics -> ics { inert_funeqs = upd_fn (inert_funeqs ics) }

updInertIrreds :: (Cts -> Cts) -> TcS ()
-- Modify the inert set with the supplied function
updInertIrreds upd_fn
  = updInertCans $ \ ics -> ics { inert_irreds = upd_fn (inert_irreds ics) }


prepareInertsForImplications :: InertSet -> (InertSet)
-- See Note [Preparing inert set for implications]
prepareInertsForImplications is@(IS { inert_cans = cans })
  = is { inert_cans       = getGivens cans
       , inert_flat_cache = emptyFunEqs }  -- See Note [Do not inherit the flat cache]
  where
    getGivens (IC { inert_eqs    = eqs
                  , inert_irreds = irreds
                  , inert_funeqs = funeqs
                  , inert_dicts  = dicts })
      = IC { inert_eqs     = filterVarEnv  is_given_ecl eqs
           , inert_funeqs  = filterFunEqs  isGivenCt funeqs
           , inert_irreds  = Bag.filterBag isGivenCt irreds
           , inert_dicts   = filterDicts   isGivenCt dicts
           , inert_insols  = emptyCts }

    is_given_ecl :: EqualCtList -> Bool
    is_given_ecl (ct:rest) | isGivenCt ct = ASSERT( null rest ) True
    is_given_ecl _                        = False
\end{code}

Note [Do not inherit the flat cache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to inherit the flat cache when processing nested
implications.  Consider
   a ~ F b, forall c. b~Int => blah
If we have F b ~ fsk in the flat-cache, and we push that into the
nested implication, we might miss that F b can be rewritten to F Int,
and hence perhpas solve it.  Moreover, the fsk from outside is 
flattened out after solving the outer level, but and we don't 
do that flattening recursively.

Note [Preparing inert set for implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before solving the nested implications, we trim the inert set,
retaining only Givens.  These givens can be used when solving
the inner implications.

There might be cases where interactions between wanteds at different levels
could help to solve a constraint. For example

        class C a b | a -> b
        (C Int alpha), (forall d. C d blah => C Int a)

If we pushed the (C Int alpha) inwards, as a given, it can produce a
fundep (alpha~a) and this can float out again and be used to fix
alpha.  (In general we can't float class constraints out just in case
(C d blah) might help to solve (C Int a).)  But we ignore this possiblity.

For Derived constraints we don't have evidence, so we do not turn
them into Givens.  There can *be* deriving CFunEqCans; see Trac #8129.

\begin{code}
getInertEqs :: TcS (TyVarEnv EqualCtList)
getInertEqs = do { inert <- getTcSInerts
                 ; return (inert_eqs (inert_cans inert)) }

getUnsolvedInerts :: TcS ( Cts     -- Tyvar eqs: a ~ ty
                         , Cts     -- Fun eqs:   F a ~ ty
                         , Cts     -- Insoluble
                         , Cts )   -- All others
getUnsolvedInerts
 = do { IC { inert_eqs = tv_eqs, inert_funeqs = fun_eqs
           , inert_irreds = irreds, inert_dicts = idicts
           , inert_insols = insols } <- getInertCans

      ; let unsolved_tv_eqs  = foldVarEnv (\cts rest -> foldr add_if_unsolved rest cts)
                                          emptyCts tv_eqs
            unsolved_fun_eqs = foldFunEqs add_if_unsolved fun_eqs emptyCts
            unsolved_irreds  = Bag.filterBag is_unsolved irreds
            unsolved_dicts   = foldDicts add_if_unsolved idicts emptyCts
            others = unsolved_irreds `unionBags` unsolved_dicts

      ; return ( unsolved_tv_eqs, unsolved_fun_eqs, insols, others) }
              -- Keep even the given insolubles
              -- so that we can report dead GADT pattern match branches
  where
    add_if_unsolved :: Ct -> Cts -> Cts
    add_if_unsolved ct cts | is_unsolved ct = ct `consCts` cts
                           | otherwise      = cts

    is_unsolved ct = not (isGivenCt ct)   -- Wanted or Derived

getNoGivenEqs :: Untouchables     -- Untouchables of this implication
               -> [TcTyVar]       -- Skolems of this implication
               -> TcS Bool        -- True <=> definitely no residual given equalities
-- See Note [When does an implication have given equalities?]
getNoGivenEqs untch skol_tvs
  = do { inerts@(IC { inert_eqs = ieqs, inert_irreds = iirreds, inert_funeqs = funeqs })
             <- getInertCans
       ; let local_fsks = foldFunEqs add_fsk funeqs emptyVarSet

             has_given_eqs = foldrBag ((||) . ev_given_here . ctEvidence)  False iirreds
                          || foldVarEnv ((||) . eqs_given_here local_fsks) False ieqs

       ; traceTcS "getNoGivenEqs" (vcat [ppr has_given_eqs, ppr inerts])
       ; return (not has_given_eqs) }
  where
    eqs_given_here :: VarSet -> EqualCtList -> Bool
    eqs_given_here local_fsks [CTyEqCan { cc_tyvar = tv, cc_ev = ev }]
                              -- Givens are always a sigleton
      = not (skolem_bound_here local_fsks tv) && ev_given_here ev
    eqs_given_here _ _ = False

    ev_given_here :: CtEvidence -> Bool
    -- True for a Given bound by the curent implication,
    -- i.e. the current level
    ev_given_here ev
      =  isGiven ev
      && untch == tcl_untch (ctl_env (ctEvLoc ev))

    add_fsk :: Ct -> VarSet -> VarSet
    add_fsk ct fsks | CFunEqCan { cc_fsk = tv, cc_ev = ev } <- ct
                    , isGiven ev = extendVarSet fsks tv
                    | otherwise  = fsks

    skol_tv_set = mkVarSet skol_tvs
    skolem_bound_here local_fsks tv -- See Note [Let-bound skolems]
      = case tcTyVarDetails tv of
          SkolemTv {} -> tv `elemVarSet` skol_tv_set
          FlatSkol {} -> not (tv `elemVarSet` local_fsks)
          _           -> False
\end{code}

Note [When does an implication have given equalities?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider an implication
   beta => alpha ~ Int
where beta is a unification variable that has already been unified
to () in an outer scope.  Then we can float the (alpha ~ Int) out
just fine. So when deciding whether the givens contain an equality,
we should canonicalise first, rather than just looking at the original
givens (Trac #8644).

So we simply look at the inert, canonical Givens and see if there are
any equalities among them, the calculation of has_given_eqs.  There
are some wrinkles:

 * We must know which ones are bound in *this* implication and which
   are bound further out.  We can find that out from the Untouchable
   level of the Given, which is itself recorded in the tcl_untch field
   of the TcLclEnv stored in the Given (ev_given_here).

   What about interactions between inner and outer givens?
      - Outer given is rewritten by an inner given, then there must
        have been an inner given equality, hence the “given-eq” flag
        will be true anyway.

      - Inner given rewritten by outer, retains its level (ie. The inner one)

 * We must take account of *potential* equalities, like the one above:
      beta => ...blah...
   If we still don't know what beta is, we conservatively treat it as potentially
   becoming an equality. Hence including 'irreds' in the calculation or has_given_eqs.

 * When flattening givens, we generate Given equalities like
     <F [a]> : F [a] ~ f,
   with Refl evidence, and we *don't* want those to count as an equality
   in the givens!  After all, the entire flattening business is just an
   internal matter, and the evidence does not mention any of the 'givens'
   of this implication.  So we do not treat inert_funeqs as a 'given equality'.

 * See Note [Let-bound skolems] for another wrinkle

Note [Let-bound skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
If   * the inert set contains a canonical Given CTyEqCan (a ~ ty)
and  * 'a' is a skolem bound in this very implication, b

then:
a) The Given is pretty much a let-binding, like
      f :: (a ~ b->c) => a -> a
   Here the equality constraint is like saying
      let a = b->c in ...
   It is not adding any new, local equality  information,
   and hence can be ignored by has_given_eqs

b) 'a' will have been completely substituted out in the inert set,
   so we can safely discard it.  Notably, it doesn't need to be
   returned as part of 'fsks'

For an example, see Trac #9211.

\begin{code}
checkAllSolved :: TcS Bool
-- True if there are no unsolved wanteds
-- Ignore Derived for this purpose, unless in insolubles
checkAllSolved
 = do { is <- getTcSInerts

      ; let icans = inert_cans is
            unsolved_irreds = Bag.anyBag isWantedCt (inert_irreds icans)
            unsolved_dicts  = foldDicts ((||)  . isWantedCt) (inert_dicts icans)  False
            unsolved_funeqs = foldFunEqs ((||) . isWantedCt) (inert_funeqs icans) False
            unsolved_eqs    = foldVarEnv ((||) . any isWantedCt) False (inert_eqs icans)

      ; return (not (unsolved_eqs || unsolved_irreds
                     || unsolved_dicts || unsolved_funeqs
                     || not (isEmptyBag (inert_insols icans)))) }

lookupFlatCache :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcTyVar))
lookupFlatCache fam_tc tys
  = do { IS { inert_flat_cache = flat_cache
            , inert_cans = IC { inert_funeqs = inert_funeqs } } <- getTcSInerts
       ; return (firstJusts [lookup_inerts inert_funeqs,
                             lookup_flats flat_cache]) }
  where
    lookup_inerts inert_funeqs
      | Just (CFunEqCan { cc_ev = ctev, cc_fsk = fsk })
           <- findFunEqs inert_funeqs fam_tc tys
      = Just (ctEvCoercion ctev, fsk)
      | otherwise = Nothing

    lookup_flats flat_cache = findFunEq flat_cache fam_tc tys


lookupInInerts :: TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet?
lookupInInerts pty
  = do { IS { inert_solved_dicts = solved_dicts
            , inert_cans         = inert_cans }
            <- getTcSInerts
       ; return $ case (classifyPredType pty) of
           ClassPred cls tys
              | Just ctev <- findDict solved_dicts cls tys
              -> Just ctev
              | Just ct <- findDict (inert_dicts inert_cans) cls tys
              -> Just (ctEvidence ct)

           _other -> Nothing -- NB: No caching for equalities, IPs, holes, or errors
      }

lookupSolvedDict :: InertSet -> Class -> [Type] -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) cls tys
  = findDict solved cls tys
\end{code}


%************************************************************************
%*                                                                      *
                   TyEqMap
%*                                                                      *
%************************************************************************

\begin{code}
type TyEqMap a = TyVarEnv a

findTyEqs :: TyEqMap EqualCtList -> TyVar -> EqualCtList
findTyEqs m tv = lookupVarEnv m tv `orElse` []
\end{code}


%************************************************************************
%*                                                                      *
                   TcAppMap, DictMap, FunEqMap
%*                                                                      *
%************************************************************************

\begin{code}
type TcAppMap a = UniqFM (ListMap TypeMap a)
    -- Indexed by tycon then the arg types
    -- Used for types and classes; hence UniqFM

emptyTcAppMap :: TcAppMap a
emptyTcAppMap = emptyUFM

findTcApp :: TcAppMap a -> Unique -> [Type] -> Maybe a
findTcApp m u tys = do { tys_map <- lookupUFM m u
                       ; lookupTM tys tys_map }

delTcApp :: TcAppMap a -> Unique -> [Type] -> TcAppMap a
delTcApp m cls tys = adjustUFM (deleteTM tys) m cls

insertTcApp :: TcAppMap a -> Unique -> [Type] -> a -> TcAppMap a
insertTcApp m cls tys ct = alterUFM alter_tm m cls
  where
    alter_tm mb_tm = Just (insertTM tys ct (mb_tm `orElse` emptyTM))

-- mapTcApp :: (a->b) -> TcAppMap a -> TcAppMap b
-- mapTcApp f = mapUFM (mapTM f)

filterTcAppMap :: (Ct -> Bool) -> TcAppMap Ct -> TcAppMap Ct
filterTcAppMap f m
  = mapUFM do_tm m
  where
    do_tm tm = foldTM insert_mb tm emptyTM
    insert_mb ct tm
       | f ct      = insertTM tys ct tm
       | otherwise = tm
       where
         tys = case ct of
                CFunEqCan { cc_tyargs = tys } -> tys
                CDictCan  { cc_tyargs = tys } -> tys
                _ -> pprPanic "filterTcAppMap" (ppr ct)

tcAppMapToBag :: TcAppMap a -> Bag a
tcAppMapToBag m = foldTcAppMap consBag m emptyBag

foldTcAppMap :: (a -> b -> b) -> TcAppMap a -> b -> b
foldTcAppMap k m z = foldUFM (foldTM k) z m

-------------------------
type DictMap a = TcAppMap a

emptyDictMap :: DictMap a
emptyDictMap = emptyTcAppMap

sizeDictMap :: DictMap a -> Int
sizeDictMap m = foldDicts (\ _ x -> x+1) m 0

findDict :: DictMap a -> Class -> [Type] -> Maybe a
findDict m cls tys = findTcApp m (getUnique cls) tys

findDictsByClass :: DictMap a -> Class -> Bag a
findDictsByClass m cls
  | Just tm <- lookupUFM m cls = foldTM consBag tm emptyBag
  | otherwise                  = emptyBag

delDict :: DictMap a -> Class -> [Type] -> DictMap a
delDict m cls tys = delTcApp m (getUnique cls) tys

addDict :: DictMap a -> Class -> [Type] -> a -> DictMap a
addDict m cls tys item = insertTcApp m (getUnique cls) tys item

addDictsByClass :: DictMap Ct -> Class -> Bag Ct -> DictMap Ct
addDictsByClass m cls items
  = addToUFM m cls (foldrBag add emptyTM items)
  where
    add ct@(CDictCan { cc_tyargs = tys }) tm = insertTM tys ct tm
    add ct _ = pprPanic "addDictsByClass" (ppr ct)

filterDicts :: (Ct -> Bool) -> DictMap Ct -> DictMap Ct
filterDicts f m = filterTcAppMap f m

partitionDicts :: (Ct -> Bool) -> DictMap Ct -> (Bag Ct, DictMap Ct)
partitionDicts f m = foldTcAppMap k m (emptyBag, emptyDicts)
  where
    k ct (yeses, noes) | f ct      = (ct `consBag` yeses, noes)
                       | otherwise = (yeses,              add ct noes)
    add ct@(CDictCan { cc_class = cls, cc_tyargs = tys }) m
      = addDict m cls tys ct
    add ct _ = pprPanic "partitionDicts" (ppr ct)

dictsToBag :: DictMap a -> Bag a
dictsToBag = tcAppMapToBag

foldDicts :: (a -> b -> b) -> DictMap a -> b -> b
foldDicts = foldTcAppMap

emptyDicts :: DictMap a
emptyDicts = emptyTcAppMap

------------------------
type FunEqMap a = TcAppMap a  -- A map whose key is a (TyCon, [Type]) pair

emptyFunEqs :: TcAppMap a
emptyFunEqs = emptyTcAppMap

sizeFunEqMap :: FunEqMap a -> Int
sizeFunEqMap m = foldFunEqs (\ _ x -> x+1) m 0

findFunEq :: FunEqMap a -> TyCon -> [Type] -> Maybe a
findFunEq m tc tys = findTcApp m (getUnique tc) tys

findFunEqs :: FunEqMap a -> TyCon -> [Type] -> Maybe a
findFunEqs m tc tys = findTcApp m (getUnique tc) tys

funEqsToList :: FunEqMap a -> [a]
funEqsToList m = foldTcAppMap (:) m []

findFunEqsByTyCon :: FunEqMap a -> TyCon -> [a]
-- Get inert function equation constraints that have the given tycon
-- in their head.  Not that the constraints remain in the inert set.
-- We use this to check for derived interactions with built-in type-function
-- constructors.
findFunEqsByTyCon m tc
  | Just tm <- lookupUFM m tc = foldTM (:) tm []
  | otherwise                 = []

foldFunEqs :: (a -> b -> b) -> FunEqMap a -> b -> b
foldFunEqs = foldTcAppMap

-- mapFunEqs :: (a -> b) -> FunEqMap a -> FunEqMap b
-- mapFunEqs = mapTcApp

filterFunEqs :: (Ct -> Bool) -> FunEqMap Ct -> FunEqMap Ct
filterFunEqs = filterTcAppMap

insertFunEq :: FunEqMap a -> TyCon -> [Type] -> a -> FunEqMap a
insertFunEq m tc tys val = insertTcApp m (getUnique tc) tys val

insertFunEqCt :: FunEqMap Ct -> Ct -> FunEqMap Ct
insertFunEqCt m ct@(CFunEqCan { cc_fun = tc, cc_tyargs = tys })
  = insertFunEq m tc tys ct
insertFunEqCt _ ct = pprPanic "insertFunEqCt" (ppr ct)

partitionFunEqs :: (Ct -> Bool) -> FunEqMap Ct -> (Bag Ct, FunEqMap Ct)
partitionFunEqs f m = foldTcAppMap k m (emptyBag, emptyFunEqs)
  where
    k ct (yeses, noes)
      | f ct      = (yeses `snocBag` ct, noes)
      | otherwise = (yeses, insertFunEqCt noes ct)
\end{code}


%************************************************************************
%*                                                                      *
%*              The TcS solver monad                                    *
%*                                                                      *
%************************************************************************

Note [The TcS monad]
~~~~~~~~~~~~~~~~~~~~
The TcS monad is a weak form of the main Tc monad

All you can do is
    * fail
    * allocate new variables
    * fill in evidence variables

Filling in a dictionary evidence variable means to create a binding
for it, so TcS carries a mutable location where the binding can be
added.  This is initialised from the innermost implication constraint.

\begin{code}
data TcSEnv
  = TcSEnv {
      tcs_ev_binds    :: EvBindsVar,

      tcs_unified :: IORef Bool,
          -- The "dirty-flag" Bool is set True when 
          -- we unify a unification variable

      tcs_count    :: IORef Int, -- Global step count

      tcs_inerts   :: IORef InertSet, -- Current inert set
      tcs_worklist :: IORef WorkList  -- Current worklist
    }
\end{code}

\begin{code}

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a }

instance Functor TcS where
  fmap f m = TcS $ fmap f . unTcS m

instance Applicative TcS where
  pure  = return
  (<*>) = ap

instance Monad TcS where
  return x  = TcS (\_ -> return x)
  fail err  = TcS (\_ -> fail err)
  m >>= k   = TcS (\ebs -> unTcS m ebs >>= \r -> unTcS (k r) ebs)

instance MonadUnique TcS where
   getUniqueSupplyM = wrapTcS getUniqueSupplyM

-- Basic functionality
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wrapTcS :: TcM a -> TcS a
-- Do not export wrapTcS, because it promotes an arbitrary TcM to TcS,
-- and TcS is supposed to have limited functionality
wrapTcS = TcS . const -- a TcM action will not use the TcEvBinds

wrapErrTcS :: TcM a -> TcS a
-- The thing wrapped should just fail
-- There's no static check; it's up to the user
-- Having a variant for each error message is too painful
wrapErrTcS = wrapTcS

wrapWarnTcS :: TcM a -> TcS a
-- The thing wrapped should just add a warning, or no-op
-- There's no static check; it's up to the user
wrapWarnTcS = wrapTcS

failTcS, panicTcS :: SDoc -> TcS a
failTcS      = wrapTcS . TcM.failWith
panicTcS doc = pprPanic "TcCanonical" doc

traceTcS :: String -> SDoc -> TcS ()
traceTcS herald doc = wrapTcS (TcM.traceTc herald doc)

instance HasDynFlags TcS where
    getDynFlags = wrapTcS getDynFlags

getGlobalRdrEnvTcS :: TcS GlobalRdrEnv
getGlobalRdrEnvTcS = wrapTcS TcM.getGlobalRdrEnv

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = TcS $ \env -> do { let ref = tcs_count env
                                    ; n <- TcM.readTcRef ref
                                    ; TcM.writeTcRef ref (n+1) }

csTraceTcS :: SDoc -> TcS ()
csTraceTcS doc
  = wrapTcS $ csTraceTcM 1 (return doc)

traceFireTcS :: CtEvidence -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS ev doc
  = TcS $ \env -> csTraceTcM 1 $
    do { n <- TcM.readTcRef (tcs_count env)
       ; untch <- TcM.getUntouchables
       ; return (hang (int n <> brackets (ptext (sLit "U:") <> ppr untch 
                                          <> ppr (ctLocDepth (ctEvLoc ev))) 
                       <+> doc <> colon)
                     4 (ppr ev)) } 

csTraceTcM :: Int -> TcM SDoc -> TcM ()
-- Constraint-solver tracing, -ddump-cs-trace
csTraceTcM trace_level mk_doc
  = do { dflags <- getDynFlags
       ; when ((dopt Opt_D_dump_cs_trace dflags || dopt Opt_D_dump_tc_trace dflags)
               && traceLevel dflags >= trace_level) $
         do { msg <- mk_doc
            ; TcM.debugDumpTcRn msg } }

runTcS :: TcS a                -- What to run
       -> TcM (a, Bag EvBind)
runTcS tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; res <- runTcSWithEvBinds ev_binds_var tcs
       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; return (res, ev_binds) }

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds ev_binds_var tcs
  = do { unified_var <- TcM.newTcRef False
       ; step_count <- TcM.newTcRef 0
       ; inert_var <- TcM.newTcRef is
       ; wl_var <- TcM.newTcRef emptyWorkList

       ; let env = TcSEnv { tcs_ev_binds = ev_binds_var
                          , tcs_unified  = unified_var
                          , tcs_count    = step_count
                          , tcs_inerts   = inert_var
                          , tcs_worklist = wl_var }

             -- Run the computation
       ; res <- unTcS tcs env

       ; count <- TcM.readTcRef step_count
       ; when (count > 0) $ 
         csTraceTcM 0 $ return (ptext (sLit "Constraint solver steps =") <+> int count)

#ifdef DEBUG
       ; ev_binds <- TcM.getTcEvBinds ev_binds_var
       ; checkForCyclicBinds ev_binds
#endif

       ; return res }
  where
    is = emptyInert

#ifdef DEBUG
checkForCyclicBinds :: Bag EvBind -> TcM ()
checkForCyclicBinds ev_binds
  | null cycles
  = return ()
  | null coercion_cycles
  = TcM.traceTc "Cycle in evidence binds" $ ppr cycles
  | otherwise
  = pprPanic "Cycle in coercion bindings" $ ppr coercion_cycles
  where
    cycles :: [[EvBind]]
    cycles = [c | CyclicSCC c <- stronglyConnCompFromEdgedVertices edges]

    coercion_cycles = [c | c <- cycles, any is_co_bind c]
    is_co_bind (EvBind b _) = isEqVar b

    edges :: [(EvBind, EvVar, [EvVar])]
    edges = [(bind, bndr, varSetElems (evVarsOfTerm rhs)) | bind@(EvBind bndr rhs) <- bagToList ev_binds]
#endif

nestImplicTcS :: EvBindsVar -> Untouchables -> TcS a -> TcS a
nestImplicTcS ref inner_untch (TcS thing_inside)
  = TcS $ \ TcSEnv { tcs_unified = unified_var
                   , tcs_inerts = old_inert_var
                   , tcs_count = count } ->
    do { inerts <- TcM.readTcRef old_inert_var
       ; let nest_inert = inerts { inert_flat_cache = emptyFunEqs }
                                   -- See Note [Do not inherit the flat cache]
       ; new_inert_var <- TcM.newTcRef nest_inert
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = TcSEnv { tcs_ev_binds    = ref
                               , tcs_unified     = unified_var
                               , tcs_count       = count
                               , tcs_inerts      = new_inert_var
                               , tcs_worklist    = new_wl_var }
       ; res <- TcM.setUntouchables inner_untch $
                thing_inside nest_env

#ifdef DEBUG
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBinds ref
       ; checkForCyclicBinds ev_binds
#endif

       ; return res }

recoverTcS :: TcS a -> TcS a -> TcS a
recoverTcS (TcS recovery_code) (TcS thing_inside)
  = TcS $ \ env ->
    TcM.recoverM (recovery_code env) (thing_inside env)

nestTcS ::  TcS a -> TcS a
-- Use the current untouchables, augmenting the current
-- evidence bindings, and solved caches
-- But have no effect on the InertCans or insolubles
nestTcS (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_inerts = inerts_var }) ->
    do { inerts <- TcM.readTcRef inerts_var
       ; new_inert_var <- TcM.newTcRef inerts
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = env { tcs_inerts   = new_inert_var
                            , tcs_worklist = new_wl_var }
       ; thing_inside nest_env }

tryTcS :: TcS a -> TcS a
-- Like runTcS, but from within the TcS monad
-- Completely fresh inerts and worklist, be careful!
-- Moreover, we will simply throw away all the evidence generated.
tryTcS (TcS thing_inside)
  = TcS $ \env ->
    do { is_var <- TcM.newTcRef emptyInert
       ; unified_var <- TcM.newTcRef False
       ; ev_binds_var <- TcM.newTcEvBinds
       ; wl_var <- TcM.newTcRef emptyWorkList
       ; let nest_env = env { tcs_ev_binds = ev_binds_var
                            , tcs_unified  = unified_var
                            , tcs_inerts   = is_var
                            , tcs_worklist = wl_var }
       ; thing_inside nest_env }

-- Getters and setters of TcEnv fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Getter of inerts and worklist
getTcSInertsRef :: TcS (IORef InertSet)
getTcSInertsRef = TcS (return . tcs_inerts)

getTcSWorkListRef :: TcS (IORef WorkList)
getTcSWorkListRef = TcS (return . tcs_worklist)

getTcSInerts :: TcS InertSet
getTcSInerts = getTcSInertsRef >>= wrapTcS . (TcM.readTcRef)

setTcSInerts :: InertSet -> TcS ()
setTcSInerts ics = do { r <- getTcSInertsRef; wrapTcS (TcM.writeTcRef r ics) }

getWorkListImplics :: TcS (Bag Implication)
getWorkListImplics
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- wrapTcS (TcM.readTcRef wl_var)
       ; return (wl_implics wl_curr) }

updWorkListTcS :: (WorkList -> WorkList) -> TcS ()
updWorkListTcS f
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- wrapTcS (TcM.readTcRef wl_var)
       ; let new_work = f wl_curr
       ; wrapTcS (TcM.writeTcRef wl_var new_work) }

updWorkListTcS_return :: (WorkList -> (a,WorkList)) -> TcS a
-- Process the work list, returning a depleted work list,
-- plus a value extracted from it (typically a work item removed from it)
updWorkListTcS_return f
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- wrapTcS (TcM.readTcRef wl_var)
       ; traceTcS "updWorkList" (ppr wl_curr)
       ; let (res,new_work) = f wl_curr
       ; wrapTcS (TcM.writeTcRef wl_var new_work)
       ; return res }

emitWorkNC :: [CtEvidence] -> TcS ()
emitWorkNC evs
  | null evs
  = return ()
  | otherwise
  = do { traceTcS "Emitting fresh work" (vcat (map ppr evs))
       ; updWorkListTcS (extendWorkListCts (map mkNonCanonical evs)) }

emitInsoluble :: Ct -> TcS ()
-- Emits a non-canonical constraint that will stand for a frozen error in the inerts.
emitInsoluble ct
  = do { traceTcS "Emit insoluble" (ppr ct)
       ; updInertTcS add_insol }
  where
    this_pred = ctPred ct
    add_insol is@(IS { inert_cans = ics@(IC { inert_insols = old_insols }) })
      | already_there = is
      | otherwise     = is { inert_cans = ics { inert_insols = old_insols `snocCts` ct } }
      where
        already_there = not (isWantedCt ct) && anyBag (tcEqType this_pred . ctPred) old_insols
             -- See Note [Do not add duplicate derived insolubles]

getTcEvBinds :: TcS EvBindsVar
getTcEvBinds = TcS (return . tcs_ev_binds)

getUntouchables :: TcS Untouchables
getUntouchables = wrapTcS TcM.getUntouchables
\end{code}

\begin{code}
getTcEvBindsMap :: TcS EvBindMap
getTcEvBindsMap
  = do { EvBindsVar ev_ref _ <- getTcEvBinds
       ; wrapTcS $ TcM.readTcRef ev_ref }

setWantedTyBind :: TcTyVar -> TcType -> TcS ()
-- Add a type binding
-- We never do this twice!
setWantedTyBind tv ty
  | ASSERT2( isMetaTyVar tv, ppr tv )
    isFmvTyVar tv
  = ASSERT2( isMetaTyVar tv, ppr tv )
    wrapTcS (TcM.writeMetaTyVar tv ty)
           -- Write directly into the mutable tyvar
           -- Flatten meta-vars are born and die locally

  | otherwise
  = TcS $ \ env ->
    do { TcM.traceTc "setWantedTyBind" (ppr tv <+> text ":=" <+> ppr ty)
       ; TcM.writeMetaTyVar tv ty
       ; TcM.writeTcRef (tcs_unified env) True }

reportUnifications :: TcS a -> TcS (Bool, a)
reportUnifications (TcS thing_inside)
  = TcS $ \ env ->
    do { inner_unified <- TcM.newTcRef False
       ; res <- thing_inside (env { tcs_unified = inner_unified })
       ; dirty <- TcM.readTcRef inner_unified
       ; return (dirty, res) }
\end{code}

\begin{code}
getDefaultInfo ::  TcS ([Type], (Bool, Bool))
getDefaultInfo = wrapTcS TcM.tcGetDefaultTys

-- Just get some environments needed for instance looking up and matching
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getInstEnvs :: TcS (InstEnv, InstEnv)
getInstEnvs = wrapTcS $ Inst.tcGetInstEnvs

getFamInstEnvs :: TcS (FamInstEnv, FamInstEnv)
getFamInstEnvs = wrapTcS $ FamInst.tcGetFamInstEnvs

getTopEnv :: TcS HscEnv
getTopEnv = wrapTcS $ TcM.getTopEnv

getGblEnv :: TcS TcGblEnv
getGblEnv = wrapTcS $ TcM.getGblEnv

-- Setting names as used (used in the deriving of Coercible evidence)
-- Too hackish to expose it to TcS? In that case somehow extract the used
-- constructors from the result of solveInteract
addUsedRdrNamesTcS :: [RdrName] -> TcS ()
addUsedRdrNamesTcS names = wrapTcS  $ addUsedRdrNames names

-- Various smaller utilities [TODO, maybe will be absorbed in the instance matcher]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

checkWellStagedDFun :: PredType -> DFunId -> CtLoc -> TcS ()
checkWellStagedDFun pred dfun_id loc
  = wrapTcS $ TcM.setCtLoc loc $
    do { use_stage <- TcM.getStage
       ; TcM.checkWellStaged pp_thing bind_lvl (thLevel use_stage) }
  where
    pp_thing = ptext (sLit "instance for") <+> quotes (ppr pred)
    bind_lvl = TcM.topIdLvl dfun_id

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprParendType ty1 <+> char '~' <+> pprParendType ty2

isTouchableMetaTyVarTcS :: TcTyVar -> TcS Bool
isTouchableMetaTyVarTcS tv
  = do { untch <- getUntouchables
       ; return $ isTouchableMetaTyVar untch tv }

isFilledMetaTyVar_maybe :: TcTyVar -> TcS (Maybe Type)
isFilledMetaTyVar_maybe tv
 = ASSERT2( isTcTyVar tv, ppr tv )
   case tcTyVarDetails tv of
     MetaTv { mtv_ref = ref }
        -> do { cts <- wrapTcS (TcM.readTcRef ref)
              ; case cts of
                  Indirect ty -> return (Just ty)
                  Flexi       -> return Nothing }
     _ -> return Nothing

isFilledMetaTyVar :: TcTyVar -> TcS Bool
isFilledMetaTyVar tv = wrapTcS (TcM.isFilledMetaTyVar tv)

zonkTyVarsAndFV :: TcTyVarSet -> TcS TcTyVarSet
zonkTyVarsAndFV tvs = wrapTcS (TcM.zonkTyVarsAndFV tvs)

zonkTcType :: TcType -> TcS TcType
zonkTcType ty = wrapTcS (TcM.zonkTcType ty)

zonkTcTyVar :: TcTyVar -> TcS TcType
zonkTcTyVar tv = wrapTcS (TcM.zonkTcTyVar tv)

zonkFlats :: Cts -> TcS Cts
zonkFlats cts = wrapTcS (TcM.zonkFlats cts)
\end{code}

Note [Do not add duplicate derived insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we *must* add an insoluble (Int ~ Bool) even if there is
one such there already, because they may come from distinct call
sites.  Not only do we want an error message for each, but with
-fdefer-type-errors we must generate evidence for each.  But for
*derived* insolubles, we only want to report each one once.  Why?

(a) A constraint (C r s t) where r -> s, say, may generate the same fundep
    equality many times, as the original constraint is sucessively rewritten.

(b) Ditto the successive iterations of the main solver itself, as it traverses
    the constraint tree. See example below.

Also for *given* insolubles we may get repeated errors, as we
repeatedly traverse the constraint tree.  These are relatively rare
anyway, so removing duplicates seems ok.  (Alternatively we could take
the SrcLoc into account.)

Note that the test does not need to be particularly efficient because
it is only used if the program has a type error anyway.

Example of (b): assume a top-level class and instance declaration:

  class D a b | a -> b
  instance D [a] [a]

Assume we have started with an implication:

  forall c. Eq c => { wc_flat = D [c] c [W] }

which we have simplified to:

  forall c. Eq c => { wc_flat = D [c] c [W]
                    , wc_insols = (c ~ [c]) [D] }

For some reason, e.g. because we floated an equality somewhere else,
we might try to re-solve this implication. If we do not do a
dropDerivedWC, then we will end up trying to solve the following
constraints the second time:

  (D [c] c) [W]
  (c ~ [c]) [D]

which will result in two Deriveds to end up in the insoluble set:

  wc_flat   = D [c] c [W]
  wc_insols = (c ~ [c]) [D], (c ~ [c]) [D]



\begin{code}
-- Flatten skolems
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
newFlattenSkolem :: CtEvidence -> TcType         -- F xis
                 -> TcS (CtEvidence, TcTyVar)    -- [W] x:: F xis ~ fsk
newFlattenSkolem ctxt_ev fam_ty
  | isGiven ctxt_ev   -- Make a given
  =  do { fsk <- wrapTcS $
                 do { uniq <- TcM.newUnique
                    ; let name = TcM.mkTcTyVarName uniq (fsLit "fsk")
                    ; return (mkTcTyVar name (typeKind fam_ty) (FlatSkol fam_ty)) }
        ; let ev = CtGiven { ctev_pred = mkTcEqPred fam_ty (mkTyVarTy fsk)
                           , ctev_evtm = EvCoercion (mkTcNomReflCo fam_ty)
                           , ctev_loc  = loc }
        ; return (ev, fsk) }

  | otherwise        -- Make a wanted
  = do { fuv <- wrapTcS $
                 do { uniq <- TcM.newUnique
                    ; ref  <- TcM.newMutVar Flexi
                    ; let details = MetaTv { mtv_info  = FlatMetaTv
                                           , mtv_ref   = ref
                                           , mtv_untch = fskUntouchables }
                          name = TcM.mkTcTyVarName uniq (fsLit "s")
                    ; return (mkTcTyVar name (typeKind fam_ty) details) }
       ; ev <- newWantedEvVarNC loc (mkTcEqPred fam_ty (mkTyVarTy fuv))
       ; return (ev, fuv) }
  where
    loc = ctEvLoc ctxt_ev

extendFlatCache :: TyCon -> [Type] -> (TcCoercion, TcTyVar) -> TcS ()
extendFlatCache tc xi_args (co, fsk)
  = do { dflags <- getDynFlags
       ; when (gopt Opt_FlatCache dflags) $
         updInertTcS $ \ is@(IS { inert_flat_cache = fc }) ->
            is { inert_flat_cache = insertFunEq fc tc xi_args (co, fsk) } }

-- Instantiations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instDFunType :: DFunId -> [DFunInstType] -> TcS ([TcType], TcType)
instDFunType dfun_id mb_inst_tys
  = wrapTcS $ go dfun_tvs mb_inst_tys (mkTopTvSubst [])
  where
    (dfun_tvs, dfun_phi) = tcSplitForAllTys (idType dfun_id)

    go :: [TyVar] -> [DFunInstType] -> TvSubst -> TcM ([TcType], TcType)
    go [] [] subst = return ([], substTy subst dfun_phi)
    go (tv:tvs) (Just ty : mb_tys) subst
      = do { (tys, phi) <- go tvs mb_tys (extendTvSubst subst tv ty)
           ; return (ty : tys, phi) }
    go (tv:tvs) (Nothing : mb_tys) subst
      = do { ty <- instFlexiTcSHelper (tyVarName tv) (substTy subst (tyVarKind tv))
                         -- Don't forget to instantiate the kind!
                         -- cf TcMType.tcInstTyVarX
           ; (tys, phi) <- go tvs mb_tys (extendTvSubst subst tv ty)
           ; return (ty : tys, phi) }
    go _ _ _ = pprPanic "instDFunTypes" (ppr dfun_id $$ ppr mb_inst_tys)

newFlexiTcSTy :: Kind -> TcS TcType
newFlexiTcSTy knd = wrapTcS (TcM.newFlexiTyVarTy knd)

cloneMetaTyVar :: TcTyVar -> TcS TcTyVar
cloneMetaTyVar tv = wrapTcS (TcM.cloneMetaTyVar tv)

demoteUnfilledFmv :: TcTyVar -> TcS ()
-- If a flatten-meta-var is still un-filled,
-- turn it into an ordinary meta-var
demoteUnfilledFmv fmv
  = wrapTcS $ do { is_filled <- TcM.isFilledMetaTyVar fmv
                 ; unless is_filled $
                   do { tv_ty <- TcM.newFlexiTyVarTy (tyVarKind fmv)
                      ; TcM.writeMetaTyVar fmv tv_ty } }

instFlexiTcS :: [TKVar] -> TcS (TvSubst, [TcType])
instFlexiTcS tvs = wrapTcS (mapAccumLM inst_one emptyTvSubst tvs)
  where
     inst_one subst tv
         = do { ty' <- instFlexiTcSHelper (tyVarName tv)
                                          (substTy subst (tyVarKind tv))
              ; return (extendTvSubst subst tv ty', ty') }

instFlexiTcSHelper :: Name -> Kind -> TcM TcType
instFlexiTcSHelper tvname kind
  = do { uniq <- TcM.newUnique
       ; details <- TcM.newMetaDetails TauTv
       ; let name = setNameUnique tvname uniq
       ; return (mkTyVarTy (mkTcTyVar name kind details)) }

instFlexiTcSHelperTcS :: Name -> Kind -> TcS TcType
instFlexiTcSHelperTcS n k = wrapTcS (instFlexiTcSHelper n k)


-- Creating and setting evidence variables and CtFlavors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data XEvTerm
  = XEvTerm { ev_preds  :: [PredType]           -- New predicate types
            , ev_comp   :: [EvTerm] -> EvTerm   -- How to compose evidence
            , ev_decomp :: EvTerm -> [EvTerm]   -- How to decompose evidence
            -- In both ev_comp and ev_decomp, the [EvTerm] is 1-1 with ev_preds
            -- and each EvTerm has type of the corresponding EvPred
            }

data Freshness = Fresh | Cached

freshGoals :: [(CtEvidence, Freshness)] -> [CtEvidence]
freshGoals mns = [ ctev | (ctev, Fresh) <- mns ]

setEvBind :: EvVar -> EvTerm -> TcS ()
setEvBind the_ev tm
  = do { tc_evbinds <- getTcEvBinds
       ; wrapTcS $ TcM.addTcEvBind tc_evbinds the_ev tm }

newTcEvBinds :: TcS EvBindsVar
newTcEvBinds = wrapTcS TcM.newTcEvBinds

newEvVar :: TcPredType -> TcS EvVar
newEvVar pred = wrapTcS (TcM.newEvVar pred)

newGivenEvVar :: CtLoc -> (TcPredType, EvTerm) -> TcS CtEvidence
-- Make a new variable of the given PredType,
-- immediately bind it to the given term
-- and return its CtEvidence
newGivenEvVar loc (pred, rhs)
  = do { new_ev <- newEvVar pred
       ; setEvBind new_ev rhs
       ; return (CtGiven { ctev_pred = pred, ctev_evtm = EvId new_ev, ctev_loc = loc }) }

newWantedEvVarNC :: CtLoc -> TcPredType -> TcS CtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC loc pty
  = do { new_ev <- newEvVar pty
       ; return (CtWanted { ctev_pred = pty, ctev_evar = new_ev, ctev_loc = loc })}

-- | Variant of newWantedEvVar that has a lower bound on the depth of the result
--   (see Note [Preventing recursive dictionaries])
newWantedEvVarNonrec :: CtLoc -> TcPredType -> TcS (CtEvidence, Freshness)
newWantedEvVarNonrec loc pty
  = do { mb_ct <- lookupInInerts pty
       ; case mb_ct of
            Just ctev | not (isDerived ctev) && ctEvCheckDepth (ctLocDepth loc) ctev
                      -> do { traceTcS "newWantedEvVarNonrec/cache hit" $ ppr ctev
                            ; return (ctev, Cached) }
            _ -> do { ctev <- newWantedEvVarNC loc pty
                    ; traceTcS "newWantedEvVarNonrec/cache miss" $ ppr ctev
                    ; return (ctev, Fresh) } }

newWantedEvVar :: CtLoc -> TcPredType -> TcS (CtEvidence, Freshness)
newWantedEvVar loc pty
  = do { mb_ct <- lookupInInerts pty
       ; case mb_ct of
            Just ctev | not (isDerived ctev)
                      -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                            ; return (ctev, Cached) }
            _ -> do { ctev <- newWantedEvVarNC loc pty
                    ; traceTcS "newWantedEvVar/cache miss" $ ppr ctev
                    ; return (ctev, Fresh) } }

emitNewDerivedEq :: CtLoc -> Pair TcType -> TcS ()
-- Create new Derived and put it in the work list
emitNewDerivedEq loc (Pair ty1 ty2)
  | ty1 `tcEqType` ty2   -- Quite common!
  = return ()
  | otherwise
  = emitNewDerived loc (mkTcEqPred ty1 ty2)

emitNewDerived :: CtLoc -> TcPredType -> TcS ()
-- Create new Derived and put it in the work list
emitNewDerived loc pred
  = do { mb_ct <- lookupInInerts pred
       ; case mb_ct of
           Just {} -> return ()
           Nothing -> do { traceTcS "Emitting [D]" (ppr der_ct)
                         ; updWorkListTcS (extendWorkListCt der_ct) } }
  where
    der_ct = mkNonCanonical (CtDerived { ctev_pred = pred, ctev_loc = loc })

newDerived :: CtLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Returns Nothing    if cached,
--         Just pred  if not cached
newDerived loc pred
  = do { mb_ct <- lookupInInerts pred
       ; return (case mb_ct of
                    Just {} -> Nothing
                    Nothing -> Just (CtDerived { ctev_pred = pred, ctev_loc = loc })) }

instDFunConstraints :: CtLoc -> TcThetaType -> TcS [(CtEvidence, Freshness)]
instDFunConstraints loc = mapM (newWantedEvVar loc)
\end{code}


Note [xCtEvidence]
~~~~~~~~~~~~~~~~~~
A call might look like this:

    xCtEvidence ev evidence-transformer

  ev is Given   => use ev_decomp to create new Givens for ev_preds,
                   and return them

  ev is Wanted  => create new wanteds for ev_preds,
                   use ev_comp to bind ev,
                   return fresh wanteds (ie ones not cached in inert_cans or solved)

  ev is Derived => create new deriveds for ev_preds
                      (unless cached in inert_cans or solved)

Note: The [CtEvidence] returned is a subset of the subgoal-preds passed in
      Ones that are already cached are not returned

Example
    ev : Tree a b ~ Tree c d
    xCtEvidence ev [a~c, b~d] (XEvTerm { ev_comp = \[c1 c2]. <Tree> c1 c2
                                       , ev_decomp = \c. [nth 1 c, nth 2 c] })
              (\fresh-goals.  stuff)

Note [Bind new Givens immediately]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Givens we make new EvVars and bind them immediately. We don't worry
about caching, but we don't expect complicated calculations among Givens.
It is important to bind each given:
      class (a~b) => C a b where ....
      f :: C a b => ....
Then in f's Givens we have g:(C a b) and the superclass sc(g,0):a~b.
But that superclass selector can't (yet) appear in a coercion
(see evTermCoercion), so the easy thing is to bind it to an Id.

See Note [Coercion evidence terms] in TcEvidence.

Note [Do not create Given kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to create a Given kind equality like

   [G]  kv ~ k   -- kv is a skolem kind variable
                 -- Reason we don't yet support non-Refl kind equalities

This showed up in Trac #8566, where we had a data type
   data I (u :: U *) (r :: [*]) :: * where
        A :: I (AA t as) r                  -- Existential k
so A has type
   A :: forall (u:U *) (r:[*])                  Universal
        (k:BOX) (t:k) (as:[U *]).        Existential
        (u ~ AA * k t as) => I u r

There is no direct kind equality, but in a pattern match where 'u' is
instantiated to, say, (AA * kk (t1:kk) as1), we'd decompose to get
   k ~ kk, t ~ t1, as ~ as1
This is bad.  We "fix" this by simply ignoring the Given kind equality
But the Right Thing is to add kind equalities!

But note (Trac #8705) that we *do* create Given (non-canonical) equalities
with un-equal kinds, e.g.
   [G]  t1::k1 ~ t2::k2   -- k1 and k2 are un-equal kinds
Reason: k1 or k2 might be unification variables that have already been
unified (at this point we have not canonicalised the types), so we want
to emit this t1~t2 as a (non-canonical) Given in the work-list. If k1/k2 
have been unified, we'll find that when we canonicalise it, and the 
t1~t2 information may be crucial (Trac #8705 is an example).

If it turns out that k1 and k2 are really un-equal, then it'll end up
as an Irreducible (see Note [Equalities with incompatible kinds] in
TcCanonical), and will do no harm.

\begin{code}
xCtEvidence :: CtEvidence            -- Original evidence
            -> XEvTerm               -- Instructions about how to manipulate evidence
            -> TcS ()

xCtEvidence (CtWanted { ctev_evar = evar, ctev_loc = loc })
            (XEvTerm { ev_preds = ptys, ev_comp = comp_fn })
  = do { new_evars <- mapM (newWantedEvVar loc) ptys
       ; setEvBind evar (comp_fn (map (ctEvTerm . fst) new_evars))
       ; emitWorkNC (freshGoals new_evars) }
         -- Note the "NC": these are fresh goals, not necessarily canonical

xCtEvidence (CtGiven { ctev_evtm = tm, ctev_loc = loc })
            (XEvTerm { ev_preds = ptys, ev_decomp = decomp_fn })
  = ASSERT( equalLength ptys (decomp_fn tm) )
    do { given_evs <- mapM (newGivenEvVar loc) $    -- See Note [Bind new Givens immediately]
                      filterOut bad_given_pred (ptys `zip` decomp_fn tm)
       ; emitWorkNC given_evs }
  where
    -- See Note [Do not create Given kind equalities]
    bad_given_pred (pred_ty, _)
      | EqPred t1 _ <- classifyPredType pred_ty
      = isKind t1
      | otherwise
      = False

xCtEvidence (CtDerived { ctev_loc = loc })
            (XEvTerm { ev_preds = ptys })
  = mapM_ (emitNewDerived loc) ptys

-----------------------------
data StopOrContinue a
  = ContinueWith a    -- The constraint was not solved, although it may have
                      --   been rewritten

  | Stop CtEvidence   -- The (rewritten) constraint was solved
         SDoc         -- Tells how it was solved
                      -- Any new sub-goals have been put on the work list

instance Functor StopOrContinue where
  fmap f (ContinueWith x) = ContinueWith (f x)
  fmap _ (Stop ev s)      = Stop ev s

instance Outputable a => Outputable (StopOrContinue a) where
  ppr (Stop ev s)      = ptext (sLit "Stop") <> parens s <+> ppr ev
  ppr (ContinueWith w) = ptext (sLit "ContinueWith") <+> ppr w

continueWith :: a -> TcS (StopOrContinue a)
continueWith = return . ContinueWith

stopWith :: CtEvidence -> String -> TcS (StopOrContinue a)
stopWith ev s = return (Stop ev (text s))

andWhenContinue :: TcS (StopOrContinue a)
                -> (a -> TcS (StopOrContinue b))
                -> TcS (StopOrContinue b)
andWhenContinue tcs1 tcs2
  = do { r <- tcs1
       ; case r of
           Stop ev s       -> return (Stop ev s)
           ContinueWith ct -> tcs2 ct }

rewriteEvidence :: CtEvidence   -- old evidence
                -> TcPredType   -- new predicate
                -> TcCoercion   -- Of type :: new predicate ~ <type of old evidence>
                -> TcS (StopOrContinue CtEvidence)
-- Returns Just new_ev iff either (i)  'co' is reflexivity
--                             or (ii) 'co' is not reflexivity, and 'new_pred' not cached
-- In either case, there is nothing new to do with new_ev
{-
     rewriteEvidence old_ev new_pred co
Main purpose: create new evidence for new_pred;
              unless new_pred is cached already
* Returns a new_ev : new_pred, with same wanted/given/derived flag as old_ev
* If old_ev was wanted, create a binding for old_ev, in terms of new_ev
* If old_ev was given, AND not cached, create a binding for new_ev, in terms of old_ev
* Returns Nothing if new_ev is already cached

        Old evidence    New predicate is               Return new evidence
        flavour                                        of same flavor
        -------------------------------------------------------------------
        Wanted          Already solved or in inert     Nothing
        or Derived      Not                            Just new_evidence

        Given           Already in inert               Nothing
                        Not                            Just new_evidence

Note [Rewriting with Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~
If the coercion is just reflexivity then you may re-use the same
variable.  But be careful!  Although the coercion is Refl, new_pred
may reflect the result of unification alpha := ty, so new_pred might
not _look_ the same as old_pred, and it's vital to proceed from now on
using new_pred.

The flattener preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.
 -}


rewriteEvidence old_ev@(CtDerived { ctev_loc = loc }) new_pred _co
  = -- If derived, don't even look at the coercion.
    -- This is very important, DO NOT re-order the equations for
    -- rewriteEvidence to put the isTcReflCo test first!
    -- Why?  Because for *Derived* constraints, c, the coercion, which
    -- was produced by flattening, may contain suspended calls to
    -- (ctEvTerm c), which fails for Derived constraints.
    -- (Getting this wrong caused Trac #7384.)
    do { mb_ev <- newDerived loc new_pred
       ; case mb_ev of
           Just new_ev -> continueWith new_ev
           Nothing     -> stopWith old_ev "Cached derived" }

rewriteEvidence old_ev new_pred co
  | isTcReflCo co -- See Note [Rewriting with Refl]
  = return (ContinueWith (old_ev { ctev_pred = new_pred }))

rewriteEvidence (CtGiven { ctev_evtm = old_tm , ctev_loc = loc }) new_pred co
  = do { new_ev <- newGivenEvVar loc (new_pred, new_tm)  -- See Note [Bind new Givens immediately]
       ; return (ContinueWith new_ev) }
  where
    new_tm = mkEvCast old_tm (mkTcSubCo (mkTcSymCo co))  -- mkEvCast optimises ReflCo

rewriteEvidence ev@(CtWanted { ctev_evar = evar, ctev_loc = loc }) new_pred co
  = do { (new_ev, freshness) <- newWantedEvVar loc new_pred
       ; MASSERT( tcCoercionRole co == Nominal )
       ; setEvBind evar (mkEvCast (ctEvTerm new_ev) (mkTcSubCo co))
       ; case freshness of
            Fresh  -> continueWith new_ev
            Cached -> stopWith ev "Cached wanted" }


rewriteEqEvidence :: CtEvidence         -- Old evidence :: olhs ~ orhs (not swapped)
                                        --              or orhs ~ olhs (swapped)
                  -> SwapFlag
                  -> TcType -> TcType   -- New predicate  nlhs ~ nrhs
                                        -- Should be zonked, because we use typeKind on nlhs/nrhs
                  -> TcCoercion         -- lhs_co, of type :: nlhs ~ olhs
                  -> TcCoercion         -- rhs_co, of type :: nrhs ~ orhs
                  -> TcS (StopOrContinue CtEvidence)  -- Of type nlhs ~ nrhs
-- For (rewriteEqEvidence (Given g olhs orhs) False nlhs nrhs lhs_co rhs_co)
-- we generate
-- If not swapped
--      g1 : nlhs ~ nrhs = lhs_co ; g ; sym rhs_co
-- If 'swapped'
--      g1 : nlhs ~ nrhs = lhs_co ; Sym g ; sym rhs_co
--
-- For (Wanted w) we do the dual thing.
-- New  w1 : nlhs ~ nrhs
-- If not swapped
--      w : olhs ~ orhs = sym lhs_co ; w1 ; rhs_co
-- If swapped
--      w : orhs ~ olhs = sym rhs_co ; sym w1 ; lhs_co
--
-- It's all a form of rewwriteEvidence, specialised for equalities
rewriteEqEvidence old_ev swapped nlhs nrhs lhs_co rhs_co
  | CtDerived { ctev_loc = loc } <- old_ev
  = do { mb <- newDerived loc (mkTcEqPred nlhs nrhs)
       ; case mb of 
           Just new_ev -> continueWith new_ev
           Nothing     -> stopWith old_ev "Cached derived" }

  | NotSwapped <- swapped
  , isTcReflCo lhs_co      -- See Note [Rewriting with Refl]
  , isTcReflCo rhs_co
  = return (ContinueWith (old_ev { ctev_pred = new_pred }))

  | CtGiven { ctev_evtm = old_tm , ctev_loc = loc } <- old_ev
  = do { let new_tm = EvCoercion (lhs_co 
                                  `mkTcTransCo` maybeSym swapped (evTermCoercion old_tm)
                                  `mkTcTransCo` mkTcSymCo rhs_co)
       ; new_ev <- newGivenEvVar loc (new_pred, new_tm)  -- See Note [Bind new Givens immediately]
       ; return (ContinueWith new_ev) }

  | CtWanted { ctev_evar = evar, ctev_loc = loc } <- old_ev
  = do { new_evar <- newWantedEvVarNC loc new_pred
                     -- Not much point in seeking exact-match equality evidence
       ; let co = maybeSym swapped $
                  mkTcSymCo lhs_co
                  `mkTcTransCo` ctEvCoercion new_evar
                  `mkTcTransCo` rhs_co
       ; setEvBind evar (EvCoercion co)
       ; traceTcS "rewriteEqEvidence" (vcat [ppr old_ev, ppr nlhs, ppr nrhs, ppr co])
       ; return (ContinueWith new_evar) }

  | otherwise
  = panic "rewriteEvidence"
  where
    new_pred = mkTcEqPred nlhs nrhs

maybeSym :: SwapFlag -> TcCoercion -> TcCoercion
maybeSym IsSwapped  co = mkTcSymCo co
maybeSym NotSwapped co = co

matchFam :: TyCon -> [Type] -> TcS (Maybe (TcCoercion, TcType))
-- Given (F tys) return (ty, co), where co :: F tys ~ ty
matchFam tycon args
  | isOpenSynFamilyTyCon tycon
  = do { fam_envs <- getFamInstEnvs
       ; let mb_match = tcLookupFamInst fam_envs tycon args
       ; traceTcS "lookupFamInst" $
                  vcat [ ppr tycon <+> ppr args
                       , pprTvBndrs (varSetElems (tyVarsOfTypes args))
                       , ppr mb_match ]
       ; case mb_match of
           Nothing -> return Nothing
           Just (FamInstMatch { fim_instance = famInst
                              , fim_tys      = inst_tys })
             -> let co = mkTcUnbranchedAxInstCo Nominal (famInstAxiom famInst) inst_tys
                    ty = pSnd $ tcCoercionKind co
                in return $ Just (co, ty) }

  | Just ax <- isClosedSynFamilyTyCon_maybe tycon
  , Just (ind, inst_tys) <- chooseBranch ax args
  = let co = mkTcAxInstCo Nominal ax ind inst_tys
        ty = pSnd (tcCoercionKind co)
    in return $ Just (co, ty)

  | Just ops <- isBuiltInSynFamTyCon_maybe tycon =
    return $ do (r,ts,ty) <- sfMatchFam ops args
                return (mkTcAxiomRuleCo r ts [], ty)

  | otherwise
  = return Nothing

\end{code}

Note [Residual implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wl_implics in the WorkList are the residual implication
constraints that are generated while solving or canonicalising the
current worklist.  Specifically, when canonicalising
   (forall a. t1 ~ forall a. t2) 
from which we get the implication
   (forall a. t1 ~ t2)
See TcSMonad.deferTcSForAllEq


\begin{code}
-- Deferring forall equalities as implications
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

deferTcSForAllEq :: Role -- Nominal or Representational
                 -> CtLoc  -- Original wanted equality flavor
                 -> ([TyVar],TcType)   -- ForAll tvs1 body1
                 -> ([TyVar],TcType)   -- ForAll tvs2 body2
                 -> TcS EvTerm
-- Some of this functionality is repeated from TcUnify,
-- consider having a single place where we create fresh implications.
deferTcSForAllEq role loc (tvs1,body1) (tvs2,body2)
 = do { (subst1, skol_tvs) <- wrapTcS $ TcM.tcInstSkolTyVars tvs1
      ; let tys  = mkTyVarTys skol_tvs
            phi1 = Type.substTy subst1 body1
            phi2 = Type.substTy (zipTopTvSubst tvs2 tys) body2
            skol_info = UnifyForAllSkol skol_tvs phi1
            eq_pred   = case role of
                          Nominal ->          mkTcEqPred      phi1 phi2
                          Representational -> mkCoerciblePred phi1 phi2
                          Phantom ->          panic "deferTcSForAllEq Phantom"
        ; (ctev, freshness) <- newWantedEvVar loc eq_pred
        ; coe_inside <- case freshness of
            Cached -> return (ctEvCoercion ctev)
            Fresh  -> do { ev_binds_var <- newTcEvBinds
                         ; env <- wrapTcS $ TcM.getLclEnv
                         ; let ev_binds = TcEvBinds ev_binds_var
                               new_ct = mkNonCanonical ctev
                               new_co = ctEvCoercion ctev
                               new_untch = pushUntouchables (tcl_untch env)
                         ; let wc = WC { wc_flat  = singleCt new_ct
                                       , wc_impl  = emptyBag
                                       , wc_insol = emptyCts }
                               imp = Implic { ic_untch  = new_untch
                                            , ic_skols  = skol_tvs
                                            , ic_no_eqs = True
                                            , ic_given  = []
                                            , ic_wanted = wc
                                            , ic_insol  = False
                                            , ic_binds  = ev_binds_var
                                            , ic_env    = env
                                            , ic_info   = skol_info }
                         ; updWorkListTcS (extendWorkListImplic imp)
                         ; return (TcLetCo ev_binds new_co) }

        ; return $ EvCoercion (foldr mkTcForAllCo coe_inside skol_tvs) }
\end{code}

