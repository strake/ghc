{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Type subsumption and unification
module GHC.Tc.Utils.Unify (
  -- Full-blown subsumption
  tcWrapResult, tcWrapResultO, tcSkolemise, tcSkolemiseET,
  tcSubTypeHR, tcSubTypeO, tcSubType_NC, tcSubTypeDS,
  tcSubTypeDS_NC_O, tcSubTypePat,
  checkConstraints, checkTvConstraints,
  buildImplicationFor, emitResidualTvConstraint,

  -- Various unifications
  unifyType, unifyKind,
  uType, promoteTcType,
  swapOverTyVars, canSolveByUnification,

  --------------------------------
  -- Holes
  tcInfer,
  matchExpectedListTy,
  matchExpectedTyConApp,
  matchExpectedAppTy,
  matchExpectedFunTys,
  matchActualFunTys, matchActualFunTysPart,
  matchExpectedFunKind,

  metaTyVarUpdateOK, occCheckForErrors, MetaTyVarUpdateResult(..)

  ) where

import GHC.Prelude

import GHC.Hs
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr( debugPprType )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Types.Name( isSystemName )
import GHC.Tc.Utils.Instantiate
import GHC.Core.TyCon
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim( tYPE )
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Driver.Session
import GHC.Types.Basic
import GHC.Data.Bag
import GHC.Utils.Constants
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Maybe( isNothing )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( StateT (..) )

{-
************************************************************************
*                                                                      *
             matchExpected functions
*                                                                      *
************************************************************************

Note [Herald for matchExpectedFunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'herald' always looks like:
   "The equation(s) for 'f' have"
   "The abstraction (\x.e) takes"
   "The section (+ x) expects"
   "The function 'f' is applied to"

This is used to construct a message of form

   The abstraction `\Just 1 -> ...' takes two arguments
   but its type `Maybe a -> a' has only one

   The equation(s) for `f' have two arguments
   but its type `Maybe a -> a' has only one

   The section `(f 3)' requires 'f' to take two arguments
   but its type `Int -> Int' has only one

   The function 'f' is applied to two arguments
   but its type `Int -> Int' has only one

When visible type applications (e.g., `f @Int 1 2`, as in #13902) enter the
picture, we have a choice in deciding whether to count the type applications as
proper arguments:

   The function 'f' is applied to one visible type argument
     and two value arguments
   but its type `forall a. a -> a` has only one visible type argument
     and one value argument

Or whether to include the type applications as part of the herald itself:

   The expression 'f @Int' is applied to two arguments
   but its type `Int -> Int` has only one

The latter is easier to implement and is arguably easier to understand, so we
choose to implement that option.

Note [matchExpectedFunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~
matchExpectedFunTys checks that a sigma has the form
of an n-ary function.  It passes the decomposed type to the
thing_inside, and returns a wrapper to coerce between the two types

It's used wherever a language construct must have a functional type,
namely:
        A lambda expression
        A function definition
     An operator section

This function must be written CPS'd because it needs to fill in the
ExpTypes produced for arguments before it can fill in the ExpType
passed in.

-}

-- Use this one when you have an "expected" type.
matchExpectedFunTys :: forall a.
                       SDoc   -- See Note [Herald for matchExpectedFunTys]
                    -> Arity
                    -> ExpRhoType  -- deeply skolemised
                    -> ([ExpSigmaType] -> ExpRhoType -> TcM a)
                          -- must fill in these ExpTypes here
                    -> TcM (a, HsWrapper)
-- If    matchExpectedFunTys n ty = (_, wrap)
-- then  wrap : (t1 -> ... -> tn -> ty_r) ~> ty,
--   where [t1, ..., tn], ty_r are passed to the thing_inside
matchExpectedFunTys herald arity orig_ty thing_inside
  = case orig_ty of
      Check ty -> go [] arity ty
      _        -> defer [] arity orig_ty
  where
    go acc_arg_tys 0 ty
      = flip (,) idHsWrapper <$> thing_inside (reverse acc_arg_tys) (mkCheckExpType ty)

    go acc_arg_tys n ty
      | Just ty' <- tcView ty = go acc_arg_tys n ty'

    go acc_arg_tys n (FunTy { ft_af = af, ft_arg = arg_ty, ft_res = res_ty })
      = assert (af == VisArg) $
        go (mkCheckExpType arg_ty : acc_arg_tys) (n-1) res_ty <???> \ (result, wrap_res) ->
        (result, mkWpFun idHsWrapper wrap_res arg_ty res_ty doc)
      where
        doc = text "When inferring the argument type of a function with type" <+>
              quotes (ppr orig_ty)

    go acc_arg_tys n ty@(TyVarTy tv)
      | isMetaTyVar tv
      = readMetaTyVar tv >>= \ case
               Indirect ty' -> go acc_arg_tys n ty'
               Flexi        -> defer acc_arg_tys n (mkCheckExpType ty)

       -- In all other cases we bale out into ordinary unification
       -- However unlike the meta-tyvar case, we are sure that the
       -- number of arguments doesn't match arity of the original
       -- type, so we can add a bit more context to the error message
       -- (cf #7869).
       --
       -- It is not always an error, because specialized type may have
       -- different arity, for example:
       --
       -- > f1 = f2 'a'
       -- > f2 :: Monad m => m Bool
       -- > f2 = undefined
       --
       -- But in that case we add specialized type into error context
       -- anyway, because it may be useful. See also #9605.
    go acc_arg_tys n ty = addErrCtxtM (mk_ctxt acc_arg_tys ty) $
                          defer acc_arg_tys n (mkCheckExpType ty)

    ------------
    defer :: [ExpSigmaType] -> Arity -> ExpRhoType -> TcM (a, HsWrapper)
    defer acc_arg_tys n fun_ty
      = do { more_arg_tys <- replicateM n newInferExpType
           ; res_ty       <- newInferExpType
           ; result       <- thing_inside (reverse acc_arg_tys ++ more_arg_tys) res_ty
           ; more_arg_tys <- traverse readExpType more_arg_tys
           ; res_ty       <- readExpType res_ty
           ; let unif_fun_ty = mkVisFunTys more_arg_tys res_ty
           ; (,) result <$> tcSubTypeDS AppOrigin GenSigCtxt unif_fun_ty fun_ty
                         {- Not a good origin at all :-( -} }

    ------------
    mk_ctxt :: [ExpSigmaType] -> TcType -> StateT TidyEnv TcM MsgDoc
    mk_ctxt arg_tys res_ty
      = zonkTidyTcType (mkVisFunTys arg_tys' res_ty) <???> \ ty ->
        mk_fun_tys_msg herald ty arity
      where
        arg_tys' = checkingExpType "matchExpectedFunTys" <$> reverse arg_tys
            -- this is safe b/c we're called from "go"

-- Like 'matchExpectedFunTys', but used when you have an "actual" type,
-- for example in function application
matchActualFunTys :: SDoc   -- See Note [Herald for matchExpectedFunTys]
                  -> CtOrigin
                  -> Maybe (HsExpr GhcRn)   -- the thing with type TcSigmaType
                  -> Arity
                  -> TcSigmaType
                  -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
-- If    matchActualFunTys n ty = (wrap, [t1,..,tn], ty_r)
-- then  wrap : ty ~> (t1 -> ... -> tn -> ty_r)
matchActualFunTys herald ct_orig mb_thing n_val_args_wanted
  = matchActualFunTysPart herald ct_orig mb_thing
                          n_val_args_wanted []
                          n_val_args_wanted

-- | Variant of 'matchActualFunTys' that works when supplied only part
-- (that is, to the right of some arrows) of the full function type
matchActualFunTysPart
  :: SDoc -- See Note [Herald for matchExpectedFunTys]
  -> CtOrigin
  -> Maybe (HsExpr GhcRn)  -- The thing with type TcSigmaType
  -> Arity                 -- Total number of value args in the call
  -> [TcSigmaType]         -- Types of values args to which function has
                           --   been applied already (reversed)
  -> Arity                 -- Number of new value args wanted
  -> TcSigmaType           -- Type to analyse
  -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
-- See Note [matchActualFunTys error handling] for all these arguments
matchActualFunTysPart herald ct_orig mb_thing
                      n_val_args_in_call arg_tys_so_far
                      n_val_args_wanted fun_ty
  = go n_val_args_wanted arg_tys_so_far fun_ty
-- Does not allocate unnecessary meta variables: if the input already is
-- a function, we just take it apart.  Not only is this efficient,
-- it's important for higher rank: the argument might be of form
--              (forall a. ty) -> other
-- If allocated (fresh-meta-var1 -> fresh-meta-var2) and unified, we'd
-- hide the forall inside a meta-variable

-- (*) Sometimes it's necessary to call matchActualFunTys with only part
-- (that is, to the right of some arrows) of the type of the function in
-- question. (See GHC.Tc.Gen.Expr.tcArgs.) This argument is the reversed list of
-- arguments already seen (that is, not part of the TcSigmaType passed
-- in elsewhere).

  where
    -- This function has a bizarre mechanic: it accumulates arguments on
    -- the way down and also builds an argument list on the way up. Why:
    -- 1. The returns args list and the accumulated args list might be different.
    --    The accumulated args include all the arg types for the function,
    --    including those from before this function was called. The returned
    --    list should include only those arguments produced by this call of
    --    matchActualFunTys
    --
    -- 2. The HsWrapper can be built only on the way up. It seems (more)
    --    bizarre to build the HsWrapper but not the arg_tys.
    --
    -- Refactoring is welcome.
    go :: Arity
       -> [TcSigmaType] -- Types of value args to which the function has
                        -- been applied so far (reversed)
                        -- Used only for error messages
       -> TcSigmaType   -- the remainder of the type as we're processing
       -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
    go 0 _ ty = return (idHsWrapper, [], ty)

    go n so_far ty
      | not (null tvs && null theta) =
      [ (wrap2 <.> wrap1, arg_tys, res_ty)
      | (wrap1, rho) <- topInstantiate ct_orig ty
      , (wrap2, arg_tys, res_ty) <- go n so_far rho ]
      where
        (tvs, theta, _) = tcSplitSigmaTy ty

    go n so_far ty
      | Just ty' <- tcView ty = go n so_far ty'

    go n so_far (FunTy { ft_af = af, ft_arg = arg_ty, ft_res = res_ty })
      = assert (af == VisArg)
        go (n-1) (arg_ty:so_far) res_ty <???> \ (wrap_res, tys, ty_r) ->
        ( mkWpFun idHsWrapper wrap_res arg_ty ty_r doc
                    , arg_ty : tys, ty_r )
      where
        doc = text "When inferring the argument type of a function with type" <+>
              quotes (ppr fun_ty)

    go n so_far ty@(TyVarTy tv)
      | isMetaTyVar tv
      = readMetaTyVar tv >>= \ case
               Indirect ty' -> go n so_far ty'
               Flexi        -> defer n ty

       -- In all other cases we bale out into ordinary unification
       -- However unlike the meta-tyvar case, we are sure that the
       -- number of arguments doesn't match arity of the original
       -- type, so we can add a bit more context to the error message
       -- (cf #7869).
       --
       -- It is not always an error, because specialized type may have
       -- different arity, for example:
       --
       -- > f1 = f2 'a'
       -- > f2 :: Monad m => m Bool
       -- > f2 = undefined
       --
       -- But in that case we add specialized type into error context
       -- anyway, because it may be useful. See also #9605.
    go n so_far ty = addErrCtxtM (mk_ctxt so_far ty) (defer n ty)

    ------------
    defer n fun_ty =
      [ (mkWpCastN co, arg_tys, res_ty)
      | arg_tys <- replicateM n newOpenFlexiTyVarTy
      , res_ty  <- newOpenFlexiTyVarTy
      , let unif_fun_ty = mkVisFunTys arg_tys res_ty
      , co <- unifyType mb_thing fun_ty unif_fun_ty ]

    ------------
    mk_ctxt :: [TcType] -> TcType -> StateT TidyEnv TcM MsgDoc
    mk_ctxt arg_tys res_ty
      = zonkTidyTcType (mkVisFunTys (reverse arg_tys) res_ty) <???> \ ty ->
        mk_fun_tys_msg herald ty n_val_args_in_call

mk_fun_tys_msg :: SDoc -> TcType -> Arity -> SDoc
mk_fun_tys_msg herald ty n_args_in_call
  | n_args_in_call <= n_fun_args  -- Enough args, in the end
  = text "In the result of a function call"
  | otherwise
  = hang (herald <+> speakNOf n_args_in_call (text "value argument") <> comma)
       2 (sep [ text "but its type" <+> quotes (pprType ty)
              , if n_fun_args == 0 then text "has none"
                else text "has only" <+> speakN n_fun_args])
  where
    (args, _) = tcSplitFunTys ty
    n_fun_args = length args

{- Note [matchActualFunTys error handling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
matchActualFunTysPart is made much more complicated by the
desire to produce good error messages. Consider the application
    f @Int x y
In GHC.Tc.Gen.Expr.tcArgs we deal with visible type arguments,
and then call matchActualFunTysPart for each individual value
argument. It, in turn, must instantiate any type/dictionary args,
before looking for an arrow type.

But if it doesn't find an arrow type, it wants to generate a message
like "f is applied to two arguments but its type only has one".
To do that, it needs to konw about the args that tcArgs has already
munched up -- hence passing in n_val_args_in_call and arg_tys_so_far;
and hence also the accumulating so_far arg to 'go'.

This allows us (in mk_ctxt) to construct f's /instantiated/ type,
with just the values-arg arrows, which is what we really want
in the error message.

Ugh!
-}

----------------------
matchExpectedListTy :: TcRhoType -> TcM (TcCoercionN, TcRhoType)
-- Special case for lists
matchExpectedListTy exp_ty
 = [(co, elt_ty) | (co, [elt_ty]) <- matchExpectedTyConApp listTyCon exp_ty]

---------------------
matchExpectedTyConApp :: TyCon                -- T :: forall kv1 ... kvm. k1 -> ... -> kn -> *
                      -> TcRhoType            -- orig_ty
                      -> TcM (TcCoercionN,    -- T k1 k2 k3 a b c ~N orig_ty
                              [TcSigmaType])  -- Element types, k1 k2 k3 a b c

-- It's used for wired-in tycons, so we call checkWiredInTyCon
-- Precondition: never called with FunTyCon
-- Precondition: input type :: *
-- Postcondition: (T k1 k2 k3 a b c) is well-kinded

matchExpectedTyConApp tc orig_ty
  = assert (tc /= funTyCon) $ go orig_ty
  where
    go ty
       | Just ty' <- tcView ty
       = go ty'

    go ty@(TyConApp tycon args)
       | tc == tycon  -- Common case
       = return (mkTcNomReflCo ty, args)

    go (TyVarTy tv)
       | isMetaTyVar tv
       = readMetaTyVar tv >>= \ case
                Indirect ty -> go ty
                Flexi       -> defer

    go _ = defer

    -- If the common case does not occur, instantiate a template
    -- T k1 .. kn t1 .. tm, and unify with the original type
    -- Doing it this way ensures that the types we return are
    -- kind-compatible with T.  For example, suppose we have
    --       matchExpectedTyConApp T (f Maybe)
    -- where data T a = MkT a
    -- Then we don't want to instantiate T's data constructors with
    --    (a::*) ~ Maybe
    -- because that'll make types that are utterly ill-kinded.
    -- This happened in #7368
    defer
      = do { (_, arg_tvs) <- newMetaTyVars (tyConTyVars tc)
           ; traceTc "matchExpectedTyConApp" (ppr tc $$ ppr (tyConTyVars tc) $$ ppr arg_tvs)
           ; let args = mkTyVarTys arg_tvs
                 tc_template = mkTyConApp tc args
           ; flip (,) args <$> unifyType Nothing tc_template orig_ty }

----------------------
matchExpectedAppTy :: TcRhoType                         -- orig_ty
                   -> TcM (TcCoercion,                   -- m a ~N orig_ty
                           (TcSigmaType, TcSigmaType))  -- Returns m, a
-- If the incoming type is a mutable type variable of kind k, then
-- matchExpectedAppTy returns a new type variable (m: * -> k); note the *.

matchExpectedAppTy orig_ty
  = go orig_ty
  where
    go ty
      | Just ty' <- tcView ty = go ty'

      | Just (fun_ty, arg_ty) <- tcSplitAppTy_maybe ty
      = return (mkTcNomReflCo orig_ty, (fun_ty, arg_ty))

    go (TyVarTy tv)
      | isMetaTyVar tv
      = readMetaTyVar tv >>= \ case
               Indirect ty -> go ty
               Flexi       -> defer

    go _ = defer

    -- Defer splitting by generating an equality constraint
    defer
      = do { ty1 <- newFlexiTyVarTy kind1
           ; ty2 <- newFlexiTyVarTy kind2
           ; flip (,) (ty1, ty2) <$> unifyType Nothing (mkAppTy ty1 ty2) orig_ty }

    orig_kind = tcTypeKind orig_ty
    kind1 = mkVisFunTy liftedTypeKind orig_kind
    kind2 = liftedTypeKind    -- m :: * -> k
                              -- arg type :: *

{-
************************************************************************
*                                                                      *
                Subsumption checking
*                                                                      *
************************************************************************

Note [Subsumption checking: tcSubType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All the tcSubType calls have the form
                tcSubType actual_ty expected_ty
which checks
                actual_ty <= expected_ty

That is, that a value of type actual_ty is acceptable in
a place expecting a value of type expected_ty.  I.e. that

    actual ty   is more polymorphic than   expected_ty

It returns a coercion function
        co_fn :: actual_ty ~ expected_ty
which takes an HsExpr of type actual_ty into one of type
expected_ty.

These functions do not actually check for subsumption. They check if
expected_ty is an appropriate annotation to use for something of type
actual_ty. This difference matters when thinking about visible type
application. For example,

   forall a. a -> forall b. b -> b
      DOES NOT SUBSUME
   forall a b. a -> b -> b

because the type arguments appear in a different order. (Neither does
it work the other way around.) BUT, these types are appropriate annotations
for one another. Because the user directs annotations, it's OK if some
arguments shuffle around -- after all, it's what the user wants.
Bottom line: none of this changes with visible type application.

There are a number of wrinkles (below).

Notice that Wrinkle 1 and 2 both require eta-expansion, which technically
may increase termination.  We just put up with this, in exchange for getting
more predictable type inference.

Wrinkle 1: Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want   (forall a. Int -> a -> a)  <=  (Int -> forall a. a->a)
(see section 4.6 of "Practical type inference for higher rank types")
So we must deeply-skolemise the RHS before we instantiate the LHS.

That is why tc_sub_type starts with a call to tcSkolemise (which does the
deep skolemisation), and then calls the DS variant (which assumes
that expected_ty is deeply skolemised)

Wrinkle 2: Note [Co/contra-variance of subsumption checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider  g :: (Int -> Int) -> Int
  f1 :: (forall a. a -> a) -> Int
  f1 = g

  f2 :: (forall a. a -> a) -> Int
  f2 x = g x
f2 will typecheck, and it would be odd/fragile if f1 did not.
But f1 will only typecheck if we have that
    (Int->Int) -> Int  <=  (forall a. a->a) -> Int
And that is only true if we do the full co/contravariant thing
in the subsumption check.  That happens in the FunTy case of
tcSubTypeDS_NC_O, and is the sole reason for the WpFun form of
HsWrapper.

Another powerful reason for doing this co/contra stuff is visible
in #9569, involving instantiation of constraint variables,
and again involving eta-expansion.

Wrinkle 3: Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider tc150:
  f y = \ (x::forall a. a->a). blah
The following happens:
* We will infer the type of the RHS, ie with a res_ty = alpha.
* Then the lambda will split  alpha := beta -> gamma.
* And then we'll check tcSubType IsSwapped beta (forall a. a->a)

So it's important that we unify beta := forall a. a->a, rather than
skolemising the type.
-}


-- | Call this variant when you are in a higher-rank situation and
-- you know the right-hand type is deeply skolemised.
tcSubTypeHR :: CtOrigin               -- ^ of the actual type
            -> Maybe (HsExpr GhcRn)   -- ^ If present, it has type ty_actual
            -> TcSigmaType -> ExpRhoType -> TcM HsWrapper
tcSubTypeHR orig = tcSubTypeDS_NC_O orig GenSigCtxt

------------------------
tcSubTypePat :: CtOrigin -> UserTypeCtxt
            -> ExpSigmaType -> TcSigmaType -> TcM HsWrapper
-- If wrap = tc_sub_type_et t1 t2
--    => wrap :: t1 ~> t2
tcSubTypePat orig ctxt (Check ty_actual) ty_expected
  = tc_sub_tc_type eq_orig orig ctxt ty_actual ty_expected
  where
    eq_orig = TypeEqOrigin { uo_actual   = ty_expected
                           , uo_expected = ty_actual
                           , uo_thing    = Nothing
                           , uo_visible  = True }

tcSubTypePat _ _ (Infer inf_res) ty_expected
  = mkWpCastN . mkTcSymCo <$> fillInferResult ty_expected inf_res
               -- In patterns we do not instantatiate

------------------------
tcSubTypeO :: CtOrigin      -- ^ of the actual type
           -> UserTypeCtxt  -- ^ of the expected type
           -> TcSigmaType
           -> ExpRhoType
           -> TcM HsWrapper
tcSubTypeO orig ctxt ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubTypeDS_O" (vcat [ pprCtOrigin orig
                                       , pprUserTypeCtxt ctxt
                                       , ppr ty_actual
                                       , ppr ty_expected ])
       ; tcSubTypeDS_NC_O orig ctxt Nothing ty_actual ty_expected }

addSubTypeCtxt :: TcType -> ExpType -> TcM a -> TcM a
addSubTypeCtxt ty_actual ty_expected
 | isRhoTy ty_actual        -- If there is no polymorphism involved, the
 , isRhoExpTy ty_expected   -- TypeEqOrigin stuff (added by the _NC functions)
 = id             -- gives enough context by itself
 | otherwise
 = addErrCtxtM mk_msg
  where
    mk_msg =
      [ msg
      | ty_actual   <- zonkTidyTcType ty_actual
        -- might not be filled if we're debugging. ugh.
      , ty_expected <- lift (readExpType_maybe ty_expected) >>= \ case
            Just ty -> mkCheckExpType <$> zonkTidyTcType ty
            Nothing -> pure ty_expected
      , ty_expected <- lift (readExpType ty_expected)
      , ty_expected <- zonkTidyTcType ty_expected
      , let msg = vcat
              [ hang (text "When checking that:") 4 (ppr ty_actual)
              , nest 2 (hang (text "is more polymorphic than:") 2 (ppr ty_expected)) ] ]

---------------
-- The "_NC" variants do not add a typechecker-error context;
-- the caller is assumed to do that

tcSubType_NC :: UserTypeCtxt -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- Checks that actual <= expected
-- Returns HsWrapper :: actual ~ expected
tcSubType_NC ctxt ty_actual ty_expected
  = do { traceTc "tcSubType_NC" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tc_sub_tc_type origin origin ctxt ty_actual ty_expected }
  where
    origin = TypeEqOrigin { uo_actual   = ty_actual
                          , uo_expected = ty_expected
                          , uo_thing    = Nothing
                          , uo_visible  = True }

tcSubTypeDS :: CtOrigin -> UserTypeCtxt -> TcSigmaType -> ExpRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised (hence "DS")
tcSubTypeDS orig ctxt ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubTypeDS_NC" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tcSubTypeDS_NC_O orig ctxt Nothing ty_actual ty_expected }

tcSubTypeDS_NC_O :: CtOrigin   -- origin used for instantiation only
                 -> UserTypeCtxt
                 -> Maybe (HsExpr GhcRn)
                 -> TcSigmaType -> ExpRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised
tcSubTypeDS_NC_O inst_orig ctxt m_thing ty_actual ty_expected
  = case ty_expected of
      Infer inf_res -> instantiateAndFillInferResult inst_orig ty_actual inf_res
      Check ty      -> tc_sub_type_ds eq_orig inst_orig ctxt ty_actual ty
         where
           eq_orig = TypeEqOrigin { uo_actual = ty_actual, uo_expected = ty
                                  , uo_thing  = ppr <$> m_thing
                                  , uo_visible = True }

---------------
tc_sub_tc_type :: CtOrigin   -- used when calling uType
               -> CtOrigin   -- used when instantiating
               -> UserTypeCtxt -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- If wrap = tc_sub_type t1 t2
--    => wrap :: t1 ~> t2
tc_sub_tc_type eq_orig inst_orig ctxt ty_actual ty_expected
  | definitely_poly ty_expected      -- See Note [Don't skolemise unnecessarily]
  , not (possibly_poly ty_actual)
  = do { traceTc "tc_sub_tc_type (drop to equality)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; mkWpCastN <$>
         uType TypeLevel eq_orig ty_actual ty_expected }

  | otherwise   -- This is the general case
  = do { traceTc "tc_sub_tc_type (general case)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; uncurry (<.>) <$> tcSkolemise ctxt ty_expected \ _ sk_rho ->
                                  tc_sub_type_ds eq_orig inst_orig ctxt
                                                 ty_actual sk_rho }
  where
    possibly_poly ty
      | isForAllTy ty                        = True
      | Just (_, res) <- splitFunTy_maybe ty = possibly_poly res
      | otherwise                            = False
      -- NB *not* tcSplitFunTy, because here we want
      -- to decompose type-class arguments too

    definitely_poly ty
      | (tvs, theta, tau) <- tcSplitSigmaTy ty
      , (tv:_) <- tvs
      , null theta
      , isInsolubleOccursCheck NomEq tv tau
      = True
      | otherwise
      = False

{- Note [Don't skolemise unnecessarily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are trying to solve
    (Char->Char) <= (forall a. a->a)
We could skolemise the 'forall a', and then complain
that (Char ~ a) is insoluble; but that's a pretty obscure
error.  It's better to say that
    (Char->Char) ~ (forall a. a->a)
fails.

So roughly:
 * if the ty_expected has an outermost forall
      (i.e. skolemisation is the next thing we'd do)
 * and the ty_actual has no top-level polymorphism (but looking deeply)
then we can revert to simple equality.  But we need to be careful.
These examples are all fine:

 * (Char -> forall a. a->a) <= (forall a. Char -> a -> a)
      Polymorphism is buried in ty_actual

 * (Char->Char) <= (forall a. Char -> Char)
      ty_expected isn't really polymorphic

 * (Char->Char) <= (forall a. (a~Char) => a -> a)
      ty_expected isn't really polymorphic

 * (Char->Char) <= (forall a. F [a] Char -> Char)
                   where type instance F [x] t = t
     ty_expected isn't really polymorphic

If we prematurely go to equality we'll reject a program we should
accept (e.g. #13752).  So the test (which is only to improve
error message) is very conservative:
 * ty_actual is /definitely/ monomorphic
 * ty_expected is /definitely/ polymorphic
-}

---------------
tc_sub_type_ds :: CtOrigin    -- used when calling uType
               -> CtOrigin    -- used when instantiating
               -> UserTypeCtxt -> TcSigmaType -> TcRhoType -> TcM HsWrapper
-- If wrap = tc_sub_type_ds t1 t2
--    => wrap :: t1 ~> t2
-- Here is where the work actually happens!
-- Precondition: ty_expected is deeply skolemised
tc_sub_type_ds eq_orig inst_orig ctxt ty_actual ty_expected
  = do { traceTc "tc_sub_type_ds" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; go ty_actual ty_expected }
  where
    go ty_a ty_e | Just ty_a' <- tcView ty_a = go ty_a' ty_e
                 | Just ty_e' <- tcView ty_e = go ty_a  ty_e'

    go (TyVarTy tv_a) ty_e
      = lookupTcTyVar tv_a >>= \ case
               Filled ty_a' ->
                 do { traceTc "tcSubTypeDS_NC_O following filled act meta-tyvar:"
                        (ppr tv_a <+> text "-->" <+> ppr ty_a')
                    ; tc_sub_type_ds eq_orig inst_orig ctxt ty_a' ty_e }
               Unfilled _   -> unify

    -- Historical note (Sept 16): there was a case here for
    --    go ty_a (TyVarTy alpha)
    -- which, in the impredicative case unified  alpha := ty_a
    -- where th_a is a polytype.  Not only is this probably bogus (we
    -- simply do not have decent story for impredicative types), but it
    -- caused #12616 because (also bizarrely) 'deriving' code had
    -- -XImpredicativeTypes on.  I deleted the entire case.

    go (FunTy { ft_af = VisArg, ft_arg = act_arg, ft_res = act_res })
       (FunTy { ft_af = VisArg, ft_arg = exp_arg, ft_res = exp_res })
      = -- See Note [Co/contra-variance of subsumption checking]
        do { res_wrap <- tc_sub_type_ds eq_orig inst_orig  ctxt       act_res exp_res
           ; arg_wrap <- tc_sub_tc_type eq_orig given_orig GenSigCtxt exp_arg act_arg
                         -- GenSigCtxt: See Note [Setting the argument context]
           ; return (mkWpFun arg_wrap res_wrap exp_arg exp_res doc) }
               -- arg_wrap :: exp_arg ~> act_arg
               -- res_wrap :: act-res ~> exp_res
      where
        given_orig = GivenOrigin (SigSkol GenSigCtxt exp_arg [])
        doc = text "When checking that" <+> quotes (ppr ty_actual) <+>
              text "is more polymorphic than" <+> quotes (ppr ty_expected)

    go ty_a ty_e
      | let (tvs, theta, _) = tcSplitSigmaTy ty_a
      , not (null tvs && null theta) =
      [ body_wrap <.> in_wrap
      | (in_wrap, in_rho) <- topInstantiate inst_orig ty_a
      , body_wrap <- tc_sub_type_ds
                            (eq_orig { uo_actual = in_rho
                                     , uo_expected = ty_expected })
                            inst_orig ctxt in_rho ty_e ]

      | otherwise   -- Revert to unification
      = inst_and_unify
         -- It's still possible that ty_actual has nested foralls. Instantiate
         -- these, as there's no way unification will succeed with them in.
         -- See typecheck/should_compile/T11305 for an example of when this
         -- is important. The problem is that we're checking something like
         --  a -> forall b. b -> b     <=   alpha beta gamma
         -- where we end up with alpha := (->)

    inst_and_unify =
      [ mkWpCastN cow <.> wrap
      | (wrap, rho_a) <- deeplyInstantiate inst_orig ty_actual

                           -- If we haven't recurred through an arrow, then
                           -- the eq_orig will list ty_actual. In this case,
                           -- we want to update the origin to reflect the
                           -- instantiation. If we *have* recurred through
                           -- an arrow, it's better not to update.
      , let eq_orig' = case eq_orig of
                                TypeEqOrigin { uo_actual   = orig_ty_actual }
                                  |  orig_ty_actual `tcEqType` ty_actual
                                  ,  not (isIdHsWrapper wrap)
                                  -> eq_orig { uo_actual = rho_a }
                                _ -> eq_orig

      , cow <- uType TypeLevel eq_orig' rho_a ty_expected ]


     -- use versions without synonyms expanded
    unify = mkWpCastN <$> uType TypeLevel eq_orig ty_actual ty_expected

{- Note [Settting the argument context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider we are doing the ambiguity check for the (bogus)
  f :: (forall a b. C b => a -> a) -> Int

We'll call
   tcSubType ((forall a b. C b => a->a) -> Int )
             ((forall a b. C b => a->a) -> Int )

with a UserTypeCtxt of (FunSigCtxt "f").  Then we'll do the co/contra thing
on the argument type of the (->) -- and at that point we want to switch
to a UserTypeCtxt of GenSigCtxt.  Why?

* Error messages.  If we stick with FunSigCtxt we get errors like
     * Could not deduce: C b
       from the context: C b0
        bound by the type signature for:
            f :: forall a b. C b => a->a
  But of course f does not have that type signature!
  Example tests: T10508, T7220a, Simple14

* Implications. We may decide to build an implication for the whole
  ambiguity check, but we don't need one for each level within it,
  and GHC.Tc.Utils.Unify.alwaysBuildImplication checks the UserTypeCtxt.
  See Note [When to build an implication]
-}

-----------------
-- needs both un-type-checked (for origins) and type-checked (for wrapping)
-- expressions
tcWrapResult :: HsExpr GhcRn -> HsExpr GhcTc -> TcSigmaType -> ExpRhoType
             -> TcM (HsExpr GhcTc)
tcWrapResult rn_expr = tcWrapResultO (exprCtOrigin rn_expr) rn_expr

-- | Sometimes we don't have a @HsExpr Name@ to hand, and this is more
-- convenient.
tcWrapResultO :: CtOrigin -> HsExpr GhcRn -> HsExpr GhcTc -> TcSigmaType -> ExpRhoType
               -> TcM (HsExpr GhcTc)
tcWrapResultO orig rn_expr expr actual_ty res_ty =
  [ mkHsWrap cow expr
  | () <- traceTc "tcWrapResult" (vcat [ text "Actual:  " <+> ppr actual_ty
                                      , text "Expected:" <+> ppr res_ty ])
  , cow <- tcSubTypeDS_NC_O orig GenSigCtxt
                                 (Just rn_expr) actual_ty res_ty ]


{- **********************************************************************
%*                                                                      *
            ExpType functions: tcInfer, instantiateAndFillInferResult
%*                                                                      *
%********************************************************************* -}

-- | Infer a type using a fresh ExpType
-- See also Note [ExpType] in "GHC.Tc.Utils.TcMType"
tcInfer :: (ExpSigmaType -> TcM a) -> TcM (a, TcSigmaType)
tcInfer tc_check
  = do { res_ty <- newInferExpType
       ; (,) <$> tc_check res_ty <*> readExpType res_ty }

instantiateAndFillInferResult :: CtOrigin -> TcType -> InferResult -> TcM HsWrapper
-- If wrap = instantiateAndFillInferResult t1 t2
--    => wrap :: t1 ~> t2
-- See Note [Instantiation of InferResult]
instantiateAndFillInferResult orig ty inf_res =
  [ mkWpCastN co <.> wrap
  | (wrap, rho) <- deeplyInstantiate orig ty
  , co <- fillInferResult rho inf_res ]

fillInferResult :: TcType -> InferResult -> TcM TcCoercionN
-- If wrap = fillInferResult t1 t2
--    => wrap :: t1 ~> t2
fillInferResult orig_ty (IR { ir_uniq = u, ir_lvl = res_lvl
                            , ir_ref = ref })
  = do { (ty_co, ty_to_fill_with) <- promoteTcType res_lvl orig_ty

       ; traceTc "Filling ExpType" $
         ppr u <+> text ":=" <+> ppr ty_to_fill_with

       ; when debugIsOn (check_hole ty_to_fill_with)

       ; ty_co <$ writeMutVar ref (Just ty_to_fill_with) }
  where
    check_hole ty   -- Debug check only
      = do { let ty_lvl = tcTypeLevel ty
           ; massertPpr (not (ty_lvl `strictlyDeeperThan` res_lvl))
                      (ppr u $$ ppr res_lvl $$ ppr ty_lvl $$
                       ppr ty <+> dcolon <+> ppr (tcTypeKind ty) $$ ppr orig_ty)
           ; cts <- readMutVar ref
           ; for_ cts \ already_there ->
               pprPanic "writeExpType" (vcat [ ppr u
                                             , ppr ty
                                             , ppr already_there ]) }

{- Note [Instantiation of InferResult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We now always instantiate before filling in InferResult, so that
the result is a TcRhoType: see #17173 for discussion.

For example:

1. Consider
    f x = (*)
   We want to instantiate the type of (*) before returning, else we
   will infer the type
     f :: forall {a}. a -> forall b. Num b => b -> b -> b
   This is surely confusing for users.

   And worse, the monomorphism restriction won't work properly. The MR is
   dealt with in simplifyInfer, and simplifyInfer has no way of
   instantiating. This could perhaps be worked around, but it may be
   hard to know even when instantiation should happen.

2. Another reason.  Consider
       f :: (?x :: Int) => a -> a
       g y = let ?x = 3::Int in f
   Here want to instantiate f's type so that the ?x::Int constraint
   gets discharged by the enclosing implicit-parameter binding.

3. Suppose one defines plus = (+). If we instantiate lazily, we will
   infer plus :: forall a. Num a => a -> a -> a. However, the monomorphism
   restriction compels us to infer
      plus :: Integer -> Integer -> Integer
   (or similar monotype). Indeed, the only way to know whether to apply
   the monomorphism restriction at all is to instantiate

There is one place where we don't want to instantiate eagerly,
namely in GHC.Tc.Module.tcRnExpr, which implements GHCi's :type
command. See Note [Implementing :type] in GHC.Tc.Module.

-}

{- *********************************************************************
*                                                                      *
              Promoting types
*                                                                      *
********************************************************************* -}

promoteTcType :: TcLevel -> TcType -> TcM (TcCoercion, TcType)
-- See Note [Promoting a type]
-- promoteTcType level ty = (co, ty')
--   * Returns ty'  whose max level is just 'level'
--             and  whose kind is ~# to the kind of 'ty'
--             and  whose kind has form TYPE rr
--   * and co :: ty ~ ty'
--   * and emits constraints to justify the coercion
promoteTcType dest_lvl ty
  = do { cur_lvl <- getTcLevel
       ; bool promote_it dont_promote_it (cur_lvl `sameDepthAs` dest_lvl) }
  where
    promote_it :: TcM (TcCoercion, TcType)
    promote_it  -- Emit a constraint  (alpha :: TYPE rr) ~ ty
                -- where alpha and rr are fresh and from level dest_lvl
      = do { rr      <- newMetaTyVarTyAtLevel dest_lvl runtimeRepTy
           ; prom_ty <- newMetaTyVarTyAtLevel dest_lvl (tYPE rr)
           ; let eq_orig = TypeEqOrigin { uo_actual   = ty
                                        , uo_expected = prom_ty
                                        , uo_thing    = Nothing
                                        , uo_visible  = False }

           ; flip (,) prom_ty <$> emitWantedEq eq_orig TypeLevel Nominal ty prom_ty }

    dont_promote_it :: TcM (TcCoercion, TcType)
    dont_promote_it = -- Check that ty :: TYPE rr, for some (fresh) rr
      [ (co, ty `mkCastTy` ki_co)
      | res_kind <- newOpenTypeKind
      , let ty_kind = tcTypeKind ty
            kind_orig = TypeEqOrigin
              { uo_actual   = ty_kind
              , uo_expected = res_kind
              , uo_thing    = Nothing
              , uo_visible  = False }
      , ki_co <- uType KindLevel kind_orig (tcTypeKind ty) res_kind
      , let co = mkTcGReflRightCo Nominal ty ki_co ]

{- Note [Promoting a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12427)

  data T where
    MkT :: (Int -> Int) -> a -> T

  h y = case y of MkT v w -> v

We'll infer the RHS type with an expected type ExpType of
  (IR { ir_lvl = l, ir_ref = ref, ... )
where 'l' is the TcLevel of the RHS of 'h'.  Then the MkT pattern
match will increase the level, so we'll end up in tcSubType, trying to
unify the type of v,
  v :: Int -> Int
with the expected type.  But this attempt takes place at level (l+1),
rightly so, since v's type could have mentioned existential variables,
(like w's does) and we want to catch that.

So we
  - create a new meta-var alpha[l+1]
  - fill in the InferRes ref cell 'ref' with alpha
  - emit an equality constraint, thus
        [W] alpha[l+1] ~ (Int -> Int)

That constraint will float outwards, as it should, unless v's
type mentions a skolem-captured variable.

This approach fails if v has a higher rank type; see
Note [Promotion and higher rank types]


Note [Promotion and higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If v had a higher-rank type, say v :: (forall a. a->a) -> Int,
then we'd emit an equality
        [W] alpha[l+1] ~ ((forall a. a->a) -> Int)
which will sadly fail because we can't unify a unification variable
with a polytype.  But there is nothing really wrong with the program
here.

We could just about solve this by "promote the type" of v, to expose
its polymorphic "shape" while still leaving constraints that will
prevent existential escape.  But we must be careful!  Exposing
the "shape" of the type is precisely what we must NOT do under
a GADT pattern match!  So in this case we might promote the type
to
        (forall a. a->a) -> alpha[l+1]
and emit the constraint
        [W] alpha[l+1] ~ Int
Now the promoted type can fill the ref cell, while the emitted
equality can float or not, according to the usual rules.

But that's not quite right!  We are exposing the arrow! We could
deal with that too:
        (forall a. mu[l+1] a a) -> alpha[l+1]
with constraints
        [W] alpha[l+1] ~ Int
        [W] mu[l+1] ~ (->)
Here we abstract over the '->' inside the forall, in case that
is subject to an equality constraint from a GADT match.

Note that we kept the outer (->) because that's part of
the polymorphic "shape".  And because of impredicativity,
GADT matches can't give equalities that affect polymorphic
shape.

This reasoning just seems too complicated, so I decided not
to do it.  These higher-rank notes are just here to record
the thinking.
-}

{- *********************************************************************
*                                                                      *
                    Generalisation
*                                                                      *
********************************************************************* -}

-- | Take an "expected type" and strip off quantifiers to expose the
-- type underneath, binding the new skolems for the @thing_inside@.
-- The returned 'HsWrapper' has type @specific_ty -> expected_ty@.
tcSkolemise :: UserTypeCtxt -> TcSigmaType
            -> ([TcTyVar] -> TcType -> TcM result)
         -- ^ These are only ever used for scoped type variables.
            -> TcM (HsWrapper, result)
        -- ^ The expression has type: spec_ty -> expected_ty

tcSkolemise ctxt expected_ty thing_inside
   -- We expect expected_ty to be a forall-type
   -- If not, the call is a no-op
  = do  { traceTc "tcSkolemise" mempty
        ; (wrap, tv_prs, given, rho') <- deeplySkolemise expected_ty

        ; lvl <- getTcLevel
        ; when debugIsOn $
              traceTc "tcSkolemise" $ vcat [
                ppr lvl,
                text "expected_ty" <+> ppr expected_ty,
                text "inst tyvars" <+> ppr tv_prs,
                text "given"       <+> ppr given,
                text "inst type"   <+> ppr rho' ]

        -- Generally we must check that the "forall_tvs" haven't been constrained
        -- The interesting bit here is that we must include the free variables
        -- of the expected_ty.  Here's an example:
        --       runST (newVar True)
        -- Here, if we don't make a check, we'll get a type (ST s (MutVar s Bool))
        -- for (newVar True), with s fresh.  Then we unify with the runST's arg type
        -- forall s'. ST s' a. That unifies s' with s, and a with MutVar s Bool.
        -- So now s' isn't unconstrained because it's linked to a.
        --
        -- However [Oct 10] now that the untouchables are a range of
        -- TcTyVars, all this is handled automatically with no need for
        -- extra faffing around

        ; let tvs' = snd <$> tv_prs
              skol_info = SigSkol ctxt expected_ty tv_prs

        ; (ev_binds, result) <- checkConstraints skol_info tvs' given $
                                thing_inside tvs' rho'

        ; return (wrap <.> mkWpLet ev_binds, result) }
          -- The ev_binds returned by checkConstraints is very
          -- often empty, in which case mkWpLet is a no-op

-- | Variant of 'tcSkolemise' that takes an ExpType
tcSkolemiseET :: UserTypeCtxt -> ExpSigmaType
              -> (ExpRhoType -> TcM result)
              -> TcM (HsWrapper, result)
tcSkolemiseET _ et@(Infer {}) thing_inside
  = (idHsWrapper, ) <$> thing_inside et
tcSkolemiseET ctxt (Check ty) thing_inside
  = tcSkolemise ctxt ty $ \_ -> thing_inside . mkCheckExpType

checkConstraints :: SkolemInfo
                 -> [TcTyVar]           -- Skolems
                 -> [EvVar]             -- Given
                 -> TcM result
                 -> TcM (TcEvBinds, result)

checkConstraints skol_info skol_tvs given thing_inside
  = do { implication_needed <- implicationNeeded skol_info skol_tvs given

       ; if implication_needed
         then do { (tclvl, wanted, result) <- pushLevelAndCaptureConstraints thing_inside
                 ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info skol_tvs given wanted
                 ; traceTc "checkConstraints" (ppr tclvl $$ ppr skol_tvs)
                 ; (ev_binds, result) <$ emitImplications implics }

         else -- Fast path.  We check every function argument with tcCheckExpr,
              -- which uses tcSkolemise and hence checkConstraints.
              -- So this fast path is well-exercised
              (,) emptyTcEvBinds <$> thing_inside }

checkTvConstraints :: SkolemInfo
                   -> [TcTyVar]          -- Skolem tyvars
                   -> TcM result
                   -> TcM result

checkTvConstraints skol_info skol_tvs thing_inside
  = do { (tclvl, wanted, result) <- pushLevelAndCaptureConstraints thing_inside
       ; result <$ emitResidualTvConstraint skol_info Nothing skol_tvs tclvl wanted }

emitResidualTvConstraint :: SkolemInfo -> Maybe SDoc -> [TcTyVar]
                         -> TcLevel -> WantedConstraints -> TcM ()
emitResidualTvConstraint skol_info m_telescope skol_tvs tclvl wanted
  | isEmptyWC wanted
  , isNothing m_telescope || skol_tvs `lengthAtMost` 1
    -- If m_telescope is (Just d), we must do the bad-telescope check,
    -- so we must /not/ discard the implication even if there are no
    -- wanted constraints. See Note [Checking telescopes] in GHC.Tc.Types.Constraint.
    -- Lacking this check led to #16247
  = return ()

  | otherwise
  = do { ev_binds <- newNoTcEvBinds
       ; implic   <- newImplication
       ; let status | insolubleWC wanted = IC_Insoluble
                    | otherwise          = IC_Unsolved
             -- If the inner constraints are insoluble,
             -- we should mark the outer one similarly,
             -- so that insolubleWC works on the outer one

       ; emitImplication $
         implic { ic_status    = status
                , ic_tclvl     = tclvl
                , ic_skols     = skol_tvs
                , ic_no_eqs    = True
                , ic_telescope = m_telescope
                , ic_wanted    = wanted
                , ic_binds     = ev_binds
                , ic_info      = skol_info } }

implicationNeeded :: SkolemInfo -> [TcTyVar] -> [EvVar] -> TcM Bool
-- See Note [When to build an implication]
implicationNeeded skol_info skol_tvs given
  | null skol_tvs
  , null given
  , not (alwaysBuildImplication skol_info)
  = -- Empty skolems and givens
    andM (isTopTcLevel <$> getTcLevel) -- No implication needed if we are already inside an implication
      ( getDynFlags <???> \ dflags ->
        -- If any deferral can happen, we must build an implication
        gopt Opt_DeferTypeErrors dflags ||
        gopt Opt_DeferTypedHoles dflags ||
        gopt Opt_DeferOutOfScopeVariables dflags
      )

  | otherwise     -- Non-empty skolems or givens
  = return True   -- Definitely need an implication

alwaysBuildImplication :: SkolemInfo -> Bool
-- See Note [When to build an implication]
alwaysBuildImplication _ = False

{-  Commmented out for now while I figure out about error messages.
    See #14185

alwaysBuildImplication (SigSkol ctxt _ _)
  = case ctxt of
      FunSigCtxt {} -> True  -- RHS of a binding with a signature
      _             -> False
alwaysBuildImplication (RuleSkol {})      = True
alwaysBuildImplication (InstSkol {})      = True
alwaysBuildImplication (FamInstSkol {})   = True
alwaysBuildImplication _                  = False
-}

buildImplicationFor :: TcLevel -> SkolemInfo -> [TcTyVar]
                   -> [EvVar] -> WantedConstraints
                   -> TcM (Bag Implication, TcEvBinds)
buildImplicationFor tclvl skol_info skol_tvs given wanted
  | isEmptyWC wanted && null given
             -- Optimisation : if there are no wanteds, and no givens
             -- don't generate an implication at all.
             -- Reason for the (null given): we don't want to lose
             -- the "inaccessible alternative" error check
  = return (empty, emptyTcEvBinds)

  | otherwise
  = assertPpr (all (isSkolemTyVar <||> isTyVarTyVar) skol_tvs) (ppr skol_tvs) $
      -- Why allow TyVarTvs? Because implicitly declared kind variables in
      -- non-CUSK type declarations are TyVarTvs, and we need to bring them
      -- into scope as a skolem in an implication. This is OK, though,
      -- because TyVarTvs will always remain tyvars, even after unification.
    do { ev_binds_var <- newTcEvBinds
       ; implic <- newImplication
       ; let implic' = implic { ic_tclvl  = tclvl
                              , ic_skols  = skol_tvs
                              , ic_given  = given
                              , ic_wanted = wanted
                              , ic_binds  = ev_binds_var
                              , ic_info   = skol_info }

       ; return (pure implic', TcEvBinds ev_binds_var) }

{- Note [When to build an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have some 'skolems' and some 'givens', and we are
considering whether to wrap the constraints in their scope into an
implication.  We must /always/ so if either 'skolems' or 'givens' are
non-empty.  But what if both are empty?  You might think we could
always drop the implication.  Other things being equal, the fewer
implications the better.  Less clutter and overhead.  But we must
take care:

* If we have an unsolved [W] g :: a ~# b, and -fdefer-type-errors,
  we'll make a /term-level/ evidence binding for 'g = error "blah"'.
  We must have an EvBindsVar those bindings!, otherwise they end up as
  top-level unlifted bindings, which are verboten. This only matters
  at top level, so we check for that
  See also Note [Deferred errors for coercion holes] in GHC.Tc.Errors.
  cf #14149 for an example of what goes wrong.

* If you have
     f :: Int;  f = f_blah
     g :: Bool; g = g_blah
  If we don't build an implication for f or g (no tyvars, no givens),
  the constraints for f_blah and g_blah are solved together.  And that
  can yield /very/ confusing error messages, because we can get
      [W] C Int b1    -- from f_blah
      [W] C Int b2    -- from g_blan
  and fundpes can yield [D] b1 ~ b2, even though the two functions have
  literally nothing to do with each other.  #14185 is an example.
  Building an implication keeps them separage.
-}

{-
************************************************************************
*                                                                      *
                Boxy unification
*                                                                      *
************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.
-}

unifyType :: Maybe (HsExpr GhcRn)   -- ^ If present, has type 'ty1'
          -> TcTauType -> TcTauType -> TcM TcCoercionN
-- Actual and expected types
-- Returns a coercion : ty1 ~ ty2
unifyType thing ty1 ty2 = traceTc "utype" (ppr ty1 $$ ppr ty2 $$ ppr thing) >>
                          uType TypeLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual = ty1, uo_expected = ty2
                          , uo_thing  = ppr <$> thing
                          , uo_visible = True } -- always called from a visible context

unifyKind :: Maybe (HsType GhcRn) -> TcKind -> TcKind -> TcM CoercionN
unifyKind thing ty1 ty2 = traceTc "ukind" (ppr ty1 $$ ppr ty2 $$ ppr thing) >>
                          uType KindLevel origin ty1 ty2
  where origin = TypeEqOrigin { uo_actual = ty1, uo_expected = ty2
                              , uo_thing  = ppr <$> thing
                              , uo_visible = True } -- also always from a visible context

---------------

{-
%************************************************************************
%*                                                                      *
                 uType and friends
%*                                                                      *
%************************************************************************

uType is the heart of the unifier.
-}

uType, uType_defer
  :: TypeOrKind
  -> CtOrigin
  -> TcType    -- ty1 is the *actual* type
  -> TcType    -- ty2 is the *expected* type
  -> TcM CoercionN

--------------
-- It is always safe to defer unification to the main constraint solver
-- See Note [Deferred unification]
uType_defer t_or_k origin ty1 ty2
  = do { co <- emitWantedEq origin t_or_k Nominal ty1 ty2

       -- Error trace only
       -- NB. do *not* call mkErrInfo unless tracing is on,
       --     because it is hugely expensive (#5631)
       ; co <$ whenDOptM Opt_D_dump_tc_trace do
            { ctxt <- getErrCtxt
            ; doc <- mkErrInfo emptyTidyEnv ctxt
            ; traceTc "utype_defer" (vcat [ debugPprType ty1
                                          , debugPprType ty2
                                          , pprCtOrigin origin
                                          , doc])
            ; traceTc "utype_defer2" (ppr co) } }

--------------
uType t_or_k origin orig_ty1 orig_ty2
  = do { tclvl <- getTcLevel
       ; traceTc "u_tys" $ vcat
              [ text "tclvl" <+> ppr tclvl
              , sep [ ppr orig_ty1, text "~", ppr orig_ty2]
              , pprCtOrigin origin]
       ; co <- go orig_ty1 orig_ty2
       ; co <$ if isReflCo co
            then traceTc "u_tys yields no coercion" mempty
            else traceTc "u_tys yields coercion:" (ppr co) }
  where
    go :: TcType -> TcType -> TcM CoercionN
        -- The arguments to 'go' are always semantically identical
        -- to orig_ty{1,2} except for looking through type synonyms

     -- Unwrap casts before looking for variables. This way, we can easily
     -- recognize (t |> co) ~ (t |> co), which is nice. Previously, we
     -- didn't do it this way, and then the unification above was deferred.
    go (CastTy t1 co1) t2
      = mkCoherenceLeftCo Nominal t1 co1 <$> uType t_or_k origin t1 t2

    go t1 (CastTy t2 co2)
      = mkCoherenceRightCo Nominal t2 co2 <$> uType t_or_k origin t1 t2

        -- Variables; go for uUnfilledVar
        -- Note that we pass in *original* (before synonym expansion),
        -- so that type variables tend to get filled in with
        -- the most informative version of the type
    go (TyVarTy tv1) ty2
      = lookupTcTyVar tv1 >>= \ case
               Filled ty1   -> do { traceTc "found filled tyvar" (ppr tv1 <+> text ":->" <+> ppr ty1)
                                  ; go ty1 ty2 }
               Unfilled _ -> uUnfilledVar origin t_or_k NotSwapped tv1 ty2
    go ty1 (TyVarTy tv2)
      = lookupTcTyVar tv2 >>= \ case
               Filled ty2   -> do { traceTc "found filled tyvar" (ppr tv2 <+> text ":->" <+> ppr ty2)
                                  ; go ty1 ty2 }
               Unfilled _ -> uUnfilledVar origin t_or_k IsSwapped tv2 ty1

      -- See Note [Expanding synonyms during unification]
    go ty1@(TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = return $ mkNomReflCo ty1

        -- See Note [Expanding synonyms during unification]
        --
        -- Also NB that we recurse to 'go' so that we don't push a
        -- new item on the origin stack. As a result if we have
        --   type Foo = Int
        -- and we try to unify  Foo ~ Bool
        -- we'll end up saying "can't match Foo with Bool"
        -- rather than "can't match "Int with Bool".  See #4535.
    go ty1 ty2
      | Just ty1' <- tcView ty1 = go ty1' ty2
      | Just ty2' <- tcView ty2 = go ty1  ty2'

        -- Functions (or predicate functions) just check the two parts
    go (FunTy _ fun1 arg1) (FunTy _ fun2 arg2)
      = mkFunCo Nominal <$> uType t_or_k origin fun1 fun2 <*> uType t_or_k origin arg1 arg2

        -- Always defer if a type synonym family (type function)
        -- is involved.  (Data families behave rigidly.)
    go ty1@(TyConApp tc1 _) ty2
      | isTypeFamilyTyCon tc1 = defer ty1 ty2
    go ty1 ty2@(TyConApp tc2 _)
      | isTypeFamilyTyCon tc2 = defer ty1 ty2

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      -- See Note [Mismatched type lists and application decomposition]
      | tc1 == tc2, equalLength tys1 tys2
      = assertPpr (isGenerativeTyCon tc1 Nominal) (ppr tc1) $
        mkTyConAppCo Nominal tc1 <$> zipWith3M (uType t_or_k) origins' tys1 tys2
      where
        origins' = bool toInvisibleOrigin id `flip` origin <$> tcTyConVisibilities tc1

    go (LitTy m) ty@(LitTy n)
      | m == n
      = return $ mkNomReflCo ty

        -- See Note [Care with type applications]
        -- Do not decompose FunTy against App;
        -- it's often a type error, so leave it for the constraint solver
    go (AppTy s1 t1) (AppTy s2 t2)
      = go_app (isNextArgVisible s1) s1 t1 s2 t2

    go (AppTy s1 t1) (TyConApp tc2 ts2)
      | Just (ts2', t2') <- snocView ts2
      = assert (not (mustBeSaturated tc2)) $
        go_app (isNextTyConArgVisible tc2 ts2') s1 t1 (TyConApp tc2 ts2') t2'

    go (TyConApp tc1 ts1) (AppTy s2 t2)
      | Just (ts1', t1') <- snocView ts1
      = assert (not (mustBeSaturated tc1)) $
        go_app (isNextTyConArgVisible tc1 ts1') (TyConApp tc1 ts1') t1' s2 t2

    go (CoercionTy co1) (CoercionTy co2) =
      [ mkProofIrrelCo Nominal kco co1 co2
      | let ty1 = coercionType co1
            ty2 = coercionType co2
      , kco <- uType KindLevel
                          (KindEqOrigin orig_ty1 (Just orig_ty2) origin
                                        (Just t_or_k))
                          ty1 ty2 ]

        -- Anything else fails
        -- E.g. unifying for-all types, which is relative unusual
    go ty1 ty2 = defer ty1 ty2

    ------------------
    defer ty1 ty2   -- See Note [Check for equality before deferring]
      | ty1 `tcEqType` ty2 = return (mkNomReflCo ty1)
      | otherwise          = uType_defer t_or_k origin ty1 ty2

    ------------------
    go_app vis s1 t1 s2 t2
      = do { co_s <- uType t_or_k origin s1 s2
           ; let arg_origin
                   | vis       = origin
                   | otherwise = toInvisibleOrigin origin
           ; mkAppCo co_s <$> uType t_or_k arg_origin t1 t2 }

{- Note [Check for equality before deferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Particularly in ambiguity checks we can get equalities like (ty ~ ty).
If ty involves a type function we may defer, which isn't very sensible.
An egregious example of this was in test T9872a, which has a type signature
       Proxy :: Proxy (Solutions Cubes)
Doing the ambiguity check on this signature generates the equality
   Solutions Cubes ~ Solutions Cubes
and currently the constraint solver normalises both sides at vast cost.
This little short-cut in 'defer' helps quite a bit.

Note [Care with type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note: type applications need a bit of care!
They can match FunTy and TyConApp, so use splitAppTy_maybe
NB: we've already dealt with type variables and Notes,
so if one type is an App the other one jolly well better be too

Note [Mismatched type lists and application decomposition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we find two TyConApps, you might think that the argument lists
are guaranteed equal length.  But they aren't. Consider matching
        w (T x) ~ Foo (T x y)
We do match (w ~ Foo) first, but in some circumstances we simply create
a deferred constraint; and then go ahead and match (T x ~ T x y).
This came up in #3950.

So either
   (a) either we must check for identical argument kinds
       when decomposing applications,

   (b) or we must be prepared for ill-kinded unification sub-problems

Currently we adopt (b) since it seems more robust -- no need to maintain
a global invariant.

Note [Expanding synonyms during unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We expand synonyms during unification, but:
 * We expand *after* the variable case so that we tend to unify
   variables with un-expanded type synonym. This just makes it
   more likely that the inferred types will mention type synonyms
   understandable to the user

 * Similarly, we expand *after* the CastTy case, just in case the
   CastTy wraps a variable.

 * We expand *before* the TyConApp case.  For example, if we have
      type Phantom a = Int
   and are unifying
      Phantom Int ~ Phantom Char
   it is *wrong* to unify Int and Char.

 * The problem case immediately above can happen only with arguments
   to the tycon. So we check for nullary tycons *before* expanding.
   This is particularly helpful when checking (* ~ *), because * is
   now a type synonym.

Note [Deferred Unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may encounter a unification ty1 ~ ty2 that cannot be performed syntactically,
and yet its consistency is undetermined. Previously, there was no way to still
make it consistent. So a mismatch error was issued.

Now these unifications are deferred until constraint simplification, where type
family instances and given equations may (or may not) establish the consistency.
Deferred unifications are of the form
                F ... ~ ...
or              x ~ ...
where F is a type function and x is a type variable.
E.g.
        id :: x ~ y => x -> y
        id e = e

involves the unification x = y. It is deferred until we bring into account the
context x ~ y to establish that it holds.

If available, we defer original types (rather than those where closed type
synonyms have already been expanded via tcCoreView).  This is, as usual, to
improve error messages.


************************************************************************
*                                                                      *
                 uUnfilledVar and friends
*                                                                      *
************************************************************************

@uunfilledVar@ is called when at least one of the types being unified is a
variable.  It does {\em not} assume that the variable is a fixed point
of the substitution; rather, notice that @uVar@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.
-}

----------
uUnfilledVar :: CtOrigin
             -> TypeOrKind
             -> SwapFlag
             -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                               --    definitely not a /filled/ meta-tyvar
             -> TcTauType      -- Type 2
             -> TcM Coercion
-- "Unfilled" means that the variable is definitely not a filled-in meta tyvar
--            It might be a skolem, or untouchable, or meta

uUnfilledVar origin t_or_k swapped tv1 ty2
  = do { ty2 <- zonkTcType ty2
             -- Zonk to expose things to the
             -- occurs check, and so that if ty2
             -- looks like a type variable then it
             -- /is/ a type variable
       ; uUnfilledVar1 origin t_or_k swapped tv1 ty2 }

----------
uUnfilledVar1 :: CtOrigin
              -> TypeOrKind
              -> SwapFlag
              -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                                --    definitely not a /filled/ meta-tyvar
              -> TcTauType      -- Type 2, zonked
              -> TcM Coercion
uUnfilledVar1 origin t_or_k swapped tv1 ty2
  | Just tv2 <- tcGetTyVar_maybe ty2
  = go tv2

  | otherwise
  = uUnfilledVar2 origin t_or_k swapped tv1 ty2

  where
    -- 'go' handles the case where both are
    -- tyvars so we might want to swap
    -- E.g. maybe tv2 is a meta-tyvar and tv1 is not
    go tv2 | tv1 == tv2  -- Same type variable => no-op
           = return (mkNomReflCo (mkTyVarTy tv1))

           | swapOverTyVars tv1 tv2   -- Distinct type variables
               -- Swap meta tyvar to the left if poss
           = do { tv1 <- zonkTyCoVarKind tv1
                     -- We must zonk tv1's kind because that might
                     -- not have happened yet, and it's an invariant of
                     -- uUnfilledTyVar2 that ty2 is fully zonked
                     -- Omitting this caused #16902
                ; uUnfilledVar2 origin t_or_k (flipSwap swapped)
                           tv2 (mkTyVarTy tv1) }

           | otherwise
           = uUnfilledVar2 origin t_or_k swapped tv1 ty2

----------
uUnfilledVar2 :: CtOrigin
              -> TypeOrKind
              -> SwapFlag
              -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                                --    definitely not a /filled/ meta-tyvar
              -> TcTauType      -- Type 2, zonked
              -> TcM Coercion
uUnfilledVar2 origin t_or_k swapped tv1 ty2
  = do { dflags  <- getDynFlags
       ; cur_lvl <- getTcLevel
       ; go dflags cur_lvl }
  where
    go dflags cur_lvl
      | canSolveByUnification cur_lvl tv1 ty2
      , MTVU_OK ty2' <- metaTyVarUpdateOK dflags tv1 ty2
      = do { co_k <- uType KindLevel kind_origin (tcTypeKind ty2') (tyVarKind tv1)
           ; traceTc "uUnfilledVar2 ok" $
             vcat [ ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1)
                  , ppr ty2 <+> dcolon <+> ppr (tcTypeKind  ty2)
                  , ppr (isTcReflCo co_k), ppr co_k ]

           ; if isTcReflCo co_k
               -- Only proceed if the kinds match
               -- NB: tv1 should still be unfilled, despite the kind unification
               --     because tv1 is not free in ty2 (or, hence, in its kind)
             then mkTcNomReflCo ty2' <$ writeMetaTyVar tv1 ty2'

             else defer } -- This cannot be solved now.  See GHC.Tc.Solver.Canonical
                          -- Note [Equalities with incompatible kinds]

      | otherwise
      = do { traceTc "uUnfilledVar2 not ok" (ppr tv1 $$ ppr ty2)
               -- Occurs check or an untouchable: just defer
               -- NB: occurs check isn't necessarily fatal:
               --     eg tv1 occurred in type family parameter
            ; defer }

    ty1 = mkTyVarTy tv1
    kind_origin = KindEqOrigin ty1 (Just ty2) origin (Just t_or_k)

    defer = unSwap swapped (uType_defer t_or_k origin) ty1 ty2

swapOverTyVars :: TcTyVar -> TcTyVar -> Bool
swapOverTyVars tv1 tv2
  -- Level comparison: see Note [TyVar/TyVar orientation]
  | lvl1 `strictlyDeeperThan` lvl2 = False
  | lvl2 `strictlyDeeperThan` lvl1 = True

  -- Priority: see Note [TyVar/TyVar orientation]
  | pri1 > pri2 = False
  | pri2 > pri1 = True

  -- Names: see Note [TyVar/TyVar orientation]
  | isSystemName tv2_name, not (isSystemName tv1_name) = True

  | otherwise = False

  where
    lvl1 = tcTyVarLevel tv1
    lvl2 = tcTyVarLevel tv2
    pri1 = lhsPriority tv1
    pri2 = lhsPriority tv2
    tv1_name = Var.varName tv1
    tv2_name = Var.varName tv2


lhsPriority :: TcTyVar -> Int
-- Higher => more important to be on the LHS
-- See Note [TyVar/TyVar orientation]
lhsPriority tv
  = assertPpr (isTyVar tv) (ppr tv) $
    case tcTyVarDetails tv of
      RuntimeUnk  -> 0
      SkolemTv {} -> 0
      MetaTv { mtv_info = info } -> case info of
                                     FlatSkolTv -> 1
                                     TyVarTv    -> 2
                                     TauTv      -> 3
                                     FlatMetaTv -> 4
{- Note [TyVar/TyVar orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (a ~ b), should we orient the CTyEqCan as (a~b) or (b~a)?
This is a surprisingly tricky question! This is invariant (TyEq:TV).

The question is answered by swapOverTyVars, which is use
  - in the eager unifier, in GHC.Tc.Utils.Unify.uUnfilledVar1
  - in the constraint solver, in GHC.Tc.Solver.Canonical.canEqTyVarHomo

First note: only swap if you have to!
   See Note [Avoid unnecessary swaps]

So we look for a positive reason to swap, using a three-step test:

* Level comparison. If 'a' has deeper level than 'b',
  put 'a' on the left.  See Note [Deeper level on the left]

* Priority.  If the levels are the same, look at what kind of
  type variable it is, using 'lhsPriority'.

  Generally speaking we always try to put a MetaTv on the left
  in preference to SkolemTv or RuntimeUnkTv:
     a) Because the MetaTv may be touchable and can be unified
     b) Even if it's not touchable, GHC.Tc.Solver.floatEqualities
        looks for meta tyvars on the left

  Tie-breaking rules for MetaTvs:
  - FlatMetaTv = 4: always put on the left.
        See Note [Fmv Orientation Invariant]

        NB: FlatMetaTvs always have the current level, never an
        outer one.  So nothing can be deeper than a FlatMetaTv.

  - TauTv = 3: if we have  tyv_tv ~ tau_tv,
       put tau_tv on the left because there are fewer
       restrictions on updating TauTvs.  Or to say it another
       way, then we won't lose the TyVarTv flag

  - TyVarTv = 2: remember, flat-skols are *only* updated by
       the unflattener, never unified, so TyVarTvs come next

  - FlatSkolTv = 1: put on the left in preference to a SkolemTv.
       See Note [Eliminate flat-skols]

* Names. If the level and priority comparisons are all
  equal, try to eliminate a TyVars with a System Name in
  favour of ones with a Name derived from a user type signature

* Age.  At one point in the past we tried to break any remaining
  ties by eliminating the younger type variable, based on their
  Uniques.  See Note [Eliminate younger unification variables]
  (which also explains why we don't do this any more)

Note [Deeper level on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The most important thing is that we want to put tyvars with
the deepest level on the left.  The reason to do so differs for
Wanteds and Givens, but either way, deepest wins!  Simple.

* Wanteds.  Putting the deepest variable on the left maximise the
  chances that it's a touchable meta-tyvar which can be solved.

* Givens. Suppose we have something like
     forall a[2]. b[1] ~ a[2] => beta[1] ~ a[2]

  If we orient the Given a[2] on the left, we'll rewrite the Wanted to
  (beta[1] ~ b[1]), and that can float out of the implication.
  Otherwise it can't.  By putting the deepest variable on the left
  we maximise our changes of eliminating skolem capture.

  See also GHC.Tc.Solver.Monad Note [Let-bound skolems] for another reason
  to orient with the deepest skolem on the left.

  IMPORTANT NOTE: this test does a level-number comparison on
  skolems, so it's important that skolems have (accurate) level
  numbers.

See #15009 for an further analysis of why "deepest on the left"
is a good plan.

Note [Fmv Orientation Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * We always orient a constraint
        fmv ~ alpha
     with fmv on the left, even if alpha is
     a touchable unification variable

Reason: doing it the other way round would unify alpha:=fmv, but that
really doesn't add any info to alpha.  But a later constraint alpha ~
Int might unlock everything.  Comment:9 of #12526 gives a detailed
example.

WARNING: I've gone to and fro on this one several times.
I'm now pretty sure that unifying alpha:=fmv is a bad idea!
So orienting with fmvs on the left is a good thing.

This example comes from IndTypesPerfMerge. (Others include
T10226, T10009.)
    From the ambiguity check for
      f :: (F a ~ a) => a
    we get:
          [G] F a ~ a
          [WD] F alpha ~ alpha, alpha ~ a

    From Givens we get
          [G] F a ~ fsk, fsk ~ a

    Now if we flatten we get
          [WD] alpha ~ fmv, F alpha ~ fmv, alpha ~ a

    Now, if we unified alpha := fmv, we'd get
          [WD] F fmv ~ fmv, [WD] fmv ~ a
    And now we are stuck.

So instead the Fmv Orientation Invariant puts the fmv on the
left, giving
      [WD] fmv ~ alpha, [WD] F alpha ~ fmv, [WD] alpha ~ a

    Now we get alpha:=a, and everything works out

Note [Eliminate flat-skols]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have  [G] Num (F [a])
then we flatten to
     [G] Num fsk
     [G] F [a] ~ fsk
where fsk is a flatten-skolem (FlatSkolTv). Suppose we have
      type instance F [a] = a
then we'll reduce the second constraint to
     [G] a ~ fsk
and then replace all uses of 'a' with fsk.  That's bad because
in error messages instead of saying 'a' we'll say (F [a]).  In all
places, including those where the programmer wrote 'a' in the first
place.  Very confusing!  See #7862.

Solution: re-orient a~fsk to fsk~a, so that we preferentially eliminate
the fsk.

Note [Avoid unnecessary swaps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we swap without actually improving matters, we can get an infinite loop.
Consider
    work item:  a ~ b
   inert item:  b ~ c
We canonicalise the work-item to (a ~ c).  If we then swap it before
adding to the inert set, we'll add (c ~ a), and therefore kick out the
inert guy, so we get
   new work item:  b ~ c
   inert item:     c ~ a
And now the cycle just repeats

Note [Eliminate younger unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a choice of unifying
     alpha := beta   or   beta := alpha
we try, if possible, to eliminate the "younger" one, as determined
by `ltUnique`.  Reason: the younger one is less likely to appear free in
an existing inert constraint, and hence we are less likely to be forced
into kicking out and rewriting inert constraints.

This is a performance optimisation only.  It turns out to fix
#14723 all by itself, but clearly not reliably so!

It's simple to implement (see nicer_to_update_tv2 in swapOverTyVars).
But, to my surprise, it didn't seem to make any significant difference
to the compiler's performance, so I didn't take it any further.  Still
it seemed to too nice to discard altogether, so I'm leaving these
notes.  SLPJ Jan 18.
-}

-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable.
-- Returns True <=> spontaneous solve happened
canSolveByUnification :: TcLevel -> TcTyVar -> TcType -> Bool
canSolveByUnification tclvl tv xi
  | isTouchableMetaTyVar tclvl tv
  = case metaTyVarInfo tv of
      TyVarTv -> is_tyvar xi
      _       -> True

  | otherwise    -- Untouchable
  = False
  where
    is_tyvar xi
      = case tcGetTyVar_maybe xi of
          Nothing -> False
          Just tv -> case tcTyVarDetails tv of
                       MetaTv { mtv_info = info }
                                   -> case info of
                                        TyVarTv -> True
                                        _       -> False
                       SkolemTv {} -> True
                       RuntimeUnk  -> True

{- Note [Prevent unification with type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prevent unification with type families because of an uneasy compromise.
It's perfectly sound to unify with type families, and it even improves the
error messages in the testsuite. It also modestly improves performance, at
least in some cases. But it's disastrous for test case perf/compiler/T3064.
Here is the problem: Suppose we have (F ty) where we also have [G] F ty ~ a.
What do we do? Do we reduce F? Or do we use the given? Hard to know what's
best. GHC reduces. This is a disaster for T3064, where the type's size
spirals out of control during reduction. (We're not helped by the fact that
the flattener re-flattens all the arguments every time around.) If we prevent
unification with type families, then the solver happens to use the equality
before expanding the type family.

It would be lovely in the future to revisit this problem and remove this
extra, unnecessary check. But we retain it for now as it seems to work
better in practice.

Note [Refactoring hazard: checkTauTvUpdate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I (Richard E.) have a sad story about refactoring this code, retained here
to prevent others (or a future me!) from falling into the same traps.

It all started with #11407, which was caused by the fact that the TyVarTy
case of defer_me didn't look in the kind. But it seemed reasonable to
simply remove the defer_me check instead.

It referred to two Notes (since removed) that were out of date, and the
fast_check code in occurCheckExpand seemed to do just about the same thing as
defer_me. The one piece that defer_me did that wasn't repeated by
occurCheckExpand was the type-family check. (See Note [Prevent unification
with type families].) So I checked the result of occurCheckExpand for any
type family occurrences and deferred if there were any. This was done
in commit e9bf7bb5cc9fb3f87dd05111aa23da76b86a8967 .

This approach turned out not to be performant, because the expanded
type was bigger than the original type, and tyConsOfType (needed to
see if there are any type family occurrences) looks through type
synonyms. So it then struck me that we could dispense with the
defer_me check entirely. This simplified the code nicely, and it cut
the allocations in T5030 by half. But, as documented in Note [Prevent
unification with type families], this destroyed performance in
T3064. Regardless, I missed this regression and the change was
committed as 3f5d1a13f112f34d992f6b74656d64d95a3f506d .

Bottom lines:
 * defer_me is back, but now fixed w.r.t. #11407.
 * Tread carefully before you start to refactor here. There can be
   lots of hard-to-predict consequences.

Note [Type synonyms and the occur check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking we try to update a variable with type synonyms not
expanded, which improves later error messages, unless looking
inside a type synonym may help resolve a spurious occurs check
error. Consider:
          type A a = ()

          f :: (A a -> a -> ()) -> ()
          f = \ _ -> ()

          x :: ()
          x = f (\ x p -> p x)

We will eventually get a constraint of the form t ~ A t. The ok function above will
properly expand the type (A t) to just (), which is ok to be unified with t. If we had
unified with the original type A t, we would lead the type checker into an infinite loop.

Hence, if the occurs check fails for a type synonym application, then (and *only* then),
the ok function expands the synonym to detect opportunities for occurs check success using
the underlying definition of the type synonym.

The same applies later on in the constraint interaction code; see GHC.Tc.Solver.Interact,
function @occ_check_ok@.

Note [Non-TcTyVars in GHC.Tc.Utils.Unify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because the same code is now shared between unifying types and unifying
kinds, we sometimes will see proper TyVars floating around the unifier.
Example (from test case polykinds/PolyKinds12):

    type family Apply (f :: k1 -> k2) (x :: k1) :: k2
    type instance Apply g y = g y

When checking the instance declaration, we first *kind-check* the LHS
and RHS, discovering that the instance really should be

    type instance Apply k3 k4 (g :: k3 -> k4) (y :: k3) = g y

During this kind-checking, all the tyvars will be TcTyVars. Then, however,
as a second pass, we desugar the RHS (which is done in functions prefixed
with "tc" in GHC.Tc.TyCl"). By this time, all the kind-vars are proper
TyVars, not TcTyVars, get some kind unification must happen.

Thus, we always check if a TyVar is a TcTyVar before asking if it's a
meta-tyvar.

This used to not be necessary for type-checking (that is, before * :: *)
because expressions get desugared via an algorithm separate from
type-checking (with wrappers, etc.). Types get desugared very differently,
causing this wibble in behavior seen here.
-}

data LookupTyVarResult  -- The result of a lookupTcTyVar call
  = Unfilled TcTyVarDetails     -- SkolemTv or virgin MetaTv
  | Filled   TcType

lookupTcTyVar :: TcTyVar -> TcM LookupTyVarResult
lookupTcTyVar tyvar
  | MetaTv { mtv_ref = ref } <- details
  = readMutVar ref >>= \ case
           Indirect ty -> return (Filled ty)
           Flexi -> isTouchableTcM tyvar <???>
                             -- Note [Unifying untouchables]
                Unfilled . bool vanillaSkolemTv details
  | otherwise
  = pure (Unfilled details)
  where
    details = tcTyVarDetails tyvar

{-
Note [Unifying untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat an untouchable type variable as if it was a skolem.  That
ensures it won't unify with anything.  It's a slight hack, because
we return a made-up TcTyVarDetails, but I think it works smoothly.
-}

-- | Breaks apart a function kind into its pieces.
matchExpectedFunKind
  :: Outputable fun
  => fun             -- ^ type, only for errors
  -> Arity           -- ^ n: number of desired arrows
  -> TcKind          -- ^ fun_ kind
  -> TcM Coercion    -- ^ co :: fun_kind ~ (arg1 -> ... -> argn -> res)

matchExpectedFunKind hs_ty n k = go n k
  where
    go 0 k = pure (mkNomReflCo k)

    go n k | Just k' <- tcView k = go n k'

    go n k@(TyVarTy kvar)
      | isMetaTyVar kvar
      = readMetaTyVar kvar >>= \ case
                Indirect fun_kind -> go n fun_kind
                Flexi ->             defer n k

    go n (FunTy _ arg res)
      = mkTcFunCo Nominal (mkTcNomReflCo arg) <$> go (n-1) res

    go n other
     = defer n other

    defer n k
      = do { arg_kinds <- newMetaKindVars n
           ; res_kind  <- newMetaKindVar
           ; let new_fun = mkVisFunTys arg_kinds res_kind
                 origin  = TypeEqOrigin { uo_actual   = k
                                        , uo_expected = new_fun
                                        , uo_thing    = Just (ppr hs_ty)
                                        , uo_visible  = True
                                        }
           ; uType KindLevel origin k new_fun }

{- *********************************************************************
*                                                                      *
                 Occurrence checking
*                                                                      *
********************************************************************* -}


{-  Note [Occurrence checking: look inside kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are considering unifying
   (alpha :: *)  ~  Int -> (beta :: alpha -> alpha)
This may be an error (what is that alpha doing inside beta's kind?),
but we must not make the mistake of actually unifying or we'll
build an infinite data structure.  So when looking for occurrences
of alpha in the rhs, we must look in the kinds of type variables
that occur there.

NB: we may be able to remove the problem via expansion; see
    Note [Occurs check expansion].  So we have to try that.

Note [Checking for foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unless we have -XImpredicativeTypes (which is a totally unsupported
feature), we do not want to unify
    alpha ~ (forall a. a->a) -> Int
So we look for foralls hidden inside the type, and it's convenient
to do that at the same time as the occurs check (which looks for
occurrences of alpha).

However, it's not just a question of looking for foralls /anywhere/!
Consider
   (alpha :: forall k. k->*)  ~  (beta :: forall k. k->*)
This is legal; e.g. dependent/should_compile/T11635.

We don't want to reject it because of the forall in beta's kind,
but (see Note [Occurrence checking: look inside kinds]) we do
need to look in beta's kind.  So we carry a flag saying if a 'forall'
is OK, and switch the flag on when stepping inside a kind.

Why is it OK?  Why does it not count as impredicative polymorphism?
The reason foralls are bad is because we reply on "seeing" foralls
when doing implicit instantiation.  But the forall inside the kind is
fine.  We'll generate a kind equality constraint
  (forall k. k->*) ~ (forall k. k->*)
to check that the kinds of lhs and rhs are compatible.  If alpha's
kind had instead been
  (alpha :: kappa)
then this kind equality would rightly complain about unifying kappa
with (forall k. k->*)

-}

data MetaTyVarUpdateResult a
  = MTVU_OK a
  | MTVU_Bad          -- Forall, predicate, or type family
  | MTVU_HoleBlocker  -- Blocking coercion hole
        -- See Note [Equalities with incompatible kinds] in "GHC.Tc.Solver.Canonical"
  | MTVU_Occurs
    deriving (Functor)

instance Applicative MetaTyVarUpdateResult where
      pure = MTVU_OK
      (<*>) = ap

instance Monad MetaTyVarUpdateResult where
  MTVU_OK x        >>= k = k x
  MTVU_Bad         >>= _ = MTVU_Bad
  MTVU_HoleBlocker >>= _ = MTVU_HoleBlocker
  MTVU_Occurs      >>= _ = MTVU_Occurs

instance Outputable a => Outputable (MetaTyVarUpdateResult a) where
  ppr (MTVU_OK a)      = text "MTVU_OK" <+> ppr a
  ppr MTVU_Bad         = text "MTVU_Bad"
  ppr MTVU_HoleBlocker = text "MTVU_HoleBlocker"
  ppr MTVU_Occurs      = text "MTVU_Occurs"

occCheckForErrors :: DynFlags -> TcTyVar -> Type -> MetaTyVarUpdateResult ()
-- Just for error-message generation; so we return MetaTyVarUpdateResult
-- so the caller can report the right kind of error
-- Check whether
--   a) the given variable occurs in the given type.
--   b) there is a forall in the type (unless we have -XImpredicativeTypes)
occCheckForErrors dflags tv ty
  = case preCheck dflags True tv ty of
      MTVU_OK _        -> MTVU_OK ()
      MTVU_Bad         -> MTVU_Bad
      MTVU_HoleBlocker -> MTVU_HoleBlocker
      MTVU_Occurs      -> case occCheckExpand [tv] ty of
                            Nothing -> MTVU_Occurs
                            Just _  -> MTVU_OK ()

----------------
metaTyVarUpdateOK :: DynFlags
                  -> TcTyVar             -- tv :: k1
                  -> TcType              -- ty :: k2
                  -> MetaTyVarUpdateResult TcType        -- possibly-expanded ty
-- (metaTyVarUpdateOK tv ty)
-- We are about to update the meta-tyvar tv with ty
-- Check (a) that tv doesn't occur in ty (occurs check)
--       (b) that ty does not have any foralls
--           (in the impredicative case), or type functions
--       (c) that ty does not have any blocking coercion holes
--           See Note [Equalities with incompatible kinds] in "GHC.Tc.Solver.Canonical"
--
-- We have two possible outcomes:
-- (1) Return the type to update the type variable with,
--        [we know the update is ok]
-- (2) Return Nothing,
--        [the update might be dodgy]
--
-- Note that "Nothing" does not mean "definite error".  For example
--   type family F a
--   type instance F Int = Int
-- consider
--   a ~ F a
-- This is perfectly reasonable, if we later get a ~ Int.  For now, though,
-- we return Nothing, leaving it to the later constraint simplifier to
-- sort matters out.
--
-- See Note [Refactoring hazard: checkTauTvUpdate]

metaTyVarUpdateOK dflags tv ty
  = case preCheck dflags False tv ty of
         -- False <=> type families not ok
         -- See Note [Prevent unification with type families]
      MTVU_OK _        -> MTVU_OK ty
      MTVU_Bad         -> MTVU_Bad          -- forall, predicate, type function
      MTVU_HoleBlocker -> MTVU_HoleBlocker  -- coercion hole
      MTVU_Occurs      -> case occCheckExpand [tv] ty of
                            Just expanded_ty -> MTVU_OK expanded_ty
                            Nothing          -> MTVU_Occurs

preCheck :: DynFlags -> Bool -> TcTyVar -> TcType -> MetaTyVarUpdateResult ()
-- A quick check for
--   (a) a forall type (unless -XImpredicativeTypes)
--   (b) a predicate type (unless -XImpredicativeTypes)
--   (c) a type family
--   (d) a blocking coercion hole
--   (e) an occurrence of the type variable (occurs check)
--
-- For (a), (b), and (c) we check only the top level of the type, NOT
-- inside the kinds of variables it mentions.  For (d) we look deeply
-- in coercions, and for (e) we do look in the kinds of course.

preCheck dflags ty_fam_ok tv ty
  = fast_check ty
  where
    details          = tcTyVarDetails tv
    impredicative_ok = canUnifyWithPolyType dflags details

    ok :: MetaTyVarUpdateResult ()
    ok = MTVU_OK ()

    fast_check :: TcType -> MetaTyVarUpdateResult ()
    fast_check (TyVarTy tv')
      | tv == tv' = MTVU_Occurs
      | otherwise = fast_check_occ (tyVarKind tv')
           -- See Note [Occurrence checking: look inside kinds]

    fast_check (TyConApp tc tys)
      | bad_tc tc              = MTVU_Bad
      | otherwise              = traverse_ fast_check tys >> ok
    fast_check (LitTy {})      = ok
    fast_check (FunTy{ft_af = af, ft_arg = a, ft_res = r})
      | InvisArg <- af
      , not impredicative_ok   = MTVU_Bad
      | otherwise              = fast_check a   >> fast_check r
    fast_check (AppTy fun arg) = fast_check fun >> fast_check arg
    fast_check (CastTy ty co)  = fast_check ty  >> fast_check_co co
    fast_check (CoercionTy co) = fast_check_co co
    fast_check (ForAllTy (Bndr tv' _) ty)
       | not impredicative_ok = MTVU_Bad
       | tv == tv'            = ok
       | otherwise = do { fast_check_occ (tyVarKind tv')
                        ; fast_check_occ ty }
       -- Under a forall we look only for occurrences of
       -- the type variable

     -- For kinds, we only do an occurs check; we do not worry
     -- about type families or foralls
     -- See Note [Checking for foralls]
    fast_check_occ k | tv `elemVarSet` tyCoVarsOfType k = MTVU_Occurs
                     | otherwise                        = ok

     -- no bother about impredicativity in coercions, as they're
     -- inferred
    fast_check_co co | not (gopt Opt_DeferTypeErrors dflags)
                     , badCoercionHoleCo co            = MTVU_HoleBlocker
        -- Wrinkle (4b) in "GHC.Tc.Solver.Canonical" Note [Equalities with incompatible kinds]

                     | tv `elemVarSet` tyCoVarsOfCo co = MTVU_Occurs
                     | otherwise                       = ok

    bad_tc :: TyCon -> Bool
    bad_tc tc = not (impredicative_ok || isTauTyCon tc) || not (ty_fam_ok || isFamFreeTyCon tc)

canUnifyWithPolyType :: DynFlags -> TcTyVarDetails -> Bool
canUnifyWithPolyType dflags = \ case
      MetaTv { mtv_info = TyVarTv }    -> False
      MetaTv { mtv_info = TauTv }      -> xopt LangExt.ImpredicativeTypes dflags
      _other                           -> True
          -- We can have non-meta tyvars in given constraints
