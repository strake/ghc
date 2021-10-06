module GHC.Types.Name.Ppr
   ( mkPrintUnqualified
   , mkQualModule
   , mkQualPackage
   , pkgQual
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.State

import GHC.Core.TyCon

import GHC.Types.Name
import GHC.Types.Name.Reader

import GHC.Builtin.Types

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc


{-
Note [Printing original names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding how to print names is pretty tricky.  We are given a name
P:M.T, where P is the package name, M is the defining module, and T is
the occurrence name, and we have to decide in which form to display
the name given a GlobalRdrEnv describing the current scope.

Ideally we want to display the name in the form in which it is in
scope.  However, the name might not be in scope at all, and that's
where it gets tricky.  Here are the cases:

 1. T uniquely maps to  P:M.T      --->  "T"      NameUnqual
 2. There is an X for which X.T
       uniquely maps to  P:M.T     --->  "X.T"    NameQual X
 3. There is no binding for "M.T"  --->  "M.T"    NameNotInScope1
 4. Otherwise                      --->  "P:M.T"  NameNotInScope2

(3) and (4) apply when the entity P:M.T is not in the GlobalRdrEnv at
all. In these cases we still want to refer to the name as "M.T", *but*
"M.T" might mean something else in the current scope (e.g. if there's
an "import X as M"), so to avoid confusion we avoid using "M.T" if
there's already a binding for it.  Instead we write P:M.T.

There's one further subtlety: in case (3), what if there are two
things around, P1:M.T and P2:M.T?  Then we don't want to print both of
them as M.T!  However only one of the modules P1:M and P2:M can be
exposed (say P2), so we use M.T for that, and P1:M.T for the other one.
This is handled by the qual_mod component of PrintUnqualified, inside
the (ppr mod) of case (3), in Name.pprModulePrefix

Note [Printing unit ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the old days, original names were tied to PackageIds, which directly
corresponded to the entities that users wrote in Cabal files, and were perfectly
suitable for printing when we need to disambiguate packages.  However, with
instantiated units, the situation can be different: if the key is instantiated
with some holes, we should try to give the user some more useful information.
-}

-- | Creates some functions that work out the best ways to format
-- names for the user according to a set of heuristics.
mkPrintUnqualified :: PackageState -> Unit -> GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualified unit_state home_unit env
 = QueryQualify qual_name
      (mkQualModule unit_state home_unit)
      (mkQualPackage unit_state)
  where
  qual_name mod occ
        | [gre] <- unqual_gres
        , right_name gre
        = NameUnqual   -- If there's a unique entity that's in scope
                       -- unqualified with 'occ' AND that entity is
                       -- the right one, then we can use the unqualified name

        | [] <- unqual_gres
        , any is_name forceUnqualNames
        , not (isDerivedOccName occ)
        = NameUnqual   -- Don't qualify names that come from modules
                       -- that come with GHC, often appear in error messages,
                       -- but aren't typically in scope. Doing this does not
                       -- cause ambiguity, and it reduces the amount of
                       -- qualification in error messages thus improving
                       -- readability.
                       --
                       -- A motivating example is 'Constraint'. It's often not
                       -- in scope, but printing GHC.Prim.Constraint seems
                       -- overkill.

        | [gre] <- qual_gres
        = NameQual (greQualModName gre)

        | null qual_gres
        = if null (lookupGRE_RdrName (mkRdrQual (moduleName mod) occ) env)
          then NameNotInScope1
          else NameNotInScope2

        | otherwise
        = NameNotInScope1   -- Can happen if 'f' is bound twice in the module
                            -- Eg  f = True; g = 0; f = False
      where
        is_name :: Name -> Bool
        is_name name = assertPpr (isExternalName name) (ppr name) $
                       nameModule name == mod && nameOccName name == occ

        forceUnqualNames :: [Name]
        forceUnqualNames =
          map tyConName [ constraintKindTyCon, heqTyCon, coercibleTyCon ]
          ++ [ eqTyConName ]

        right_name gre = nameModule_maybe (gre_name gre) == Just mod

        unqual_gres = lookupGRE_RdrName (mkRdrUnqual occ) env
        qual_gres   = filter right_name (lookupGlobalRdrEnv env occ)

    -- we can mention a module P:M without the P: qualifier iff
    -- "import M" would resolve unambiguously to P:M.  (if P is the
    -- current package we can just assume it is unqualified).

-- | Creates a function for formatting modules based on two heuristics:
-- (1) if the module is the current module, don't qualify, and (2) if there
-- is only one exposed package which exports this module, don't qualify.
mkQualModule :: PackageState -> Unit -> QueryQualifyModule
mkQualModule unit_state home_unit mod
     | home_unit == moduleUnit mod = False

     | [(_, pkgconfig)] <- lookup,
       mkUnit pkgconfig == moduleUnit mod
        -- this says: we are given a module P:M, is there just one exposed package
        -- that exposes a module M, and is it package P?
     = False

     | otherwise = True
     where lookup = lookupModuleInAllPackages unit_state (moduleName mod)

-- | Creates a function for formatting packages based on two heuristics:
-- (1) don't qualify if the package in question is "main", and (2) only qualify
-- with a unit id if the package ID would be ambiguous.
mkQualPackage :: PackageState -> QueryQualifyPackage
mkQualPackage pkgs uid
     | uid == mainUnitId || uid == interactiveUnitId
        -- Skip the lookup if it's main, since it won't be in the package
        -- database!
     = False
     | Just pkgid <- mb_pkgid
     , searchPackageId pkgs pkgid `lengthIs` 1
        -- this says: we are given a package pkg-0.1@MMM, are there only one
        -- exposed packages whose package ID is pkg-0.1?
     = False
     | otherwise
     = True
     where mb_pkgid = fmap unitPackageId (lookupUnit pkgs uid)

-- | A function which only qualifies package names if necessary; but
-- qualifies all other identifiers.
pkgQual :: PackageState -> PrintUnqualified
pkgQual pkgs = alwaysQualify { queryQualifyPackage = mkQualPackage pkgs }
