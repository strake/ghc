{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE CPP #-}

#include "lens.h"

-- | Various types used during desugaring.
module GHC.HsToCore.Types (
        DsM,
        DsLclEnv(..), dsl_metaL, dsl_locL, dsl_deltasL,
        DsGblEnv(..), ds_modL, ds_fam_inst_envL, ds_unqualL, ds_msgsL, ds_if_envL, ds_complete_matchesL, ds_cc_stL,
        DsMetaEnv, DsMetaVal(..), CompleteMatchMap
    ) where

import Prelude ((<$>))

import Data.IORef

import GHC.Types.CostCentre.State
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Hs (LForeignDecl, HsExpr, GhcTc)
import GHC.Tc.Types (TcRnIf, IfGblEnv, IfLclEnv, CompleteMatchMap)
import {-# SOURCE #-} GHC.HsToCore.PmCheck.Types (Deltas)
import GHC.Core (CoreExpr)
import GHC.Core.FamInstEnv
import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Unit.Module
import GHC.Driver.Hooks (DsForeignsHook)
import GHC.Data.OrdList (OrdList)
import GHC.Types.ForeignStubs (ForeignStubs)

{-
************************************************************************
*                                                                      *
                Desugarer monad
*                                                                      *
************************************************************************

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
-}

-- | Global read-only context and state of the desugarer.
-- The statefulness is implemented through 'IORef's.
data DsGblEnv
  = DsGblEnv
  { ds_mod          :: Module             -- For SCC profiling
  , ds_fam_inst_env :: FamInstEnv         -- Like tcg_fam_inst_env
  , ds_unqual  :: PrintUnqualified
  , ds_msgs    :: IORef Messages          -- Warning messages
  , ds_if_env  :: (IfGblEnv, IfLclEnv)    -- Used for looking up global,
                                          -- possibly-imported things
  , ds_complete_matches :: CompleteMatchMap
     -- Additional complete pattern matches
  , ds_cc_st   :: IORef CostCentreState
     -- Tracking indices for cost centre annotations
  }

LENS_FIELD(ds_modL, ds_mod)
LENS_FIELD(ds_fam_inst_envL, ds_fam_inst_env)
LENS_FIELD(ds_unqualL, ds_unqual)
LENS_FIELD(ds_msgsL, ds_msgs)
LENS_FIELD(ds_if_envL, ds_if_env)
LENS_FIELD(ds_complete_matchesL, ds_complete_matches)
LENS_FIELD(ds_cc_stL, ds_cc_st)

instance ContainsModule DsGblEnv where
  extractModule = ds_mod

-- | Local state of the desugarer, extended as we lexically descend
data DsLclEnv = DsLclEnv
  { dsl_meta    :: DsMetaEnv   -- ^ Template Haskell bindings
  , dsl_loc     :: RealSrcSpan -- ^ To put in pattern-matching error msgs
  , -- See Note [Note [Type and Term Equality Propagation] in "GHC.HsToCore.PmCheck"
    -- The set of reaching values Deltas is augmented as we walk inwards,
    -- refined through each pattern match in turn
    dsl_deltas  :: Deltas
  }

LENS_FIELD(dsl_metaL, dsl_meta)
LENS_FIELD(dsl_locL, dsl_loc)
LENS_FIELD(dsl_deltasL, dsl_deltas)

-- Inside [| |] brackets, the desugarer looks
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
  = DsBound Id         -- Bound by a pattern inside the [| |].
                       -- Will be dynamically alpha renamed.
                       -- The Id has type THSyntax.Var

  | DsSplice (HsExpr GhcTc) -- These bindings are introduced by
                            -- the PendingSplices on a HsBracketOut

-- | Desugaring monad. See also 'TcM'.
type DsM = TcRnIf DsGblEnv DsLclEnv
-- See Note [The Decoupling Abstract Data Hack]
type instance DsForeignsHook = [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
