{-
%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
-}

-- | FastStringEnv: FastString environments
module GHC.Data.FastString.Env (
        -- * FastString environments (maps)
        FastStringEnv,

        -- ** Manipulating these environments
        mkFsEnv,
        emptyFsEnv, unitFsEnv,
        extendFsEnv_C, extendFsEnv_Acc, extendFsEnv,
        extendFsEnvList, extendFsEnvList_C,
        plusFsEnv, plusFsEnv_C, alterFsEnv,
        lookupFsEnv, lookupFsEnv_NF, delFromFsEnv, delListFromFsEnv,
        elemFsEnv,

        -- * Deterministic FastString environments (maps)
        DFastStringEnv,

        -- ** Manipulating these environments
        mkDFsEnv, emptyDFsEnv, lookupDFsEnv
    ) where

import GHC.Prelude

import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Data.Maybe
import GHC.Data.FastString


-- | A non-deterministic set of FastStrings.
-- See Note [Deterministic UniqFM] in "GHC.Types.Unique.DFM" for explanation why it's not
-- deterministic and why it matters. Use DFastStringEnv if the set eventually
-- gets converted into a list or folded over in a way where the order
-- changes the generated code.
type FastStringEnv a = UniqFM FastString a  -- Domain is FastString

emptyFsEnv         :: FastStringEnv a
mkFsEnv            :: [(FastString,a)] -> FastStringEnv a
alterFsEnv         :: (Maybe a-> Maybe a) -> FastStringEnv a -> FastString -> FastStringEnv a
extendFsEnv_C      :: (a->a->a) -> FastStringEnv a -> FastString -> a -> FastStringEnv a
extendFsEnv_Acc    :: (a->b->b) -> (a->b) -> FastStringEnv b -> FastString -> a -> FastStringEnv b
extendFsEnv        :: FastStringEnv a -> FastString -> a -> FastStringEnv a
plusFsEnv          :: FastStringEnv a -> FastStringEnv a -> FastStringEnv a
plusFsEnv_C        :: (a->a->a) -> FastStringEnv a -> FastStringEnv a -> FastStringEnv a
extendFsEnvList    :: FastStringEnv a -> [(FastString,a)] -> FastStringEnv a
extendFsEnvList_C  :: (a->a->a) -> FastStringEnv a -> [(FastString,a)] -> FastStringEnv a
delFromFsEnv       :: FastStringEnv a -> FastString -> FastStringEnv a
delListFromFsEnv   :: FastStringEnv a -> [FastString] -> FastStringEnv a
elemFsEnv          :: FastString -> FastStringEnv a -> Bool
unitFsEnv          :: FastString -> a -> FastStringEnv a
lookupFsEnv        :: FastStringEnv a -> FastString -> Maybe a
lookupFsEnv_NF     :: FastStringEnv a -> FastString -> a

emptyFsEnv                = emptyUFM
unitFsEnv                 = unitUFM
extendFsEnv               = addToUFM
extendFsEnvList           = addListToUFM
lookupFsEnv               = lookupUFM
alterFsEnv                = alterUFM
mkFsEnv                   = listToUFM
elemFsEnv                 = elemUFM
plusFsEnv                 = plusUFM
plusFsEnv_C               = plusUFM_C
extendFsEnv_C             = addToUFM_C
extendFsEnv_Acc           = addToUFM_Acc
extendFsEnvList_C         = addListToUFM_C
delFromFsEnv              = delFromUFM
delListFromFsEnv          = delListFromUFM

lookupFsEnv_NF env n = expectJust "lookupFsEnv_NF" (lookupFsEnv env n)

-- Deterministic FastStringEnv
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for explanation why we need
-- DFastStringEnv.

type DFastStringEnv a = UniqDFM FastString a  -- Domain is FastString

emptyDFsEnv :: DFastStringEnv a
emptyDFsEnv = emptyUDFM

mkDFsEnv :: [(FastString,a)] -> DFastStringEnv a
mkDFsEnv l = listToUDFM l

lookupDFsEnv :: DFastStringEnv a -> FastString -> Maybe a
lookupDFsEnv = lookupUDFM
