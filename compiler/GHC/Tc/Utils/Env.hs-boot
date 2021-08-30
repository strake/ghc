{-# LANGUAGE TypeFamilies #-}

module GHC.Tc.Utils.Env where

import GHC.Tc.Types( Env, TcLclEnv )
import GHC.Types.Var.Env( TidyEnv )

import Control.Monad.IO.Class( MonadIO )
import Data.Functor.Reader.Class( IsReader (EnvType) )

-- Annoyingly, there's a recursion between tcInitTidyEnv
-- (which does zonking and hence needs GHC.Tc.Utils.TcMType) and
-- addErrTc etc which live in GHC.Tc.Utils.Monad.  Rats.
tcInitTidyEnv :: (IsReader m, MonadIO m, EnvType m ~ Env gbl TcLclEnv) => m TidyEnv
