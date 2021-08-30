module GHC.Tc.Types where

import GHC.Tc.Utils.TcType
import GHC.Types.SrcLoc
import Lens.Micro (Lens')

data TcLclEnv

tcl_tclvlL :: Lens' TcLclEnv TcLevel
tcl_locL :: Lens' TcLclEnv RealSrcSpan
