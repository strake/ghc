module GHC.Rename.Splice where

import GHC.Prelude
import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Types.Name.Set

import Control.Monad.Trans.Writer ( WriterT (..) )

rnSpliceType :: HsSplice GhcPs   -> WriterT FreeVars RnM (HsType GhcRn)
rnSplicePat  :: HsSplice GhcPs   -> WriterT FreeVars RnM (Either (Pat GhcPs) (Pat GhcRn))
rnSpliceDecl :: SpliceDecl GhcPs -> WriterT FreeVars RnM (SpliceDecl GhcRn)

rnTopSpliceDecls :: HsSplice GhcPs -> WriterT FreeVars RnM [LHsDecl GhcPs]
