module GHC.Rename.Expr where
import GHC.Types.Name
import GHC.Hs
import GHC.Types.Name.Set ( FreeVars )
import GHC.Tc.Types
import GHC.Types.SrcLoc   ( Located )
import GHC.Utils.Outputable  ( Outputable )

import Control.Monad.Trans.Writer ( WriterT (..) )

rnLExpr :: LHsExpr GhcPs
        -> WriterT FreeVars RnM (LHsExpr GhcRn)

rnStmts :: --forall thing body.
           Outputable (body GhcPs) => HsStmtContext GhcRn
        -> (Located (body GhcPs) -> WriterT FreeVars RnM (Located (body GhcRn)))
        -> [LStmt GhcPs (Located (body GhcPs))]
        -> ([Name] -> WriterT FreeVars RnM a)
        -> WriterT FreeVars RnM ([LStmt GhcRn (Located (body GhcRn))], a)
