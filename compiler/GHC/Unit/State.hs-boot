module GHC.Unit.State where
import GHC.Data.FastString
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, Unit, UnitId)
data PackageState
data UnitInfoMap
data PackageDatabase unit
emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
pprUnitIdForUser :: PackageState -> UnitId -> SDoc
improveUnit :: UnitInfoMap -> Unit -> Unit
unitInfoMap :: PackageState -> UnitInfoMap
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
