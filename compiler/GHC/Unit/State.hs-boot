module GHC.Unit.State where
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Types (Unit, UnitId)
data PackageState
data UnitInfoMap
data PackageDatabase unit
emptyPackageState :: PackageState
pprUnitIdForUser :: PackageState -> UnitId -> SDoc
improveUnit :: UnitInfoMap -> Unit -> Unit
unitInfoMap :: PackageState -> UnitInfoMap
pprWithUnitState :: PackageState -> SDoc -> SDoc
