module GHC.IfaceToCore where

import GHC.Prelude
import GHC.Iface.Syntax ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule
                        , IfaceAnnotation, IfaceCompleteMatch )
import GHC.Types.TyThing   ( TyThing )
import GHC.Tc.Types        ( IfL )
import GHC.Core.InstEnv    ( ClsInst )
import GHC.Core.FamInstEnv ( FamInst )
import GHC.Core         ( CoreRule )
import GHC.Types.CompleteMatch ( CompleteMatch )
import GHC.Types.Annotations ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
