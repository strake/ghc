{-# LANGUAGE CPP #-}

module TcSplice where
import HsSyn    ( HsSplice, HsBracket, HsQuasiQuote,
                  HsExpr, LHsType, LHsExpr, LPat, LHsDecl )
import HsExpr   ( PendingRnSplice )
import Name     ( Name )
import RdrName  ( RdrName )
import TcRnTypes( TcM, TcId )
import TcType   ( TcRhoType )
import Annotations ( Annotation, CoreAnnTarget )

#ifdef GHCI
import Id       ( Id )
import qualified Language.Haskell.TH as TH
#endif

tcSpliceExpr :: HsSplice Name
             -> TcRhoType
             -> TcM (HsExpr TcId)

tcUntypedBracket :: HsBracket Name
                 -> [PendingRnSplice]
                 -> TcRhoType
                 -> TcM (HsExpr TcId)
tcTypedBracket :: HsBracket Name
               -> TcRhoType
               -> TcM (HsExpr TcId)

runQuasiQuoteDecl :: HsQuasiQuote RdrName -> TcM [LHsDecl RdrName]
runQuasiQuoteExpr :: HsQuasiQuote RdrName -> TcM (LHsExpr RdrName)
runQuasiQuoteType :: HsQuasiQuote RdrName -> TcM (LHsType RdrName)
runQuasiQuotePat  :: HsQuasiQuote RdrName -> TcM (LPat RdrName)
runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

#ifdef GHCI
tcTopSpliceExpr :: Bool -> TcM (LHsExpr Id) -> TcM (LHsExpr Id)

runMetaE :: LHsExpr Id -> TcM (LHsExpr RdrName)
runMetaP :: LHsExpr Id -> TcM (LPat RdrName)
runMetaT :: LHsExpr Id  -> TcM (LHsType RdrName)
runMetaD :: LHsExpr Id -> TcM [LHsDecl RdrName]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
#endif
