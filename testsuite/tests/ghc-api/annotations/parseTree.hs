{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import Data.List (intercalate)
import System.IO
import GHC
import GHC.Types.Basic
import GHC.Driver.Session
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Outputable.Ppr (showPprDefault)
import GHC.Data.Bag (isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "AnnotationTuple"

testOneFile libdir fileName = do
       p <- runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        setSessionDynFlags dflags
                        let mn =mkModuleName fileName
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        load LoadAllTargets
                        modSum <- getModSummary mn
                        p <- parseModule modSum
                        t <- typecheckModule p
                        d <- desugarModule t
                        l <- loadModule d
                        return p

       let anns = pm_annotations p
           ann_items = apiAnnItems anns
           ann_eof = apiAnnEofPos anns
       let tupArgs = gq (pm_parsed_source p)

       putStrLn (pp tupArgs)
       putStrLn (intercalate "\n" [showAnns ann_items, "EOF: " ++ show ann_eof])

    where
     gq ast = everything (++) ([] `mkQ` doLHsTupArg) ast

     doLHsTupArg :: LHsTupArg GhcPs -> [(SrcSpan,String,HsExpr GhcPs)]
     doLHsTupArg (L l arg@(Present {}))
                                = [(l,"p",ExplicitTuple noExtField [L l arg] Boxed)]
     doLHsTupArg (L l arg@(Missing {}))
                                = [(l,"m",ExplicitTuple noExtField [L l arg] Boxed)]


showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\((s,k),v)
              -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n"))
   $ Map.toList anns)
    ++ "]\n"

pp = showPprDefault


-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r



-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results

everything k f x = foldl k (f x) (gmapQ (everything k f) x)
