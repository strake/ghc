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
import GHC.Parser.Annotation
import GHC.Data.Bag (isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import System.Exit
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "ListComprehensions"
        exitSuccess

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
       let spans = Set.fromList $ getAllSrcSpans (pm_parsed_source p)

       putStrLn (pp spans)
       putStrLn "--------------------------------"
       putStrLn (intercalate "\n" [showAnns ann_items,"EOF: " ++ show ann_eof])

    where
      getAnnSrcSpans :: ApiAnns -> [(RealSrcSpan,(ApiAnnKey,[RealSrcSpan]))]
      getAnnSrcSpans anns = map (\a@((ss,_),_) -> (ss,a)) $ Map.toList (apiAnnItems anns)

      getAllSrcSpans :: (Data t) => t -> [RealSrcSpan]
      getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
        where
          getSrcSpan :: SrcSpan -> [RealSrcSpan]
          getSrcSpan (RealSrcSpan ss _) = [ss]
          getSrcSpan (UnhelpfulSpan _) = []

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
