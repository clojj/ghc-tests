{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line argument
module Main where

import Data.Data
import Data.List
import GHC
import DynFlags
import MonadUtils
import Outputable
import ApiAnnotation
import System.Environment( getArgs )
import qualified Data.Map as Map
import qualified Data.Set as Set

import GHC.SYB.Utils

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "Test"

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir fileName = do
       ((anns,cs),p) <- runGhc (Just libdir) $ do

                        dflags <- getSessionDynFlags
                        let dflags' = gopt_set dflags Opt_KeepRawTokenStream

                        setSessionDynFlags dflags'
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
                        let ts=typecheckedSource l
                            r =renamedSource l
                        return (pm_annotations p,p)

       putStrLn $ "---pm_parsed_source---------------------"
       liftIO (putStr $ showData Parser 2 $ pm_parsed_source p)

       let spans = Set.fromList $ getAllSrcSpans (pm_parsed_source p)

       -- putStrLn (pp spans)
           problems = filter (\(s,a) -> not (Set.member s spans))
                             $ getAnnSrcSpans (anns,cs)
       putStrLn "---Problems---------------------"
       putStrLn (intercalate "\n" [showAnns $ Map.fromList $ map snd problems])
       putStrLn "--------------------------------"
       putStrLn (intercalate "\n" [showAnns anns])
       putStrLn "---comments---------------------"
       let annsComments = pm_annotations p
       putStrLn (intercalate "\n" [showAnnsComments annsComments])

    where
      getAnnSrcSpans :: ApiAnns -> [(SrcSpan,(ApiAnnKey,[SrcSpan]))]
      getAnnSrcSpans (anns,_) = map (\a@((ss,_),_) -> (ss,a)) $ Map.toList anns

      getAllSrcSpans :: (Data t) => t -> [SrcSpan]
      getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
        where
          getSrcSpan :: SrcSpan -> [SrcSpan]
          getSrcSpan ss = [ss]


showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\((s,k),v)
              -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n"))
   $ Map.toList anns)
    ++ "]\n"

pp a = showPpr unsafeGlobalDynFlags a

--
showAnnsComments (_,anns) =
  "[\n" ++
  (intercalate "\n" $
   map (\(s,v) -> ("( " ++ show s ++ " =\n[" ++ showToks v ++ "])\n")) $
   Map.toList anns) ++
  "]\n"

showToks ts =
  intercalate ",\n\n" $
  map (\(L p t) -> "(" ++ show p ++ "," ++ show t ++ ")") ts


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
