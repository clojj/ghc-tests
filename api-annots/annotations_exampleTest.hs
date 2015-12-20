{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Data
import Data.List
import GHC
import DynFlags
import Outputable
import ApiAnnotation
import System.Environment( getArgs )
import qualified Data.Map as Map
import qualified Data.Set as Set

import GHC.SYB.Utils

main::IO()
main = do
        [libdir, name] <- getArgs
        testOneFile libdir name

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir module_name = do
       ((anns, cs), p, ts, r) <- runGhc (Just libdir) $ do

                        dflags <- getSessionDynFlags
                        let dflags' = gopt_set dflags Opt_KeepRawTokenStream

                        _ <- setSessionDynFlags dflags'

                        let mn = mkModuleName module_name
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        _ <- load LoadAllTargets
                        modSum <- getModSummary mn

                        p <- parseModule modSum
                        t <- typecheckModule p
                        d <- desugarModule t
                        lm <- loadModule d
                        let ts = typecheckedSource lm
                            r  = renamedSource lm
                        return (pm_annotations p, p, ts, r)

       let parsed_source = pm_parsed_source p

       putStrLn "---showData parsed_source---------------"
       putStrLn $ showData Parser 2 parsed_source
       putStrLn "---pp parsed_source---------------------"
       putStrLn $ pp parsed_source
       putStrLn "---pp renamed_source--------------------"
       putStrLn $ pp r

       putStrLn "---typecheckedSource--------------------"
       putStrLn $ pp ts

       let src_spans = Set.fromList $ getAllSrcSpans parsed_source

           problems = filter (\(s, _) -> not (Set.member s src_spans))
                             $ getAnnSrcSpans (anns,cs)

       putStrLn "---src_spans--------------------"
       putStrLn $ pp src_spans

       putStrLn "---Problems---------------------"
       putStrLn (intercalate "\n" [showAnns $ Map.fromList $ map snd problems])

       putStrLn "---ApiAnns----------------------"
       putStrLn (intercalate "\n" [showAnns anns])

       putStrLn "---ApiAnns Comments-------------"
       putStrLn (intercalate "\n" [showAnnsComments (anns, cs)])

    where
      getAnnSrcSpans :: ApiAnns -> [(SrcSpan,(ApiAnnKey,[SrcSpan]))]
      getAnnSrcSpans (anns,_) = map (\a@((ss,_),_) -> (ss,a)) $ Map.toList anns

      getAllSrcSpans :: (Data t) => t -> [SrcSpan]
      getAllSrcSpans = everything (++) ([] `mkQ` getSrcSpan)
        where
          getSrcSpan :: SrcSpan -> [SrcSpan]
          getSrcSpan ss = [ss]

showAnns :: Map.Map ApiAnnKey [SrcSpan] -> String
showAnns anns =
  "[\n" ++
  intercalate "\n" (map (\((s,k),v) -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n")) $ Map.toList anns)
  ++ "]\n"

pp :: forall a. Outputable a => a -> String
pp = showPpr unsafeGlobalDynFlags

--
showAnnsComments :: forall t a a1 a2.
                      (Show a, Show a1, Show a2) =>
                      (t, Map.Map a [GenLocated a1 a2]) -> String
showAnnsComments (_, anns) =
  "[\n" ++
  intercalate "\n" (map (\(s,v) -> ("( " ++ show s ++ " =\n[" ++ showToks v ++ "])\n")) $ Map.toList anns)
  ++ "]\n"

showToks :: forall a a1.
              (Show a, Show a1) =>
              [GenLocated a a1] -> String
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
