module Main where

import           System.IO
import System.Environment( getArgs )

import GHC
import GHC.Paths ( libdir )
import Lexer
import qualified MonadUtils as GMU
import StringBuffer
import FastString (mkFastString)
import SrcLoc
import ErrUtils (mkPlainErrMsg)

main :: IO()
main = do
        [name] <- getArgs
        contents <- readUTF8File name
        -- let contents="{-# LANGUAGE CPP #-}\nmodule Main where\nmain=undefined"

        runGhc (Just libdir) $ do
                flags <- getSessionDynFlags
                let sb = stringToStringBuffer contents
                let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1

                let prTS = lexTokenStream sb lexLoc flags
                case prTS of
                        POk _ toks    -> GMU.liftIO $ print $ map (show . unLoc) toks
                        PFailed l msg -> GMU.liftIO $ do
                          putStrLn "Lexer Error:"
                          print $ mkPlainErrMsg flags l msg

-- UTF8 utils {{{
readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h
