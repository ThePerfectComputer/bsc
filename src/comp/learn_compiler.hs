module Main_learn_parser(main) where

import System.Environment
import qualified Control.Exception as CE

import FStringCompat
import Parse
import Parser.Classic(pPackage, errSyntax)
import PVPrint
import CVPrint()
import Lex
import Error(internalError, showErrorList)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> getContents >>= bsc2bsv "-"
         [fn] -> readFile fn >>= bsc2bsv fn
         _ -> error "usage: bsc2bsv filename"

-- get_tokens "/Users/yehowshuaimmanuel/git/bsc-yehowshua/testsuite/bsc.typechecker/deriving/scope/AbstractList.bs"
getTokens :: FilePath -> IO [Token]
getTokens fn = 
  do 
    readFile fn >>= bscTokens fn

bscTokens :: Monad m => String -> String -> m [Token]
bscTokens filename text =
  return $ lexStart lflags (mkFString filename) text
  where lflags = LFlags { lf_is_stdlib = False,
                             lf_allow_sv_kws = True}

bsc2bsv :: String -> String -> IO ()
bsc2bsv filename text =
    do let lflags = LFlags { lf_is_stdlib = False,
                             lf_allow_sv_kws = True }
           tokens = lexStart lflags (mkFString filename) text
       case parse pPackage tokens of
         Left  (ss, tokens') -> let es = [errSyntax [s | s@(_:_) <- ss] tokens']
                                in  CE.throw $ CE.ErrorCall (showErrorList es)
         Right ((package,_):_) ->
          --  putStrLn $ pvpReadable package
           putStrLn $ show package
         Right [] -> internalError "bsc2bsv: parse succeeded with no packages"
