module Main where

import System.Environment
import SchemeRepl

main :: IO ()
main = do args <- getArgs
          case length args of
           0 -> runRepl
           1 -> evalAndPrint $ args !! 0
           _ -> putStrLn "Program takes only 0 or 1 argument"
