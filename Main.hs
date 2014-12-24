module Main where

import System.Environment
import Control.Monad.Error

import SchemeValue
import SchemeError
import SchemeParser
import SchemeReader
import SchemeEvaluator

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
