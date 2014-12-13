module Main where

import System.Environment

import SchemeEvaluator
import SchemeReader

main :: IO ()
main = getArgs >>= \x -> putStrLn . show . eval . readExpr . (!!0) $ x
