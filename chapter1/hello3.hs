module Main where
import System.Environment

main :: IO ()
main = do name <- getLine -- this whole thing here is a shorthand for monadic sequencing
          putStrLn $ "Hello, " ++ name
