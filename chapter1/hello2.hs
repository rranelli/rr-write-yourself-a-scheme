module Main where
import System.Environment

main :: IO ()
main = do args <- getArgs
          putStrLn ("Sum of your stuff is " ++ show ((read $ args !! 0) + (read $ args !! 1)))
