{-#LANGUAGE DoAndIfThenElse #-}
module SchemeRepl (runRepl, runOne, evalAndPrint) where

import System.IO
import Control.Monad.Error

import SchemeError
import SchemeReader
import SchemeEvaluator

flushStr :: String -> IO ()
flushStr = \str -> putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt = \prompt -> flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString = \env expr -> runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint = \env expr -> evalString env expr >>= putStrLn

until_ :: (String -> Bool) -> IO String -> (String -> IO ()) -> IO ()
until_ haltp prompt action = do result <- prompt
                                if haltp result
                                then return ()
                                else (action result) >> until_ haltp prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= (until_ (== "quit") (readPrompt "Lisp >>> ")) . evalAndPrint
