{-#LANGUAGE DoAndIfThenElse #-}
module SchemeRepl (runRepl, evalAndPrint) where

import System.IO
import Control.Monad.Error

import SchemeError
import SchemeReader
import SchemeEvaluator

flushStr :: String -> IO ()
flushStr = \str -> putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt = \prompt -> flushStr prompt >> getLine

evalString :: String -> IO String
evalString = \expr -> return . extractValue . trapError $ (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint = \expr -> evalString expr >>= putStrLn

until_ :: (String -> Bool) -> IO String -> (String -> IO ()) -> IO ()
until_ haltp prompt action = do result <- prompt
                                if haltp result
                                then return ()
                                else (action result) >> until_ haltp prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp >>> ") evalAndPrint
