module SchemeReader (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error

import SchemeParser
import SchemeValue
import SchemeError

readExpr :: String -> ThrowsError LispVal
readExpr = \input -> case parse parseExpr "lisp" input of
                      Left err -> throwError $ Parser err
                      Right val -> return val
