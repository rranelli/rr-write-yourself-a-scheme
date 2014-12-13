module SchemeReader where

import Text.ParserCombinators.Parsec hiding (spaces)
import SchemeParser

instance Show LispVal where show = showVal

readExpr :: String -> LispVal
readExpr = \input -> case parse parseExpr "lisp" input of
                      Left err -> String $ "No match: " ++ show err
                      Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList left right) = "(" ++ unwordsList left ++ " . " ++ showVal right ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = \xs -> unwords . map showVal $ xs
