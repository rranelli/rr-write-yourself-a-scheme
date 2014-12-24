module SchemeValue (
  LispVal (
     Atom,
     Number,
     Bool,
     String,
     List,
     DottedList),
  unwordsList
  ) where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Eq)

instance Show LispVal where show = showVal

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
