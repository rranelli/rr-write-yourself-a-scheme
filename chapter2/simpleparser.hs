module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

-- exercises
-- 1x,2x,3x,4o,5x,6,7

main :: IO ()
main = do arg : _ <- getArgs
          putStrLn (readExpr arg)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Char Char
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr = \input -> case parse parseExpr "lisp" input of
                      Left err -> "No match: " ++ show err
                      Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    x <- anyChar
                    return $ Char x

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (\x -> return $ Number (read x))
-- parseNumber = do x <- (many1 digit)
--                  return $ Number (read x)

-- parseFloat :: Parser LispVal
-- parseFloat = do whole <- (many1 digit)
--                 char '.'
--                 frac <- (many1 digit)
--                 let num = whole ++ "." ++ frac
--                 return $ case (readFloat num) of
--                           [] -> []
--                           [(x, _)] -> Float x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseCharacter
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
