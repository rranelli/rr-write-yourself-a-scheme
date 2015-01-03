module SchemeValue (
  Env,
  LispVal (
    Atom,
    Number,
    Bool,
    String,
    List,
    DottedList),
  LispError (
    NotFunction,
    NumArgs,
    TypeMismatch,
    Parser,
    BadSpecialForm,
    UnboundVar),
  extractValue,
  trapError,
  unwordsList,
  ThrowsError,
  nullEnv,
  runIOThrows,
  liftThrows,
  defineVar,
  getVar,
  setVar,
  IOThrowsError,
  ) where

import Data.IORef
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }

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

-- environment

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows = \action -> runErrorT (trapError action) >>= (return . extractValue)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound = \envRef var -> readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar = \envRef var -> do env <- liftIO $ readIORef envRef
                           maybe (throwError $ UnboundVar "Getting an unbound var" var)
                                 (liftIO . readIORef)
                                 (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar = \envRef var value -> do env <- liftIO $ readIORef envRef
                                 maybe (throwError $ UnboundVar "Setting unbound variable" var)
                                       (liftIO . (flip writeIORef value))
                                       (lookup var env)
                                 return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar = \envRef var value -> do alreadyDefined <- liftIO $ isBound envRef var
                                    if alreadyDefined
                                       then setVar envRef var value >> return value
                                       else liftIO $ do
                                          valueRef <- newIORef value
                                          env <- readIORef envRef
                                          writeIORef envRef ((var, valueRef) : env)
                                          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
                           where
                             extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
                             addBinding (var, value) = do ref <- newIORef value
                                                          return (var, ref)

-- error handling

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " , found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
