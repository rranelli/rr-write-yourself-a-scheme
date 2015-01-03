module SchemeEvaluator (
  Env,
  eval,
  nullEnv,
  runIOThrows,
  liftThrows
  ) where

import SchemeValue
import SchemeError
import SchemePrimitives

import Control.Monad.Error
import Data.IORef

eval :: Env -> LispVal -> IOThrowsError LispVal
eval ___ val @ (String _) = return val
eval ___ val @ (Number _) = return val
eval ___ val @ (Bool _) = return val
eval env (Atom ref) = getVar env ref
eval ___ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do result <- eval env predicate
                                                         case result of
                                                              Bool False -> eval env alt
                                                              _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

eval env (List (func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval ___ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (Atom func) args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                               ($ args)
                               (lookup func primitives)

-- environment handling

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
