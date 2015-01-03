module SchemeEvaluator (eval) where

import SchemeValue
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
