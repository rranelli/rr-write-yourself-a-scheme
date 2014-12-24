module SchemeEvaluator (eval) where

import SchemeValue
import SchemeError
import SchemePrimitives
import SchemeParser

import Control.Monad.Error

eval :: LispVal -> ThrowsError LispVal
eval val @ (String _) = return val
eval val @ (Number _) = return val
eval val @ (Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, conseq, alt]) = do result <- eval predicate
                                                     case result of
                                                        Bool False -> eval alt
                                                        otherwise -> eval conseq
eval (List (func : args)) = mapM eval args >>= apply func

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (Atom func) args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)
