module Lambda.ReadExpression
  ( symbol
  , readExpr
  , eval
  ) where

-- Local Imports
import Lambda.Error
import Lambda.Types
import Lambda.Primitives
import Lambda.Parse
-- Global Imports
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

-- Apply
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognised primitive function args" func)
        ($ args)
        (lookup func primitives)

-- Evaluate
eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm                    = throwError $
                                  BadSpecialForm "Unrecognised special form" badForm

-- Main read function
readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
