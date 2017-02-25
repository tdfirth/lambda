module Lambda.ReadExpression
  ( symbol
  , readExpr
  , eval
  ) where

-- Local Imports
import           Lambda.Error
import           Lambda.Types
import           Lambda.Primitives
import           Lambda.Parse
-- Global Imports
import           Text.ParserCombinators.Parsec hiding (spaces)

-- Evaluate
eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

-- Apply
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- Main read function
readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val
