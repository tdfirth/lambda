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
import Lambda.Variables
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
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val

eval env val@(Number _) = return val

eval env val@(Bool _)   = return val

eval env (Atom id) = getVar env id

eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
          Bool False -> eval env alt
          otherwise  -> eval env conseq

eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
     then throwError $ BadSpecialForm "No true clause in cond expression: " form
     else case head clauses of
            List [Atom "else", expr] -> eval env expr
            List [test, expr] -> eval env $
              List [Atom "if", test, expr, List(Atom "cond" : tail clauses)]
            _ -> throwError $ BadSpecialForm "Ill formed cond expression: " form

eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
     then throwError $ BadSpecialForm "No true clause in case expression: " form
     else case head clauses of
            List (Atom "else" : exprs)   -> mapM (eval env) exprs >>= return . last
            List ((List datums) : exprs) -> do
              result <- eval env key
              equality <- mapM (\x -> eqv[result, x]) datums
              if Bool True `elem` equality
                 then mapM (eval env) exprs >>= return . last
                 else eval env $ List (Atom "case" : key : tail clauses)
            _ -> throwError $ BadSpecialForm "Ill formed case expression: " form

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func

eval env badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

-- Main read function
readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
