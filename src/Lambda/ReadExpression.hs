module Lambda.ReadExpression
  ( symbol
  , readExpr
  , readExprList
  , eval
  , apply
  ) where

-- Local Imports
import Lambda.Types
import Lambda.Parse
import Lambda.Utils
-- Global Imports
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

-- Apply
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
       where remainingArgs       = drop (length params) args
             num                 = toInteger . length
             evalBody env        = liftM last $ mapM (eval env) body
             bindVarArgs arg env = case arg of
                 Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                 Nothing -> return env
apply e _ = throwError $ TypeMismatch "function" e

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

eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body

eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)

eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

-- Helper Functions
makeFunc varargs env params body = return $ Func(map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal
