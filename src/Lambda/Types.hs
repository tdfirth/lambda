module Lambda.Types where

-- Global Imports
import Data.Ratio
import Data.Complex
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error
import Data.IORef
import System.IO
import qualified Data.Map.Strict as Map

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector [LispVal]
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | String String
  | Bool Bool
  | Character Char
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params  :: [String]
         , vararg  :: (Maybe String)
         , body    :: [LispVal]
         , closure :: Env
         }

instance Show LispVal where show = showVal
-- This feels like a hack...not sure why it is working or if it truly is
instance Eq LispVal where (==) = (==)

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents)      = "(" ++ unwordsList contents ++ ")"
showVal (IOFunc _)             = "<IO primitive>"
showVal (Port _)               = "<IO port>"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Func {params = args ,
vararg = varargs             ,
body = body                  ,
               closure = env}) = "(lambda (" ++ unwords (map show args) ++
                 (case varargs of
                    Nothing -> ""
                    Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- LispError Data Type and associated functions
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar msg var)          = msg ++ ": " ++ var
showError (BadSpecialForm msg form)     = msg ++ ": " ++ show form
showError (NotFunction msg func)        = msg ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type.  Expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default err)                 = "Unknown error: " ++ show err

instance Show LispError where show = showError

type ThrowsError = Either LispError

-- Helper Functions for Parsing
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Environment Errors
type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
-- Note that this type signature is not general...
-- try (MonadError m a) => Either e a -> m a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- Environment Definition
type Env = IORef (Map.Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef Map.empty

isBound :: Env -> String -> IO Bool
isBound e s = readIORef e >>= return . Map.member s

getVar :: Env -> String -> IOThrowsError LispVal
getVar e s = do
  env <- liftIO $ readIORef e
  case Map.lookup s env of
    Just v  -> liftIO $ readIORef v
    Nothing -> throwError $ UnboundVar "Trying to get an unbound variable..." s

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar e s l = do
  env <- liftIO $ readIORef e
  case Map.lookup s env of
    Just v  -> liftIO $ writeIORef v l
    Nothing -> throwError $ UnboundVar "Trying to update an unbound variable.." s
  return l

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar e s l = do
  defined <- liftIO $ isBound e s
  if defined then setVar e s l
  else liftIO $ do
    newVar <- newIORef l
    modifyIORef' e $ Map.insert s newVar
    return l

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars e ls = do
  updates <- mapM mkRef ls >>= return . Map.fromList
  modifyIORef' e (Map.union updates)
  return e
    where
      mkRef :: (String, LispVal) -> IO (String, IORef LispVal)
      mkRef (s, l) = newIORef l >>= return . (,) s

showEnv :: Env -> IO String
showEnv e = do
  env <- readIORef e
  vars <- return $ Map.toList env
  (sequence $ map printOne vars) >>= return . unlines
    where
      printOne :: (String, IORef LispVal) -> IO String
      printOne (s, l) = do
        lispval <- readIORef l
        return $ s ++ " : " ++ show lispval
