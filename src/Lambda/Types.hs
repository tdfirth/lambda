module Lambda.Types where

-- Global Imports
import Data.Ratio
import Data.Complex
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error
import Data.IORef
import System.IO

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
showVal (Func {params = args,
               vararg = varargs,
               body = body,
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

-- Environment Definition
type Env = IORef [(String, IORef LispVal)]

-- Create empty Environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Environment Errors
type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
-- Note that this type signature is not general...
-- try (MonadError m a) => Either e a -> m a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- Main setting and getting functionality
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


