module Lambda.Error where

-- Local Imports
import Lambda.Types
-- Global Imports
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

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

instance Show LispError where show = showError

type ThrowsError = Either LispError

-- Helper Functions for Parsing
trapError action = catchError action (return . show)

