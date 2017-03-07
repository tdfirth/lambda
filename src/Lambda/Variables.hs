module Lambda.Variables where

-- Local Imports
import Lambda.Types

-- Global Imports
import Data.IORef

-- Environment Definition
type Env = IORef [(String, IORef LispVal)]


