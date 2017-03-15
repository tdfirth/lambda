module Main where

-- Local Imports
import Lambda.Types
import Lambda.ReadExpression
import Lambda.Primitives
-- Global Imports
import System.Environment
import System.IO
import Control.Monad
import System.Console.Haskeline
import Control.Monad.Trans (lift)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runProgram $ args

-- Set up main loop
runRepl :: IO ()
runRepl = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    input <- getInputLine "lambda> "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just input -> (lift $ primitiveBindings >>= flip evalAndPrint input) >> loop

-- Executed a single program file as specified on the command line.
runProgram :: [String] -> IO ()
runProgram args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

-- Helper Functions
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
