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
import Control.Monad.State.Strict

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runProgram $ args

-- Type definitions and start state (primitives).
type REPL  = StateT Env IO
startState = primitiveBindings

-- Run the interactive REPL.
runRepl :: IO ()
runRepl = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    input <- getInputLine "lambda> "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just input -> (outputStrLn $ "Input was: " ++ input) >> loop
      -- Just input -> evaluate input >> loop

-- Executed a single program file as specified on the command line.
runProgram :: [String] -> IO ()
runProgram args = do
  env <- startState >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

-- Helper Functions
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
