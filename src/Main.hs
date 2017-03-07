module Main where

-- Local Imports
import Lambda.ReadExpression
import Lambda.Error
import Lambda.Variables
-- Global Imports
import System.Environment
import System.IO
import Control.Monad

main :: IO()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> runOne $ args !! 0
            otherwise -> putStrLn "Program only takes 0 or 1 argument!"

-- Set up main loop
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "lambda> ") . evalAndPrint

-- Helper Functions
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
  (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO()
evalAndPrint env expr = evalString env expr >>= putStrLn
