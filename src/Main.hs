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
          if null args then runRepl else runOne $ args

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

  -- loop = primitiveBindings >>= until_ (== "quit") (readPrompt "lambda> ") . evalAndPrint
{-
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action
-}

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

-- Helper Functions
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
  (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
