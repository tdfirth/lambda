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
          if null args then runRepl else runProgram args

-- Set up main loop
runRepl :: IO ()
runRepl = primitiveBindings >>= runInputT defaultSettings . loop where
  loop :: Env -> InputT IO ()
  loop state = do
    input <- getInputLine "lambda> "
    case input of
      Nothing -> return ()
      Just (':' : command) -> do
        action <- lift $ handleCommand command state
        case action of
          True  -> loop state
          False -> return ()
      Just input -> (lift $ evalAndPrint input state) >> loop state

-- Executed a single program file as specified on the command line.
runProgram :: [String] -> IO ()
runProgram args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

-- Helper Functions
evalAndPrint :: String -> Env -> IO ()
evalAndPrint expr env = evalString expr env >>= putStrLn

evalString :: String -> Env -> IO String
evalString expr env = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

handleCommand :: String -> Env -> IO Bool
handleCommand s e =
  case s of
    "q"    -> return False
    "quit" -> return False
    "h"    -> putStrLn "No help for you I'm afraid..." >> return True
    "help" -> putStrLn "No help for you I'm afraid..." >> return True
    "e"    -> showEnv e >>= putStrLn >> return True
    "env"  -> showEnv e >>= putStrLn >> return True

-- handleCommand ("env")  =
