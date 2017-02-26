module Main where

-- Local Imports
import Lambda.ReadExpression
import Lambda.Error
-- Global Imports
import System.Environment
import Control.Monad

main :: IO()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
