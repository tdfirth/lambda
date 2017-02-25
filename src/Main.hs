module Main where
import System.Environment
import Lambda.ReadExpression

main :: IO()
main = getArgs >>= print . eval . readExpr . head
