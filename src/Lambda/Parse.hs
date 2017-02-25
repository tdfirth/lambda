module Lambda.Parse where

-- Local Imports
import Lambda.Types
-- Global Imports
import Control.Monad
import Control.Monad.Except
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Ratio
import Data.Complex

-- Symbol and definitions
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                             '\\' -> x
                             '"'  -> x
                             'n'  -> '\n'
                             'r'  -> '\r'
                             't'  -> '\t'

-- Parsing
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

oct2dig x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

bin2dig  = bin2dig' 0
bin2dig' digint ""     = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                               bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal1)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal1)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble(Float f)  = realToFrac f
toDouble(Number n) = fromIntegral n

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
          <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
  return $ Character $ case value of
                         "space"   -> ' '
                         "newline" -> '\n'
                         otherwise -> (value !! 0)

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseFloat
  <|> try parseRatio
  <|> try parseComplex
  <|> try parseNumber
  <|> try parseBool
  <|> try parseCharacter
  <|> do char '('
         x <- parseList <|> parseDottedList
         char ')'
         return x


