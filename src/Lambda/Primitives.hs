module Lambda.Primitives where

import Lambda.Types

-- Primitives
primitives :: [( String, [LispVal] -> LispVal)]
primitives = [( "+", numericBinop (+)),
              ( "-", numericBinop (-)),
              ( "*", numericBinop (*)),
              ( "/", numericBinop div),
              ( "mod", numericBinop mod),
              ( "quotient", numericBinop quot),
              ( "remainder", numericBinop rem),
              ( "symbol?", unaryOp symbolp),
              ( "string?", unaryOp stringp),
              ( "number?", unaryOp numberp),
              ( "bool?", unaryOp boolp),
              ( "list?", unaryOp listp),
              ( "symbol->string", unaryOp symbol2string),
              ( "string->symbol", unaryOp string2symbol)
             ]

-- Primitives Helper Functions
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, stringp, numberp, boolp, listp :: LispVal -> LispVal

symbolp (Atom _)       = Bool True
symbolp _              = Bool False

stringp (String _)     = Bool True
stringp _              = Bool False

numberp (Number _)     = Bool True
numberp _              = Bool False

boolp (Bool _)         = Bool True
boolp _                = Bool False

listp (List _)         = Bool True
listp (DottedList _ _) = Bool True
listp _                = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String a) = Atom a
string2symbol _          = Atom ""


