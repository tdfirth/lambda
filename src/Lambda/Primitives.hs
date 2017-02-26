{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Primitives where

-- Local Imports
import Lambda.Types
import Lambda.Error
-- Global Imports
import Control.Monad.Except (throwError, MonadError)

-- Primitives
primitives :: [( String, [LispVal] -> ThrowsError LispVal)]
primitives = [( "+", numericBinop (+)),
              ( "-", numericBinop (-)),
              ( "*", numericBinop (*)),
              ( "/", numericBinop div),
              ( "mod", numericBinop mod),
              ( "quotient", numericBinop quot),
              ( "remainder", numericBinop rem),
              ( "symbol?", isLispVal symbolp),
              ( "string?", isLispVal stringp),
              ( "number?", isLispVal numberp),
              ( "bool?", isLispVal boolp),
              ( "list?", isLispVal listp),
              ( "symbol->string", symbolToString),
              ( "string->symbol", stringToSymbol),
              ( "==", numEqBoolBinop (==)),
              ( "<", numOrdBoolBinop (<)),
              ( ">", numOrdBoolBinop (>)),
              ( "/=", numEqBoolBinop (/=)),
              ( ">=", numOrdBoolBinop (>=)),
              ( "<=", numOrdBoolBinop (<=)),
              ( "&&", boolBoolBinop (&&)),
              ( "||", boolBoolBinop (||)),
              ( "string=?", strBoolBinop (==)),
              ( "string<?", strBoolBinop (<)),
              ( "string>?", strBoolBinop (>)),
              ( "string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]

-- Binary Operations
numericBinop :: (MonadError LispError m) => (Integer -> Integer -> Integer) -> [LispVal] -> m LispVal
numericBinop op []           = throwError $ NumArgs 2 []
numericBinop op singeVal@[_] = throwError $ NumArgs 2 singeVal
numericBinop op params       = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (MonadError LispError m) => (LispVal -> m a) -> (a -> a -> Bool) -> [LispVal] -> m LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numEqBoolBinop :: (MonadError LispError m) => (Integer -> Integer -> Bool) -> [LispVal] -> m LispVal
numEqBoolBinop  = boolBinop unpackNum

numOrdBoolBinop :: (MonadError LispError m) => (Integer -> Integer -> Bool) -> [LispVal] -> m LispVal
numOrdBoolBinop  = boolBinop unpackNum

-- eqBoolBinop :: (MonadError LispError m) => (forall a. Eq a => a -> a -> Bool) -> [LispVal] -> m LispVal
-- eqBoolBinop f (Number a) (Number b) = return $ Bool (f a b)


boolBoolBinop :: (MonadError LispError m) => (Bool -> Bool -> Bool) -> [LispVal] -> m LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (MonadError LispError m) => (String -> String -> Bool) -> [LispVal] -> m LispVal
strBoolBinop = boolBinop unpackStr

-- Unary Operations
isLispVal :: (MonadError LispError m) => (LispVal -> Bool) -> [LispVal] -> m LispVal
isLispVal f (l:[])     = return $ Bool $ f l
isLispVal _ badArgList = throwError $ NumArgs 2 badArgList

symbolp, stringp, numberp, boolp, listp :: LispVal -> Bool
symbolp (Atom _)       = True
symbolp _              = False

stringp (String _)     = True
stringp _              = False

numberp (Number _)     = True
numberp _              = False

boolp (Bool _)         = True
boolp _                = False

listp (List _)         = True
listp (DottedList _ _) = True
listp _                = False

symbolToString, stringToSymbol :: (MonadError LispError m) => [LispVal] -> m LispVal
symbolToString (Atom s:[])  = return $ String s
symbolToString (e:[])       = throwError $ TypeMismatch "symbol" e
symbolToString e            = throwError $ NumArgs 1 e

stringToSymbol (Atom s:[])  = return $ Atom s
stringToSymbol (e:[])       = throwError $ TypeMismatch "string" e
stringToSymbol e            = throwError $ NumArgs 1 e

-- Unpack LispVals
unpackNum :: (MonadError LispError m) => LispVal -> m Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: (MonadError LispError m) => LispVal -> m Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackStr :: (MonadError LispError m) => LispVal -> m String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString
