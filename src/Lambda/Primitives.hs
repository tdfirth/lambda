{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Primitives where

-- Local Imports
import Lambda.Types
import Lambda.ReadExpression
import Lambda.Utils
-- Global Imports
import Control.Monad.Except
import System.IO

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                             ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

-- IO Primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = writeProc [Port stdout]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

{-
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
-}
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

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
              ( "=", numEqBoolBinop (==)),
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
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

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

numOrdBoolBinop :: (MonadError LispError m) => (forall a. Ord a => a -> a -> Bool) -> [LispVal] -> m LispVal
numOrdBoolBinop  = boolBinop unpackNum

numEqBoolBinop :: (MonadError LispError m) => (forall a. Eq a => a -> a -> Bool) -> [LispVal] -> m LispVal
numEqBoolBinop = boolBinop unpackNum

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

-- List Primitives
car :: (MonadError LispError m) => [LispVal] -> m LispVal
car[List (x : xs)]         = return x
car[DottedList (x : xs) _] = return x
car[badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: (MonadError LispError m) => [LispVal] -> m LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: (MonadError LispError m) => [LispVal] -> m LispVal
cons [x, List []]             = return $ List [x]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xLast] = return $ DottedList (x : xs) xLast
cons [x, y]                   = return $ DottedList [x] y
cons badArgList               = throwError $ NumArgs 2 badArgList

-- Equivalence Primitives
{-
eqv :: (MonadError LispError m) => [LispVal] -> m LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                        (all eqvPair $ zip arg1 arg2)
                                                          where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                                                     Left err -> False
                                                                                     Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList
-}

equal :: (MonadError LispError m) => [LispVal] -> m LispVal
equal [a, b] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals a b)
                           [AnyUnpacker unpackNum, AnyUnpacker unpackBool, AnyUnpacker unpackStr]
  eqvEquals <- eqv [a, b]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Predicate Primitives
-- caseExp :: (MonadError LispError m) => [LispVal] -> m LispVal
-- caseExp (key : expressions) = do

-- Unpack LispVals
data Unpacker m = forall a. Eq a => AnyUnpacker (LispVal -> m a)

unpackEquals :: (MonadError LispError m) => LispVal -> LispVal -> Unpacker m -> m Bool
unpackEquals a b (AnyUnpacker unpacker) = do
               unpacked1 <- unpacker a
               unpacked2 <- unpacker b
               return $ unpacked2 == unpacked2
        `catchError` (const $ return False)

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
