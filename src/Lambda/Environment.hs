{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Lambda.Environment where

import qualified Data.Map.Strict as Map
import           Data.IORef
import           Control.Monad.Except
import           System.IO
import           Lambda.Types hiding (Env, isBound, getVar, setVar, defineVar, bindVars)

type Environment = IORef (Map.Map String (IORef LispVal))

nullEnv :: IO Environment
nullEnv = newIORef Map.empty

isBound :: Environment -> String -> IO Bool
isBound e s = readIORef e >>= return . Map.member s

getVar :: Environment -> String -> IOThrowsError LispVal
getVar e s = do
  env <- liftIO $ readIORef e
  case Map.lookup s env of
    Just v  -> liftIO $ readIORef v
    Nothing -> throwError $ UnboundVar "Trying to get an unbound variable..." s

updateVar :: Environment -> String -> LispVal -> IOThrowsError LispVal
updateVar e s l = do
  env <- liftIO $ readIORef e
  case Map.lookup s env of
    Just v  -> liftIO $ writeIORef v l
    Nothing -> throwError $ UnboundVar "Trying to update an unbound variable.." s
  return l

defineVar :: Environment -> String -> LispVal -> IOThrowsError LispVal
defineVar e s l = do
  defined <- liftIO $ isBound e s
  if defined then updateVar e s l
  else liftIO $ do
    newVar <- newIORef l
    modifyIORef' e $ Map.insert s newVar
    return l

bindVars :: Environment -> [(String, LispVal)] -> IO Environment
bindVars e ls = do
  updates <- mapM mkRef ls >>= return . Map.fromList
  modifyIORef' e (Map.union updates)
  return e
    where
      mkRef :: (String, LispVal) -> IO (String, IORef LispVal)
      mkRef (s, l) = newIORef l >>= return . (,) s
