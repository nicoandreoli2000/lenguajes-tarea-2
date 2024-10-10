{-# LANGUAGE CPP #-}
-- Federico Czarnievicz - 201250
module Env where

import AbsCPP
import PrintCPP
import ErrM

import Data.Map as Map

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
import Control.Monad hiding (fail)

fail = Bad
#endif

type Env = (Sig,[Context])
type Sig = Map Id ([Type],Type)
type Context = Map Id Type

lookupVar :: Env -> Id -> Err Type
lookupVar (sigma, []) id = fail $ "Variable " ++ show id ++ " not found"
lookupVar (sigma, x : xs) id = case Map.lookup id x of{
  Just t -> return t;
  Nothing -> lookupVar (sigma, xs) id;
}


lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sigma, context) id = case (Map.lookup id sigma) of{
  Just t -> return t;
  Nothing -> fail $ "Function " ++ show id ++ " not found";
}


updateVar :: Env -> Id -> Type -> Err Env
updateVar (sigma, []) id varType = fail $ "Empty context"
updateVar (sigma, x : xs) id varType = case Map.lookup id x of{
  Nothing -> return (sigma, (Map.insert id varType x) : xs);
  Just _ -> fail $ "Variable " ++ show id ++ "already exists";
}


updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sigma, context) id values = case Map.lookup id sigma of {
  Nothing -> return (Map.insert id values sigma, context);
  Just _ -> fail $ "Function " ++ show id ++ "already exists";
}


newBlock :: Env -> Env
newBlock (sigma, context) = (sigma, (empty) : context);

emptyEnv :: Env
emptyEnv = (empty, [empty]);
