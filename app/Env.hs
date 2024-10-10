{-# LANGUAGE CPP #-}

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
lookupVar (sigma, []) id = fail $ "Variable " ++ printTree id ++ " not found"
lookupVar (sig, c : cs) id = case Map.lookup id c of
    Just t -> return t
    Nothing -> lookupVar (sig, cs) id

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig, t) id = case Map.lookup id sig of
    Just (ts, t) -> return (ts, t)
    Nothing -> fail $ "Function " ++ printTree id ++ " not found"

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, c : cs) id t = case Map.lookup id c of
    Just _ -> fail $ "Variable " ++ printTree id ++ " already declared"
    Nothing -> return (sig, Map.insert id t c : cs)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sigma, cs) id t = case lookupFun (sigma, cs) id of
    Ok _ -> fail $ "Function " ++ printTree id ++ " already declared"
    Bad _ -> return (Map.insert id t sigma, cs)

newBlock :: Env -> Env
newBlock (sig, cs) = (sig, Map.empty : cs)

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])
