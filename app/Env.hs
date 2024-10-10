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
lookupVar (sig, _) id = case Map.lookup id sig of
    Just t -> return t
    Nothing -> fail $ "Variable " ++ printTree id ++ " not found"

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig, _) id = case Map.lookup id sig of
    Just (ts, t) -> return (ts, t)
    Nothing -> fail $ "Function " ++ printTree id ++ " not found"

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, (c:cs)) id t = return (sig, (insert id t c) : cs)
updateVar _ _ _ = fail "No context available to update variable"

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, cs) id funType = return (insert id funType sig, cs)

newBlock :: Env -> Env
newBlock (sig, ctx) = (sig, empty : ctx)

emptyEnv :: Env
emptyEnv = (empty, [empty])
