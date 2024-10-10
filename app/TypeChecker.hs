-- Nicolas Andreoli 210630

{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import Control.Monad
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck p = do  
  env <- sign p
  checkProg env p

checkProg :: Env -> Program -> Err ()
checkProg env (PDefs []) = return ()
checkProg env (PDefs (def:defs)) = do
  checkDef env def
  checkProg env (PDefs defs)

checkDef :: Env -> Def -> Err ()
checkDef env (DFun t1 id args stms) = do
  env2 <- foldM (\env (ADecl t2 id) -> updateVar env id t2) (newBlock env) args
  checkStms t1 env2 stms
  return ()

inferExp :: Env -> Exp -> Err Type
inferExp env ETrue = Ok Type_bool
inferExp env EFalse = Ok Type_bool
inferExp env (EInt _) = Ok Type_int
inferExp env (EDouble _) = Ok Type_double
inferExp env (EString _) = Ok Type_string
inferExp env (EId id) = lookupVar env id
inferExp env (EIncr exp) = inferExp2 env exp
inferExp env (EDecr exp) = inferExp2 env exp
inferExp env (EPIncr exp) = inferExp2 env exp
inferExp env (EPDecr exp) = inferExp2 env exp
inferExp env (EAnd exp1 exp2) = inferOperatorExps env exp1 exp2
inferExp env (EOr exp1 exp2) = inferOperatorExps env exp1 exp2
inferExp env (ETimes exp1 exp2) = inferExps env exp1 exp2
inferExp env (EDiv exp1 exp2) = inferExps env exp1 exp2
inferExp env (EPlus exp1 exp2) = inferExps env exp1 exp2
inferExp env (EMinus exp1 exp2) = inferExps env exp1 exp2
inferExp env (ELt exp1 exp2) = inferBoolExps env exp1 exp2
inferExp env (EGt exp1 exp2) = inferBoolExps env exp1 exp2
inferExp env (ELtEq exp1 exp2) = inferBoolExps env exp1 exp2
inferExp env (EGtEq exp1 exp2) = inferBoolExps env exp1 exp2
inferExp env (EEq exp1 exp2) = inferEqualExps env exp1 exp2
inferExp env (ENEq exp1 exp2) = inferEqualExps env exp1 exp2
inferExp env (EApp id exps) = do
  (argTypes, returnType) <- lookupFun env id
  if length argTypes == length exps
  then do
    expTypes <- mapM (inferExp env) exps
    if expTypes == argTypes
    then return returnType
    else fail $ "Type error"
  else fail $ "Type error"
inferExp env (EAss exp1 exp2) = do
  t1 <- inferExp env exp1
  t2 <- inferExp env exp2
  if t1 == t2
  then return t1
  else fail $ "Type error"
inferExp env (ETyped exp1 t1) = do
  t2 <- inferExp env exp1
  if t2 == t1
  then return t1
  else fail $ "Type error"

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env exp t = do
  at <- inferExp env exp
  if at == t
  then return ()
  else fail $ "Type error"

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms t env [] = return env
checkStms t env (stm : stms) = do
  env2 <- checkStm t env stm
  checkStms t env2 stms

--- AUX FUNCTIONS ---

checkStm :: Type -> Env -> Stm -> Err Env
checkStm returnType env (SExp exp) = do
  inferExp env exp
  return env
checkStm returnType env (SReturn exp) = do
  checkExp env exp returnType
  return env
checkStm returnType env SReturnVoid = do
  if returnType == Type_void
  then return env
  else fail $ "Type error"
checkStm returnType env (SDecls t ids) = foldM (\env id -> updateVar env id t) env ids
checkStm t env (SBlock stms) = do
    checkStms t (newBlock env) stms
    return env
checkStm returnType env (SInit t id exp) = do
  checkExp env exp t
  updateVar env id t
checkStm returnType env (SIfElse exp stm1 stm2) = do
  checkExp env exp Type_bool
  checkStm returnType env stm1
  checkStm returnType env stm2
  return env
checkStm returnType env (SWhile exp stm) = do
  checkExp env exp Type_bool
  checkStm returnType env stm
  return env

sign :: Program -> Err Env
sign (PDefs []) = return emptyEnv
sign (PDefs ((DFun t id args stms) : xs)) = do
  env <- sign (PDefs xs)
  updateFun env id (map (\(ADecl t _) -> t) args, t)

inferExp2 :: Env -> Exp -> Err Type
inferExp2 env exp = do
  actualType <- inferExp env exp
  if actualType == Type_int || actualType == Type_double
  then return actualType
  else fail $ "Type error"

inferExps :: Env -> Exp -> Exp -> Err Type
inferExps env exp1 exp2 = do
  at1 <- inferExp env exp1
  at2 <- inferExp env exp2
  if (at1 == Type_int && at2 == Type_int) || (at1 == Type_double && at2 == Type_double)
  then return at1
  else fail $ "Type error"

inferBoolExps :: Env -> Exp -> Exp -> Err Type
inferBoolExps env exp1 exp2 = do
  at1 <- inferExp env exp1
  at2 <- inferExp env exp2
  if (at1 == Type_int && at2 == Type_int) || (at1 == Type_double && at2 == Type_double)
  then return Type_bool
  else fail $ "Type error"

inferOperatorExps :: Env -> Exp -> Exp -> Err Type
inferOperatorExps env exp1 exp2 = do
  at1 <- inferExp env exp1
  at2 <- inferExp env exp2
  if at1 == Type_bool && at2 == Type_bool
  then return Type_bool
  else fail $ "Type error"

inferEqualExps :: Env -> Exp -> Exp -> Err Type
inferEqualExps env exp1 exp2 = do
  at1 <- inferExp env exp1
  at2 <- inferExp env exp2
  if at1 == at2
  then return Type_bool
  else fail $ "Type error"
