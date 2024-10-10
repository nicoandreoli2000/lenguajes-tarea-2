{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import Control.Monad hiding (fail)
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck (PDefs defs) = case sign defs of
  Ok env -> checkProg env (PDefs defs)
  Bad err -> fail err

checkProg :: Env -> Program -> Err ()
checkProg env (PDefs defs) = do
  env' <- sign defs
  env'' <- foldM (\env fun -> checkDef env' fun) () defs
  return ()

checkDef :: Env -> Def -> Err ()
checkDef env (DFun t1 id args stms) = do
  env' <- foldM (\env (ADecl t2 id) -> updateVar env id t2) (newBlock env) args
  checkStms t1 env' stms
  return ()

inferExp :: Env -> Exp -> Err Type
inferExp env ETrue = Ok Type_bool
inferExp env EFalse = Ok Type_bool
inferExp env (EInt _) = Ok Type_int
inferExp env (EDouble _) = Ok Type_double
inferExp env (EString _) = Ok Type_string
inferExp env (EId id) = lookupVar env id
inferExp env (EApp id exps) = do
  (argTypes, returnType) <- lookupFun env id
  if length argTypes == length exps
  then do
    expTypes <- mapM (inferExp env) exps
    if expTypes == argTypes
    then return returnType
    else fail $ "Type error"
  else fail $ "Type error"
inferExp env (EPIncr exp) = inferExp2 env exp
inferExp env (EPDecr exp) = inferExp2 env exp
inferExp env (EIncr exp) = inferExp2 env exp
inferExp env (EDecr exp) = inferExp2 env exp
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
inferExp env (EAnd exp1 exp2) = inferOperatorExps env exp1 exp2
inferExp env (EOr exp1 exp2) = inferOperatorExps env exp1 exp2
inferExp env (EAss exp1 exp2) = do
  type1 <- inferExp env exp1
  type2 <- inferExp env exp2
  if type1 == type2
  then return type1
  else fail $ "Type error"
inferExp env (ETyped exp1 type1) = do
  type2 <- inferExp env exp1
  if type2 == type1
  then return type1
  else fail $ "Type error"

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env exp t = do
  at <- inferExp env exp
  if at == t
  then return ()
  else fail $ "Type error"

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms returnType env [] = return env
checkStms returnType env (stm : stms) = do
  updatedEnv <- checkStm returnType env stm
  checkStms returnType updatedEnv stms

-- Funciones auxiliares
checkStm :: Type -> Env -> Stm -> Err Env
checkStm returnType env (SExp exp) = do
  inferExp env exp
  return env
checkStm returnType env (SDecls type1 ids) = foldM (\env id -> updateVar env id type1) env ids
checkStm returnType env (SInit type1 id exp) = do
  checkExp env exp type1
  updateVar env id type1
checkStm returnType env (SReturn exp) = do
  checkExp env exp returnType
  return env
checkStm returnType env SReturnVoid = do
  if returnType == Type_void
  then return env
  else fail $ "Type error"
checkStm returnType env (SWhile exp stm) = do
  checkExp env exp Type_bool
  checkStm returnType env stm
  return env
checkStm returnType env (SBlock stms) = do
  foldM (\env stm -> checkStm returnType env stm) (newBlock env) stms
  return env
checkStm returnType env (SIfElse exp stm1 stm2) = do
  checkExp env exp Type_bool
  checkStm returnType env stm1
  checkStm returnType env stm2
  return env

sign :: [Def] -> Err Env
sign = foldM (\env (DFun t id args _) -> updateFun env id (Prelude.map (\(ADecl t id) -> t) args, t)) emptyEnv

inferExp2 :: Env -> Exp -> Err Type
inferExp2 env exp = do
  actualType <- inferExp env exp
  if actualType == Type_int || actualType == Type_double
  then return actualType
  else fail $ "Type error"

inferExps :: Env -> Exp -> Exp -> Err Type
inferExps env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if (actualType1 == Type_int && actualType2 == Type_int) || (actualType1 == Type_double && actualType2 == Type_double)
  then return actualType1
  else fail $ "Type error"

inferBoolExps :: Env -> Exp -> Exp -> Err Type
inferBoolExps env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if (actualType1 == Type_int && actualType2 == Type_int) || (actualType1 == Type_double && actualType2 == Type_double)
  then return Type_bool
  else fail $ "Type error"

inferOperatorExps :: Env -> Exp -> Exp -> Err Type
inferOperatorExps env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if actualType1 == Type_bool && actualType2 == Type_bool
  then return Type_bool
  else fail $ "Type error"

inferEqualExps :: Env -> Exp -> Exp -> Err Type
inferEqualExps env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if actualType1 == actualType2
  then return Type_bool
  else fail $ "Type error"
