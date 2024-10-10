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
typecheck (PDefs defs) = case generateSignature defs of
  Ok env -> checkProg env (PDefs defs)
  Bad errorMessage -> fail errorMessage

checkProg :: Env -> Program -> Err ()
checkProg env (PDefs defs) = do
  env' <- generateSignature defs
  env'' <- foldM (\env fun -> checkDef env' fun) () defs
  return ()

checkDef :: Env -> Def -> Err ()
checkDef env (DFun type1 id args stms) = do
  env' <- foldM (\env (ADecl type2 id) -> updateVar env id type2) (newBlock env) args
  checkStms type1 env' stms
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
    else fail $ "Type error: Expected " ++ printTree argTypes ++ ". Actual: " ++ printTree expTypes
    else fail $ "Type error: Expected " ++ show (length argTypes) ++ " arguments. Actual: " ++ show (length exps)
inferExp env (EPIncr exp) = inferOperationWithOneExp env exp
inferExp env (EPDecr exp) = inferOperationWithOneExp env exp
inferExp env (EIncr exp) = inferOperationWithOneExp env exp
inferExp env (EDecr exp) = inferOperationWithOneExp env exp
inferExp env (ETimes exp1 exp2) = inferOperationWithTwoExp env exp1 exp2
inferExp env (EDiv exp1 exp2) = inferOperationWithTwoExp env exp1 exp2
inferExp env (EPlus exp1 exp2) = inferOperationWithTwoExp env exp1 exp2
inferExp env (EMinus exp1 exp2) = inferOperationWithTwoExp env exp1 exp2
inferExp env (ELt exp1 exp2) = inferOperationBoolWithTwoExp env exp1 exp2
inferExp env (EGt exp1 exp2) = inferOperationBoolWithTwoExp env exp1 exp2
inferExp env (ELtEq exp1 exp2) = inferOperationBoolWithTwoExp env exp1 exp2
inferExp env (EGtEq exp1 exp2) = inferOperationBoolWithTwoExp env exp1 exp2
inferExp env (EEq exp1 exp2) = inferEqualityOperation env exp1 exp2
inferExp env (ENEq exp1 exp2) = inferEqualityOperation env exp1 exp2
inferExp env (EAnd exp1 exp2) = inferOperationOperatorWithTwoExp env exp1 exp2
inferExp env (EOr exp1 exp2) = inferOperationOperatorWithTwoExp env exp1 exp2
inferExp env (EAss exp1 exp2) = do
  type1 <- inferExp env exp1
  type2 <- inferExp env exp2
  if type1 == type2
  then return type1
  else fail $ "Type error: Expected " ++ printTree type1 ++ ". Actual: " ++ printTree type2
inferExp env (ETyped exp1 type1) = do
  type2 <- inferExp env exp1
  if type2 == type1
  then return type1
  else fail $ "Type error: Expected " ++ printTree type1 ++ ". Actual: " ++ printTree type2

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env exp expType = do
  actualType <- inferExp env exp
  if actualType == expType
  then return ()
  else fail $ "Type error: Expected " ++ printTree expType ++ ". Actual: " ++ printTree actualType

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms returnType env [] = return env
checkStms returnType env (stm : stms) = do
  updatedEnv <- checkStm returnType env stm
  checkStms returnType updatedEnv stms

-- Funciones auxiliares
generateSignature :: [Def] -> Err Env
generateSignature = foldM (\env (DFun t id args _) -> updateFun env id (Prelude.map (\(ADecl t id) -> t) args, t)) emptyEnv

inferOperationWithOneExp :: Env -> Exp -> Err Type
inferOperationWithOneExp env exp = do
  actualType <- inferExp env exp
  if actualType == Type_int || actualType == Type_double
  then return actualType
  else fail $ "Type error: Expected " ++ printTree Type_int ++ "or " ++ printTree Type_double ++ ". Actual: " ++ printTree actualType

inferOperationWithTwoExp :: Env -> Exp -> Exp -> Err Type
inferOperationWithTwoExp env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if (actualType1 == Type_int && actualType2 == Type_int) || (actualType1 == Type_double && actualType2 == Type_double)
  then return actualType1
  else fail $ "Type error: Expected " ++ printTree Type_int ++ "or " ++ printTree Type_double ++ ". Actual: " ++ printTree actualType1

inferOperationBoolWithTwoExp :: Env -> Exp -> Exp -> Err Type
inferOperationBoolWithTwoExp env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if (actualType1 == Type_int && actualType2 == Type_int) || (actualType1 == Type_double && actualType2 == Type_double)
  then return Type_bool
  else fail $ "Type error: Expected " ++ printTree Type_int ++ "or " ++ printTree Type_double ++ ". Actual: " ++ printTree actualType1

inferOperationOperatorWithTwoExp :: Env -> Exp -> Exp -> Err Type
inferOperationOperatorWithTwoExp env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if actualType1 == Type_bool && actualType2 == Type_bool
  then return Type_bool
  else fail $ "Type error: Expected both" ++ printTree Type_bool ++ ". Actual: " ++ printTree actualType1 ++ " and " ++ printTree actualType2

inferEqualityOperation :: Env -> Exp -> Exp -> Err Type
inferEqualityOperation env exp1 exp2 = do
  actualType1 <- inferExp env exp1
  actualType2 <- inferExp env exp2
  if actualType1 == actualType2
  then return Type_bool
  else fail $ "Type error: Expected " ++ printTree actualType1 ++ ". Actual: " ++ printTree actualType2

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
  else fail $ "Type error: Expected " ++ printTree Type_void ++ ". Actual: " ++ printTree returnType
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