{-# LANGUAGE CPP #-}
--Federico Czarnievicz - 201250
module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck p = do  
  env <- generateSignature p
  checkProg env p

checkProg :: Env -> Program -> Err ()
checkProg env (PDefs []) = return ()
checkProg env (PDefs (x : xs)) = do
  checkDef env x
  checkProg env (PDefs xs)

checkDef :: Env -> Def -> Err ()
checkDef env (DFun t id args stms) = do
    env2 <- updateVars (newBlock env) args
    checkStms t env2 stms
    return ()

inferExp :: Env -> Exp -> Err Type
inferExp env (ETrue) = return Type_bool
inferExp env (EFalse) = return Type_bool
inferExp env (EInt _) = return Type_int
inferExp env (EDouble _) = return Type_double
inferExp env (EString _) = return Type_string
inferExp env (EId id) = lookupVar env id
inferExp env (EApp id exps) = do
  (argTypes, retType) <- lookupFun env id
  expTypes <- mapM (inferExp env) exps
  if (expTypes == argTypes)
    then return retType
    else fail "Argument count mismatch in function app"
inferExp env (EPIncr exp) = inferExpIncr env exp
inferExp env (EPDecr exp) = inferExpIncr env exp
inferExp env (EIncr exp) = inferExpIncr env exp
inferExp env (EDecr exp) = inferExpIncr env exp
inferExp env (ETimes exp1 exp2) = inferExpAritmetic env exp1 exp2
inferExp env (EDiv exp1 exp2) = inferExpAritmetic env exp1 exp2
inferExp env (EPlus exp1 exp2) = do
    t1 <- inferExp env exp1
    checkExp env exp2 t1
    if elem t1 [Type_double, Type_int, Type_string]
      then do return t1
      else fail "Type must be double, int or string"
inferExp env (EMinus exp1 exp2) = inferExpAritmetic env exp1 exp2
inferExp env (ELt exp1 exp2) = inferExpEq env exp1 exp2
inferExp env (EGt exp1 exp2) = inferExpEq env exp1 exp2
inferExp env (ELtEq exp1 exp2) = inferExpEq env exp1 exp2
inferExp env (EGtEq exp1 exp2) = inferExpEq env exp1 exp2
inferExp env (EEq exp1 exp2) = inferExpComp env exp1 exp2 [Type_double, Type_int, Type_bool]
inferExp env (ENEq exp1 exp2) = inferExpComp env exp1 exp2 [Type_double, Type_int, Type_bool]
inferExp env (EAnd exp1 exp2) = inferExpComp env exp1 exp2 [Type_bool]
inferExp env (EOr exp1 exp2) = inferExpComp env exp1 exp2 [Type_bool]
inferExp env (EAss exp1 exp2) = do
  t1 <- inferExp env exp1
  checkExp env exp2 t1
  return t1

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env exp t = do
  t1 <- inferExp env exp
  if t == t1
    then return ()
    else fail $ "Expected type " ++ show t ++ " but got " ++ show t1

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms t env [] = return env
checkStms t env (stm : stms) = do
  env2 <- checkStm t env stm
  checkStms t env2 stms


-- aux
generateSignature :: Program -> Err Env
generateSignature (PDefs []) = return emptyEnv
generateSignature (PDefs ((DFun t id args stms) : xs)) = do
  env <- generateSignature (PDefs xs)
  updateFun env id (map (\(ADecl t _) -> t) args, t)

-- aux
updateVars :: Env -> [Arg] -> Err Env
updateVars env [] = return env
updateVars env ((ADecl t id) : xs) = do
  env2 <- updateVar env id t
  updateVars env2 xs

--aux 
inferExpComp :: Env -> Exp -> Exp -> [Type] -> Err Type
inferExpComp env exp1 exp2 validTypes = do
  t1 <- inferExp env exp1
  checkExp env exp2 t1
  if elem t1 validTypes
    then return Type_bool
    else fail "Mismatch types, invalid comparison"

-- aux
inferExpEq :: Env -> Exp -> Exp -> Err Type
inferExpEq env exp1 exp2 = do
    t1 <- inferExp env exp1
    checkExp env exp2 t1
    if elem t1 [Type_double, Type_int, Type_string, Type_bool]
      then do return Type_bool
      else fail "Type must be double, int, string or bool"

--aux
inferExpIncr :: Env -> Exp -> Err Type
inferExpIncr env exp = do
    t1 <- inferExp env exp
    if elem t1 [Type_double, Type_int]
      then case exp of
        EId id -> return t1
        _ -> fail "Invalid id"
      else fail "Type must be double or int"

-- aux
inferExpAritmetic :: Env -> Exp -> Exp -> Err Type
inferExpAritmetic env exp1 exp2 = do
    t1 <- inferExp env exp1
    checkExp env exp2 t1
    if elem t1 [Type_double, Type_int]
      then do return t1
      else fail "Type must be double or int"


-- aux
checkStm :: Type -> Env -> Stm -> Err Env
checkStm t env (SExp exp) = do
    inferExp env exp
    return env
checkStm t env (SDecls t1 []) = return env
checkStm t env (SDecls t1 (id : ids)) = do
  env2 <- (updateVar env id t1)
  checkStm t env2 (SDecls t1 ids)
checkStm t env (SInit t1 id exp) = do
  checkExp env exp t1
  updateVar env id t1
checkStm t env (SReturn exp) = do
    t1 <- inferExp env exp
    if t == t1
      then return env
      else fail $ "Expected type" ++ show t1 ++ " but got " ++ show t
checkStm t env (SReturnVoid) = case t of
  Type_void -> return env
  _ -> fail "Must be type void"
checkStm t env (SBlock stms) = do
    checkStms t (newBlock env) stms
    return env
checkStm t env (SIfElse exp st1 st2) = do
    checkExp env exp Type_bool
    checkStm t env st1
    checkStm t env st2
    return env
checkStm t env (SWhile exp stm) = do
    checkExp env exp Type_bool
    checkStm t env stm
    return env
