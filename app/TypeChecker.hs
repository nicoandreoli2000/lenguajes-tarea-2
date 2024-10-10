{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

import Data.Map as Map

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck p = do
  sigma <- recolectarFunciones p
  checkProg (sigma, [empty]) p

recolectarFunciones :: Program -> Err Sig
recolectarFuncions (PDef defs) =
  foldM (\sig (DFun rho f args _) -> updateFun sig f (extraerTipos args, rho)) empty defs
  where
    extraerTipos :: [Arg] -> [Type]
    extraerTipos = undefined

checkProg :: Env -> Program -> Err ()
checkProg = undefined 

checkDef :: Env -> Def -> Err ()
checkDef gamma (DFun tipoRetorno nombreFuncion arg stms)= do
  -- falta extender gamma con args
  checkStms tipoRetorno gamma stms
  return ()
checkDef _ _ = fail "checkDef no implementado"

inferExp :: Env -> Exp -> Err Type
inferExp gamma (EInt n) = return Type_int
inferExp _ _ = fail "inferExp no implementada"

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms tipoRetorno gamma (SReturn exp : stms) = do
  checkExp gamma exp tipoRetorno
  return gamma
checkStms _ _ _ = fail "checkStms no implementado"
