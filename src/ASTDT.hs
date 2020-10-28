{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module ASTDT where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Terminals

import ASTSpec


data Set
  = Set {set_sort :: Sort, set_xs :: NVars, set_ref :: Cond}
  deriving (Show, Eq, Read)
data Exp
  = App {app_fun :: NFun, app_arg :: Exp} |
    Op_Inf {op_inf_l :: Exp, op_inf_op :: BOp, op_inf_r :: Exp} |
    Var {var_t :: NVar} |
    Lit {lit_t :: Val}
  deriving (Show, Eq, Read)
data Sig
  = Sig {sig_dom :: Set, sig_cod :: Set}
  deriving (Show, Eq, Read)
data ExpG
  = Or {or_exp :: Exp} |
    If {if_exp :: Exp, if_cond :: Cond, if_else :: ExpG}
  deriving (Show, Eq, Read)
data Cond
  = Neg {neg_e :: Cond} |
    And {and_l :: Cond, and_r :: Cond} |
    Equa {equa_l :: Exp, equa_op :: EOp, equa_r :: Exp} |
    Top {}
  deriving (Show, Eq, Read)
data Ecu
  = Ecu {ecu_l :: NVars, ecu_r :: ExpG}
  deriving (Show, Eq, Read)
data FDef
  = FDef {fDef_name :: NFun, fDef_sig :: Sig, fDef_body :: Ecu}
  deriving (Show, Eq, Read)




sem_Set asp (Set set_sort set_xs set_ref)
  = ((knitAspect p_Set) asp)
      (((.*.) (((.=.) ch_set_sort) (sem_Lit set_sort)))
         (((.*.) (((.=.) ch_set_xs) (sem_Lit set_xs)))
            (((.*.) (((.=.) ch_set_ref) ((sem_Cond asp) set_ref)))
               emptyGenRec)))
sem_Exp asp (App app_fun app_arg)
  = ((knitAspect p_App) asp)
      (((.*.) (((.=.) ch_app_fun) (sem_Lit app_fun)))
         (((.*.) (((.=.) ch_app_arg) ((sem_Exp asp) app_arg))) emptyGenRec))
sem_Exp asp (Op_Inf op_inf_l op_inf_op op_inf_r)
  = ((knitAspect p_Op_Inf) asp)
      (((.*.) (((.=.) ch_op_inf_l) ((sem_Exp asp) op_inf_l)))
         (((.*.) (((.=.) ch_op_inf_op) (sem_Lit op_inf_op)))
            (((.*.) (((.=.) ch_op_inf_r) ((sem_Exp asp) op_inf_r)))
               emptyGenRec)))
sem_Exp asp (Var var_t)
  = ((knitAspect p_Var) asp)
      (((.*.) (((.=.) ch_var_t) (sem_Lit var_t))) emptyGenRec)
sem_Exp asp (Lit lit_t)
  = ((knitAspect p_Lit) asp)
      (((.*.) (((.=.) ch_lit_t) (sem_Lit lit_t))) emptyGenRec)
sem_Sig asp (Sig sig_dom sig_cod)
  = ((knitAspect p_Sig) asp)
      (((.*.) (((.=.) ch_sig_dom) ((sem_Set asp) sig_dom)))
         (((.*.) (((.=.) ch_sig_cod) ((sem_Set asp) sig_cod))) emptyGenRec))
sem_ExpG asp (Or or_exp)
  = ((knitAspect p_Or) asp)
      (((.*.) (((.=.) ch_or_exp) ((sem_Exp asp) or_exp))) emptyGenRec)
sem_ExpG asp (If if_exp if_cond if_else)
  = ((knitAspect p_If) asp)
      (((.*.) (((.=.) ch_if_exp) ((sem_Exp asp) if_exp)))
         (((.*.) (((.=.) ch_if_cond) ((sem_Cond asp) if_cond)))
            (((.*.) (((.=.) ch_if_else) ((sem_ExpG asp) if_else)))
               emptyGenRec)))
sem_Cond asp (Neg neg_e)
  = ((knitAspect p_Neg) asp)
      (((.*.) (((.=.) ch_neg_e) ((sem_Cond asp) neg_e))) emptyGenRec)
sem_Cond asp (And and_l and_r)
  = ((knitAspect p_And) asp)
      (((.*.) (((.=.) ch_and_l) ((sem_Cond asp) and_l)))
         (((.*.) (((.=.) ch_and_r) ((sem_Cond asp) and_r))) emptyGenRec))
sem_Cond asp (Equa equa_l equa_op equa_r)
  = ((knitAspect p_Equa) asp)
      (((.*.) (((.=.) ch_equa_l) ((sem_Exp asp) equa_l)))
         (((.*.) (((.=.) ch_equa_op) (sem_Lit equa_op)))
            (((.*.) (((.=.) ch_equa_r) ((sem_Exp asp) equa_r))) emptyGenRec)))
sem_Cond asp Top = ((knitAspect p_Top) asp) emptyGenRec
sem_Ecu asp (Ecu ecu_l ecu_r)
  = ((knitAspect p_Ecu) asp)
      (((.*.) (((.=.) ch_ecu_l) (sem_Lit ecu_l)))
         (((.*.) (((.=.) ch_ecu_r) ((sem_ExpG asp) ecu_r))) emptyGenRec))
sem_FDef asp (FDef fDef_name fDef_sig fDef_body)
  = ((knitAspect p_FDef) asp)
      (((.*.) (((.=.) ch_fDef_name) (sem_Lit fDef_name)))
         (((.*.) (((.=.) ch_fDef_sig) ((sem_Sig asp) fDef_sig)))
            (((.*.) (((.=.) ch_fDef_body) ((sem_Ecu asp) fDef_body)))
               emptyGenRec)))
