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


module Language.MFAG.PP where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import Language.MFAG.SemFuncs
import Language.MFAG.Utils
import Language.MFAG.Utils.Attributes

import Control.Applicative
import Data.List (intercalate)
import Prelude hiding (exp)

-- esto seria para usar despues
{- bopPrecedence Exp = 4

bopPrecedence Times = 3
bopPrecedence Div   = bopPrecedence Times

bopPrecedence Plus  = 2
bopPrecedence Minus = bopPrecedence Plus

bopPrecedence Cons  = 1
-}

asp_precedence_Exp
   =  inh precedence p_OpInf ch_op_inf_l (return 1)
  .+: inh precedence p_OpInf ch_op_inf_r (return 1)

  .+: inh precedence p_Equa ch_equa_l (return 6)
  .+: inh precedence p_Equa ch_equa_r (return 6)

  .+: inh precedence p_App ch_app_e (return 0)

  .+: inh precedence p_TCons ch_tuple_h (return 0)
  .+: inh precedence p_TSing ch_tuple_s (return 0)

  .+: inh precedence p_Index ch_index_e (return 5)

  .+: inh precedence p_ExpGIf ch_expGIf_e (return 0)
  .+: inh precedence p_ExpGOr ch_expGOr_e (return 0)

  .+: emptyAspect

asp_spp = AspAll
  asp_spp_Set
  asp_spp_Sig
  asp_spp_Exp
  asp_spp_ExpG
  asp_spp_Cond
  asp_spp_Ecu
  asp_spp_FDef
  asp_spp_Tuple


asp_spp_Set = singAsp
  $ syn spp p_Set
  (
    do ppsort <- show <$> ter ch_sort
       ppvars <- show <$> ter ch_xs
       ppcond <- at ch_refinement spp
       return ("{" ++ ppvars ++ " in " ++ ppsort ++ " | "
         ++ ppcond ++ "}")
  )

asp_spp_Sig = singAsp
  $ syn spp p_Sig $ liftA3 append3
                    (at ch_dom spp)
                    (pure " -> ")
                    (at ch_cod spp)

asp_spp_Exp =
  syn spp p_Var (ter ch_var_t)
  .+:
  syn spp p_Lit (printVal <$> ter ch_lit_t)
  .+:
  syn spp p_OpInf (
    wrapParen <$> liftA3 append3
                  (at ch_op_inf_l spp)
                  (wrapSpace . show <$> ter ch_op_inf_op)
                  (at ch_op_inf_r spp)
    )
  .+:
  syn spp p_App (
    (++) <$> ter ch_app_f <*> (wrapParen <$> at ch_app_e spp)
  )
  .+:
  syn spp p_EProd (wrapParen <$> at ch_eprod_e spp)
  .+:
  syn spp p_Index (
  liftA3 append3 (at ch_index_e spp)
                 (pure "!")
                 (show <$> ter ch_index_i)
  )
  .+:
  emptyAspect

asp_spp_ExpG =
  syn spp p_ExpGIf (
  append5 <$> at ch_expGIf_e spp
          <*> pure " if "
          <*> at ch_expGIf_cond spp
          <*> pure "\n\tor "
          <*> at ch_expGIf_tail spp
  )
  .+:
  syn spp p_ExpGOr (at ch_expGOr_e spp)
  .+:
  emptyAspect

asp_spp_Cond =
  syn spp p_Top (return "True")
  .+:
  syn spp p_Equa (
   wrapParen <$> (append5
                  <$> at ch_equa_l spp
                  <*> pure " "
                  <*> (show <$> ter ch_equa_op)
                  <*> pure " "
                  <*> at ch_equa_r spp)
  )
  .+:
  (syn spp p_And
    $ wrapParen <$>
     (append3 <$> at ch_and_l spp
              <*> pure " and "
              <*> at ch_and_r spp
     )
  )
  .+:
  (syn spp p_Neg
    $ ("Not " ++) <$> at ch_neg_e spp
  )
  .+:
  emptyAspect

asp_spp_Ecu =
  singAsp $ syn spp p_Ecu $
  do vars <- ter ch_ecu_l
     body <- at ch_ecu_r spp
     return $ wrapParen (intercalate ", "vars) ++ " =\n\t" ++ body

asp_spp_FDef =
  singAsp $ syn spp p_FDef $
  do name <- ter ch_nfun
     sig  <- at ch_fun_sig spp
     body <- at ch_fun_body spp
     return $ name ++ " : " ++ sig ++ "\n"
           ++ name ++ " " ++ body
asp_spp_Tuple =
  syn spp p_TCons (
  append3 <$> (at ch_tuple_h spp)
          <*> (pure ", ")
          <*> (at ch_tuple_t spp)
  )
  .+:
  syn spp p_TSing (at ch_tuple_s spp)
  .+:
  emptyAspect


pp e = sem_FDef asp_spp e emptyAtt #. spp

-- Tests

secuence s from to = (take to $ map s $ iterate (+1) from)

c 1 = Var "x"
c 2 = Lit (ValZ 23)
c 3 = OpInf (c 1) Plus (c 2)
c 4 = OpInf (c 2) Plus (c 2)
--c 5 = OpPre "-" (OpInf (c 4) Plus (c 2))
c 6 = App "nomF" (OpInf (c 4) Plus (c 2))
--c 7 = AppU (Ecu ["x"] (ExpGOr (c 6))) (OpInf (c 4) "+" (c 2))
c 8 = Index (EProd (TCons (c 4) (TSing (c 3)))) 1

cnd 1 = Top
cnd 2 = Equa (Lit (ValZ 0)) GEq (Var "x")
cnd 3 = Equa (Lit (ValZ 0)) GEq (Var "y")
cnd 4 = And (cnd 2) (cnd 3)
cnd 5 = Neg (cnd 2)
cnd i = Equa (Var "x") Eq (Lit (ValZ i))

e 1 = Var "x"
e 2 = Lit (ValZ 23)
e 3 = Lit (ValR 9.85)
e 4 = Lit (ValC (C "C"))
e 5 = Lit (ValSec  [ValZ 23, ValR 9.85])
e 6 = Lit (ValTupl [ValZ 23, ValR 9.85])
e 7 = Lit (ValTupl [ValZ 23, ValR 9.85])

e 8  = OpInf (e 1) Plus  (e 2)
e 9  = OpInf (e 1) Minus (e 2)
e 10 = OpInf (e 1) Times (e 2)
e 11 = OpInf (e 1) Exp   (e 2)
e 12 = OpInf (e 1) Div   (e 2)
e 13 = OpInf (e 1) Cons  (e 2)

e 14 = EProd $ TSing (c 3)
e 15 = EProd $ TCons (c 4) (TSing (c 3))
e 16 = Index (EProd (TCons (c 4) (TSing (c 3)))) 1
e 17 = App "nomF" (e 11)

so 1 = Set Z                     [] Top
so 2 = Set R                     [] Top
so 3 = Set Enum                  [] Top
so 4 = Set (Tuple [R, Z])        [] Top
so 5 = Set (List (Tuple [R, Z])) [] Top
so _ = Set R                     [] Top

s i = Sig (so $ i*2) (so $ i*2+1)

-- test_id_Core
--   = [c i == testIdC (c i) | i <- [1..8]]

expG [] orE             = ExpGOr orE
expG ((e, cond):ts) orE = ExpGIf e cond (expG ts orE)

f s vs ls orE  = FDef "f" s (Ecu vs $ expG ls orE)

test1 = putStrLn $ pp $ f (s 1) []         []                                        (c 8)

test2 = putStrLn $ pp $ f (s 2) ["x"]      ((c 1, cnd 1):[])                         (c 2)

test3 = putStrLn $ pp $ f (s 3) ["x","y"] (zip (secuence e 1 6) (secuence cnd 1 6))  (e 7)

test4 = putStrLn $ pp $ f (s 4) ["z"]     (zip (secuence e 8 12) (secuence cnd 1 5)) (e 13)

test5 = putStrLn $ pp $ f (s 5) ["w"]     (zip (secuence e 14 16) (secuence cnd 1 2)) (e 17)
