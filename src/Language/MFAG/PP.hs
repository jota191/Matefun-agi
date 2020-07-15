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

import Control.Applicative
import Data.List (intercalate)
import Prelude hiding (exp)


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
  append5 <$> (pure "\n\t " <$> at ch_expGIf_e spp)
          <*> pure " if "
          <*> at ch_expGIf_cond spp
          <*> pure " or"
          <*> at ch_expGIf_tail spp
  )
  .+:
  syn spp p_ExpGOr (at ch_expGOr_e spp)
  .+:
  emptyAspect

asp_spp_Cond =
  syn spp p_Top (return "T")
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
     return $ wrapParen (intercalate ", "vars) ++ " =\n    " ++ body

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

r = Set R [] Top

c 1 = Var "x"
c 2 = Lit (ValZ 23)
c 3 = OpInf (c 1) Plus (c 2)
c 4 = OpInf (c 2) Plus (c 2)
--c 5 = OpPre "-" (OpInf (c 4) Plus (c 2)) 
c 6 = App "nomF" (OpInf (c 4) Plus (c 2))
--c 7 = AppU (Ecu ["x"] (ExpGOr (c 6))) (OpInf (c 4) "+" (c 2))  
c 8 = Index (EProd (TCons (c 4) (TSing (c 3)))) 1

-- test_id_Core
--   = [c i == testIdC (c i) | i <- [1..8]]
f = FDef "f" (Sig r r) (Ecu ["x"] $ ExpGOr $ c 8)
