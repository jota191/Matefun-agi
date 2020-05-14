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

module Language.MFAG.Semantics.PrettyPrintCore where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals

import Language.MFAG.Syntax.Exp.Core
import Language.MFAG.Syntax.Exp.Base

import Data.List

-- | pretty print
$(attLabels [("spp", ''String)])


asp_pp_Exp =
      (syn spp p_Lit $ printVal <$> ter ch_lit_t)
  .+: (syn spp p_Var $ ter ch_var_t)
  .+: (syn spp p_OpInf
       $ do lpp <- at ch_op_inf_l spp
            rpp <- at ch_op_inf_r spp
            op  <- ter ch_op_inf_op
            return $ "(" ++ lpp ++ " " ++ op ++ " " ++ rpp ++ ")")
  .+: (syn spp p_OpPre
       $ do epp <- at ch_op_pre_e spp
            op  <- ter ch_op_pre_op
            return $ "(" ++ op ++ " " ++ epp ++ ")")
  .+: (syn spp p_App
       $ do name <- ter ch_app_f
            arg  <- at ch_app_e spp
            return (name ++ " " ++ arg))
  .+: (syn spp p_AppU
       $ return "")
  .+: (syn spp p_EProd
       $ do e <- at ch_eprod_e spp
            return $ "( " ++ e ++ " )")
  .+: (syn spp p_TCons
       $ do h <- at ch_tuple_h spp
            t <- at ch_tuple_t spp
            return $ h ++ ", " ++ t)
  .+: use spp p_TSing (nt_Exp .:. eL) (++) ""
  .+: (syn spp p_ExpGIf
       $ do e <- at ch_expGIf_e spp
            c <- at ch_expGIf_cond spp
            t <- at ch_expGIf_tail spp
            return $ e ++ " if " ++ c ++ "\n\t" ++ t
      )
  .+: (syn spp p_ExpGOr
       $ do e <- at ch_expGOr_e spp
            return $ "or " ++ e
      )
  .+: (syn spp p_Ecu
       $ do vars <- ter ch_ecu_l
            body <- at ch_ecu_r spp
            return $ "(" ++  intercalate "," vars ++  ")" ++ body
      )
  .+: (syn spp p_Top (pure "T") )
  .+: (syn spp p_Equa
       $ pure ""
      )
  .+: (syn spp p_And (pure "") )
  .+: (syn spp p_Neg (pure "") )
  .+: emptyAspect
  
printExp e = sem_Exp asp_pp_Exp e emptyAtt #. spp


asp_pp_FDef =
  (syn spp p_FDef $
   do nfun <- ter ch_nfun
      body <- at ch_fun_body spp
      return $ nfun ++ "" ++ body)
  .+:
  (syn spp p_Sig (return ""))
  .+: asp_pp_Exp
 

-- printFunDec f = sem_FDef asp_pp_FDef f emptyAtt #. spp
