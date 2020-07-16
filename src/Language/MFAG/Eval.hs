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


module Language.MFAG.Eval where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import Language.MFAG.SemFuncs
import Language.MFAG.Utils
import Language.MFAG.Attributes
import Language.MFAG.Gamma
import Language.MFAG.Model
import Language.MFAG.SId
import Language.MFAG.Delta as D

import Control.Applicative
import Control.Monad

import Data.List (intercalate)
import Prelude hiding (exp)

import qualified Data.Map as M


asp_seval = AspAll
  asp_seval_Set
  asp_seval_Sig
  asp_seval_Exp
  asp_seval_ExpG
  asp_seval_Cond
  asp_seval_Ecu
  asp_seval_FDef
  asp_seval_Tuple

asp_seval_Set =
  emptyRuleAtPrd p_Set .+: emptyAspect
asp_seval_Sig =
  emptyRuleAtPrd p_Sig .+: emptyAspect
asp_seval_Cond =
  emptyRuleAtPrd p_Top
  .+: emptyRuleAtPrd p_Equa
  .+: emptyRuleAtPrd p_And
  .+: emptyRuleAtPrd p_Neg
  .+: emptyAspect
asp_seval_Ecu = emptyRuleAtPrd p_Ecu .+: emptyAspect
asp_seval_FDef = emptyRuleAtPrd p_FDef .+: emptyAspect

asp_seval_Exp =
  syn seval p_Var (
  do x <- ter ch_var_t
     gamma <- at lhs igamma
     let mv = M.lookup x gamma
     case mv of
       Nothing -> error ("variable not in scope\n"
                         ++ show x ++ "\n" ++ show gamma) 
       Just v  -> return v
  )
  .+:
  syn seval p_Lit (ter ch_lit_t)
  .+:
  syn seval p_OpInf (
  do l  <- at ch_op_inf_l seval
     r  <- at ch_op_inf_r seval
     op <- ter ch_op_inf_op
     return $ interpOpExpr op l r
  )
  .+:
  syn seval p_EProd (
  do res <- at ch_eprod_e seval
     case res of
       ValTupl _ -> return res
       _ -> error "unhandled type error: tuple"
  )
  .+:
  syn seval p_App (
  do f <- ter ch_app_f
     delta <- at lhs idelta
     gamma <- at lhs igamma
     evArg <- at ch_app_e seval
     let (FDef _ _ (Ecu vars expG)) = lookupFun f delta
     let newCtxElems = M.fromList (zip vars (untup evArg))
     if (length vars /= arity evArg)
     then error "arg arity mismatch"
     else return $ evalExpG (newCtxElems `M.union` gamma) delta expG
  )
  .+:
  syn seval p_Index (
  do e <- at ch_index_e seval
     i <- ter ch_index_i
     return $ e `ith` i
  )
  .+:
  emptyAspect

asp_seval_Tuple =
  syn seval p_TSing (ValTupl . wrap <$> at ch_tuple_s seval)
  .+:
  syn seval p_TCons (
  do h <- at ch_tuple_h seval
     t <- at ch_tuple_t seval
     case t of
       ValTupl t -> return (ValTupl (h : t))
       _ -> error "impossible" 
  )
  .+:
  emptyAspect

asp_seval_ExpG =
  syn seval p_ExpGIf (return (ValZ 0))
  .+:
  syn seval p_ExpGOr (at ch_expGOr_e seval)
  .+:
  emptyAspect



asp = asp_seval .:+:. asp_igamma .:+:. asp_idelta


evalExp :: GammaT -> DeltaT -> Exp -> Val
evalExp gamma delta e =
 sem_Exp asp e (igamma =. gamma *.
                idelta =. delta *. 
                emptyAtt) #. seval

evalExpG :: GammaT -> DeltaT -> ExpG -> Val
evalExpG gamma delta e =
  sem_ExpG asp e (igamma =. gamma
                 *. idelta =. delta *. emptyAtt) #. seval

