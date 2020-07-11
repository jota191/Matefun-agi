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


module Language.MFAG.Sigma where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import Language.MFAG.SemFuncs
import Language.MFAG.Utils
import Language.MFAG.Delta

import Control.Applicative
import Prelude hiding (exp)
import qualified Data.Map as M

import Language.MFAG.Attributes

import Control.Monad

type GammaT = M.Map NVar Val

-- Γ attribute, i.e. vatiable environment
$(attLabels [("igamma" , ''GammaT)])

asp_igamma = AspAll
  asp_igamma_Set
  asp_igamma_Sig
  asp_igamma_Exp
  asp_igamma_ExpG
  asp_igamma_Cond
  asp_igamma_Ecu
  asp_igamma_FDef
  asp_igamma_Tuple

asp_igamma_Set   =
  igamma `copyAtChi` ch_refinement .+:
  emptyAspect
  
asp_igamma_Sig   =
  igamma `copyAtChi` ch_dom .+:
  igamma `copyAtChi` ch_cod .+:
  emptyAspect

asp_igamma_Exp   =
  igamma `copyAtChi` ch_op_inf_l .+:
  igamma `copyAtChi` ch_op_inf_r .+:
  asp_igamma_app                 .+:
  igamma `copyAtChi` ch_eprod_e  .+:
  igamma `copyAtChi` ch_index_e  .+:
  emptyAspect

asp_igamma_app = inh igamma p_App ch_app_e $
  do f        <- ter ch_app_f
     evArg    <- at ch_app_e eval
     oldGamma <- at lhs igamma
     delta    <- at lhs idelta
     let fun = M.lookup f delta
     case fun of
       Nothing -> error "funtion not in scope"
       Just (FDef _ sig (Ecu vars expg)) ->
         do
           when (length vars /= arity evArg)
             (error "arg arity mismatch")
           let newCtxElems = M.fromList (zip vars (untup evArg))
           return (newCtxElems `M.union` oldGamma)

asp_igamma_ExpG  =
  igamma `copyAtChi` ch_expGIf_e    .+:
  igamma `copyAtChi` ch_expGIf_cond .+:
  igamma `copyAtChi` ch_expGIf_tail .+:
  igamma `copyAtChi` ch_expGOr_e    .+:
  emptyAspect

asp_igamma_Cond  =
  igamma `copyAtChi` ch_equa_l .+:
  igamma `copyAtChi` ch_equa_r .+:
  igamma `copyAtChi` ch_and_l  .+:
  igamma `copyAtChi` ch_and_l  .+:
  emptyAspect

asp_igamma_Ecu   = undefined
  -- igamma `copyAtChi` ch_ecu_r .+:
  -- emptyAspect
  
asp_igamma_FDef  = undefined
--   igamma `copyAtChi` ch_fun_sig .+:
--   igamma `copyAtChi` ch_fun_body .+:
--   emptyAspect

asp_igamma_Tuple =
  igamma `copyAtChi` ch_tuple_h .+:
  igamma `copyAtChi` ch_tuple_t .+:
  igamma `copyAtChi` ch_tuple_s .+:
  emptyAspect
