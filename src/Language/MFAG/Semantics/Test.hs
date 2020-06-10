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

module Language.MFAG.Semantics.Test where

import Data.Proxy 
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Set.Base     as Set

import Language.MFAG.Syntax.Exp.Core

import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Semantics.Env
import Language.MFAG.Semantics.Unfold
-- import Language.MFAG.Semantics.PrettyPrintCore
import Language.MFAG.Semantics.Eval

ex_Gamma
  = [add, twice, inv, partes]

add = FDef "add" addSig
  (Ecu ["x","y"]
    (ExpGOr $ OpInf (Var "x") "+" (Var "y") ))

twice = FDef "twice" twiceSig
  (Ecu ["x"]
    (ExpGOr $ OpInf (Lit $ ValR 2) "+" (Var "x") ))

inv = FDef "inv" undefined
  $ Ecu ["x"] $ ExpGIf (Var "x") (Equa (Var "x") ">" (Lit (ValR 0)))
  (ExpGOr (OpPre "-" $ Var "x"))

partes = FDef "par" undefined
  $ Ecu ["x"] $ ExpGIf (Var "x") (Equa (Var "x") ">" (Lit (ValR 0))) $
  ExpGIf (Var "x") (Equa (Var "x") ">" (Lit (ValR 0))) (ExpGOr (OpPre "-" $ Var "x"))


addSig = undefined
twiceSig = undefined

unfold :: TGamma -> Exp -> Exp
unfold gamma e
  = sem_Exp asp_unfold_All e
    ( igamma =. gamma *. emptyAtt) #. sidExpC

e 1 = App "twice" (OpInf (Lit $ ValR 2) "+" (Lit $ ValR 2))
e 2 = App "inv" (Lit $ ValR 2)


t i = unfold ex_Gamma $ e i


eval :: TGamma -> Env -> Exp -> Val
eval gamma env e = sem_Exp (asp_eval .:+: asp_env
                            .:+: asp_iGamma .:+: asp_sid_Core
     .:+: (emptyAspectC (p_Top .:. eL) Proxy))
  e
  (ienv =. env   *. igamma =. gamma *. emptyAtt) #. seval
