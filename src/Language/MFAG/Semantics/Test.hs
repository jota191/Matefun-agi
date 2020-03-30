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

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Set.Base     as Set

import Language.MFAG.Syntax.Exp.Core

import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Semantics.Env
import Language.MFAG.Semantics.Unfold


ex_Gamma
  = [add,twice]

add = FDef "add" addSig
  (Ecu ["x","y"]
    (ExpGOr $ OpInf (Var "x") "+" (Var "y") ))

twice = FDef "twice" twiceSig
  (Ecu ["x"]
    (ExpGOr $ OpInf (Lit $ ValR 2) "+" (Var "x") ))

addSig = undefined
twiceSig = undefined

unfold :: TGamma -> Exp -> Exp
unfold gamma e
  = sem_Exp (asp_unfold_All) e
    ( igamma =. gamma *. emptyAtt) #. sidExpC


--e 1 = App "twice" (OpInf (Lit $ ValR 2) "+" (Lit $ ValR 2))
--t 1 = unfold ex_Gamma $ e 1
