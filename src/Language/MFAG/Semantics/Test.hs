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

import Language.MFAG.Syntax.Exp.Core     as C
import Language.MFAG.Syntax.Exp.Unfolded as U
import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Semantics.Env
import Language.MFAG.Semantics.Unfold


ex_Gamma
  = [add,twice]

add = C.FDef "add" addSig
  (C.Ecu ["x","y"]
    (C.ExpGOr $ C.OpInf (C.Var "x") "+" (C.Var "y") ))

twice = C.FDef "twice" twiceSig
  (C.Ecu ["x"]
    (C.ExpGOr $ C.OpInf (C.Lit $ ValR 2) "+" (C.Var "x") ))

addSig = undefined
twiceSig = undefined


e 1 = C.App "twice" (C.OpInf (C.Lit $ ValR 2) "+" (C.Lit $ ValR 2))

t 1 = unfold ex_Gamma $ e 1
