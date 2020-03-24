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

module Language.MFAG.Syntax.Exp.Test where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Set.Base     as Set

import Language.MFAG.Syntax.Exp.Core     as C
import Language.MFAG.Syntax.Exp.Unfolded as U
import Language.MFAG.Syntax.Exp.Base

--- if this compiles, dependencies are ok

-- | tests
testIdU :: U.Exp -> U.Exp
testIdU e
  = U.sem_Exp ( asp_sid_U
               .:+: dummy)
               e emptyAtt #. sidExpU 


dummy = singAsp $ syn sidCond p_Top $ undefined

u 1 = U.Var "x"
u 2 = U.Lit (ValZ 23)
u 3 = U.OpInf (u 1) "+" (u 2)
u 4 = U.App "nomF" $ U.OpInf (u 3) "+" (u 2)
u 5 = U.OpPre "-" $ U.OpInf (u 4) "+" (u 2)
u 6 = U.AppU (U.Ecu ["x","y"] (U.ExpGOr (u 4))) $ u 4

test_id_Unfolded
  = [u i == testIdU (u i) | i <- [1..6]]

testIdC e
  = C.sem_Exp asp_sid_C e emptyAtt #. sidExpC

c 1 = C.Var "x"
c 2 = C.Lit (ValZ 23)
c 3 = C.OpInf (c 1) "+" (c 2)
c 4 = C.OpInf (c 2) "+" (c 2)
c 5 = C.OpPre "-" $ C.OpInf (c 4) "+" (c 2)
c 6 = C.App "nomF" $ C.OpInf (c 4) "+" (c 2)

test_id_Core
  = [c i == testIdC (c i) | i <- [1..6]]

test_id_All = test_id_Core ++ test_id_Unfolded


t = C.sem_Exp asp_sid_ExpCasU (c 5) emptyAtt #. sidExpU
-- TODO: Documentar esto, uso la id de una gramatica
--  restringida a las producciones de la otra
