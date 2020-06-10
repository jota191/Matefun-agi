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
import Language.MFAG.Syntax.Exp.Core

--- if this compiles, dependencies are ok


-- testIdC :: C.Exp -> C.Exp
-- testIdC e
--   = C.sem_Exp (asp_sid_Core .:+: asp_dummy
--               ) e emptyAtt #. sidExpC



c 1 = C.Var "x"
c 2 = C.Lit (ValZ 23)
c 3 = C.OpInf (c 1) Plus (c 2)
c 4 = C.OpInf (c 2) Plus (c 2)
--c 5 = C.OpPre "-" (C.OpInf (c 4) Plus (c 2)) 
c 6 = C.App "nomF" (C.OpInf (c 4) Plus (c 2))
--c 7 = C.AppU (Ecu ["x"] (ExpGOr (c 6))) (C.OpInf (c 4) "+" (c 2))  
c 8 = C.EProd (TCons (c 5) (TSing (c 3)))

-- test_id_Core
--   = [c i == testIdC (c i) | i <- [1..8]]
