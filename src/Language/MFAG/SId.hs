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


module Language.MFAG.SId where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Data.GenRec -- should be exported by AAG, perhaps?

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import  Language.MFAG.SemFuncs

import Prelude hiding (exp)
import Data.Singletons


-- | Identity attribute
$(attLabels [("sidExpC" , ''Exp)  ])
$(attLabels [("sidEcu"  , ''Ecu)  ])
$(attLabels [("sidExpG" , ''ExpG) ])
$(attLabels [("sidCond" , ''Cond) ])
$(attLabels [("sidTuple", ''Tuple)])
$(attLabels [("sidFDef" , ''FDef) ])
$(attLabels [("sidSig"  , ''Sig)  ])
$(attLabels [("sidSet"  , ''Set)  ])
-- emptyAspect =
--   emptyAspectC (p_And .:. p_App .:. p_EProd .:. p_Ecu .:. p_Equa
--                 .:. p_ExpGIf .:. p_ExpGOr .:. p_Lit .:. p_Neg .:.
--                 p_OpInf .:. p_TCons .:. p_TSing .:. p_Top .:. p_Var .:. p_FDef
--                 .:. p_Sig .:. eL) Proxy

asp_sid_Cond =
  syn sidCond p_Top (return Top)
  .+:
  (syn sidCond p_Equa
    $ Equa <$> at ch_equa_l sidExpC
           <*> ter ch_equa_op
           <*> at ch_equa_r sidExpC
  )
  .+:
  (syn sidCond p_And
    $ And <$> at ch_and_l sidCond <*> at ch_and_r sidCond
  )
  .+:
  (syn sidCond p_Neg
    $ Neg <$> at ch_neg_e sidCond
  )
  .+:
  emptyAspect
  
-- | identity aspect for core expressions
asp_sid_Exp
  =
  (syn sidExpC p_Lit
    $ Lit <$> ter ch_lit_t
  )
  .+:
  (syn sidExpC p_Var
    $ Var <$> ter ch_var_t
  )
  .+:
  (syn sidExpC p_OpInf
    $ OpInf <$> at  ch_op_inf_l sidExpC
            <*> ter ch_op_inf_op
            <*> at  ch_op_inf_r sidExpC
  )
  .+:
  (syn sidExpC p_App
    $ App   <$> ter ch_app_f
            <*> at ch_app_e sidExpC
  )
  .+:
  (syn sidExpC p_EProd
    $ EProd <$> at ch_eprod_e sidTuple
  )
  .+:
  (syn sidExpC p_Index
    $ Index <$> at ch_index_e sidExpC <*> ter ch_index_i
  )
  .+:
  emptyAspect



-- | identity for Ecu
asp_sid_Ecu
  = (syn sidEcu p_Ecu
       $ Ecu <$> ter ch_ecu_l
             <*> at ch_ecu_r sidExpG
    )
  .+: emptyAspect

-- | Identity for ExpG
asp_sid_ExpG
  =   (syn sidExpG p_ExpGIf
         $ ExpGIf <$> at ch_expGIf_e    sidExpC
                  <*> at ch_expGIf_cond sidCond
                  <*> at ch_expGIf_tail sidExpG
      )
  .+: 
  (syn sidExpG p_ExpGOr
         $ ExpGOr <$> at ch_expGOr_e sidExpC
      )
  .+: emptyAspect

asp_sid_Tuple
  =   (syn sidTuple p_TCons
         $ TCons <$> at ch_tuple_h sidExpC <*> at ch_tuple_t sidTuple
      )
  .+: (syn sidTuple p_TSing
         $ TSing <$> at ch_tuple_s sidExpC
      )
  .+: emptyAspect

asp_sid_FDef = singAsp (syn sidFDef p_FDef
  $ FDef <$> ter ch_nfun <*> at ch_fun_sig sidSig <*> at ch_fun_body sidEcu)

--asp_sid_Core = asp_sid_Core' .:+: asp_sid_Core''

asp_sid_Core' =
       asp_sid_Ecu
  .:+: asp_sid_Cond
  .:+: asp_sid_Exp

asp_sid_Core'' =
       asp_sid_ExpG
  .:+: asp_sid_Tuple
  .:+: asp_sid_FDef


asp_sid_Sig = singAsp
  $ syn sidSig p_Sig (Sig <$> at ch_dom sidSet <*> at ch_cod sidSet)

asp_sid_Set = singAsp
  $ syn sidSet p_Set (Set <$> ter ch_sort
                       <*> ter ch_xs
                       <*> at ch_refinement sidCond)

aspSId = AspAll
  asp_sid_Set
  asp_sid_Sig
  asp_sid_Exp
  asp_sid_ExpG
  asp_sid_Cond
  asp_sid_Ecu
  asp_sid_FDef
  asp_sid_Tuple

idFDef :: FDef -> FDef
idFDef f = sem_FDef aspSId f emptyAtt #. sidFDef

r = Set R [] Top

c 1 = Var "x"
c 2 = Lit (ValZ 23)
c 3 = OpInf (c 1) Plus (c 2)
c 4 = OpInf (c 2) Plus (c 2)
--c 5 = OpPre "-" (OpInf (c 4) Plus (c 2)) 
c 6 = App "nomF" (OpInf (c 4) Plus (c 2))
--c 7 = AppU (Ecu ["x"] (ExpGOr (c 6))) (OpInf (c 4) "+" (c 2))  
c 8 = EProd (TCons (c 4) (TSing (c 3)))

-- test_id_Core
--   = [c i == testIdC (c i) | i <- [1..8]]
f = FDef "f" (Sig r r) (Ecu ["x"] $ ExpGOr $ c 8)
