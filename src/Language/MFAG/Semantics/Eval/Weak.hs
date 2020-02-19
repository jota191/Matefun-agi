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

module Language.MFAG.Semantics.Eval.Weak where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

-- only base syntax
import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Syntax.Terminals

import Data.Map as M
import Data.Maybe (fromJust)

-- only base types
import Language.MFAG.Syntax.Set.Base as Set

-- base expression syntax generation
-- base types syntax generation
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_Val, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_Val,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu,  ''Nt_FDef])


-- Val is a Num
instance Num Val where
  (ValZ a) + (ValZ b) = ValZ $ a + b
  (ValR a) + (ValR b) = ValR $ a + b
  (ValR a) + (ValZ b) = ValR $ a + fromInteger b
  (ValZ a) + (ValR b) = ValR $ fromInteger a + b
  
  (ValZ a) * (ValZ b) = ValZ $ a * b
  (ValR a) * (ValR b) = ValR $ a * b
  (ValR a) * (ValZ b) = ValR $ a * fromInteger b
  (ValZ a) * (ValR b) = ValR $ fromInteger a * b

  abs (ValR a) = ValR $ abs a
  abs (ValZ a) = ValZ $ abs a

  fromInteger  = ValZ

  negate (ValR a) = ValR $ negate a
  negate (ValZ a) = ValZ $ negate a

  signum (ValR a) = ValR $ signum a
  signum (ValZ a) = ValZ $ signum a

    
-- examples
e1 = Lit $ ValZ $ 32 

--  First, let us define a simple evaluation, partial since terms
-- could be ill formed

-- evaluation attribute
$(attLabels [("seval", ''Val)])

-- by now, environments are only values, no fun env
type Env = M.Map NVar Val

-- eval attribute
$(attLabels [("ienv", ''Env)])

asp_env_Exp
  = let la = return (M.empty :: Env)
    in (
      (inh ienv p_OpInf ch_op_inf_l $ at lhs ienv)
  .+: (inh ienv p_OpInf ch_op_inf_r $ at lhs ienv)
  .+: (inh ienv p_OpPre ch_op_pre_e $ at lhs ienv)
  .+: (inh ienv p_App   ch_app_e    $ at lhs ienv)
  .+: emptyAspect )

---------identity-------------------------------
$(attLabels [("idVal", ''Val)])
asp_idVal
  =   (syn idVal p_ValZ (ValZ <$> ter ch_valZ_t))
  .+: (syn idVal p_ValR (ValR <$> ter ch_valR_t))
  .+: (syn idVal p_ValC (ValC <$> ter ch_valC_t))
  .+: (syn idVal p_ValC2 (ValC <$> ter ch_valC2_t))
  .+: emptyAspect
------------------------------------------------

asp_eval_Exp
  =   (syn seval p_Lit   $ at ch_lit_c idVal)
  .+: (syn seval p_Var   $ do env     <- at lhs ienv
                              ch_nam  <- ter ch_var_t
                              return $ fromJust $ M.lookup ch_nam env
      )
  .+: (syn seval p_OpInf $ do l  <- at ch_op_inf_l seval
                              r  <- at ch_op_inf_r seval
                              op <- ter ch_op_inf_op
                              return $ computeInf l op r
      )
  .+: (syn seval p_OpPre $ notImpl)
  .+: (syn seval p_App   $ notImpl)
  .+: emptyAspect

computeInf l op r
  = case op of
      "+" -> l + r -- recall: Val implements Num
      "-" -> l - r
      _   -> error "operator not yet implemented"

lala' = (asp_env_Exp .:+: asp_eval_Exp .:+: asp_idVal)

eval_Exp e
  = sem_Exp (asp_eval_Exp .:+: asp_idVal .:+: asp_env_Exp) e initialAtt #. seval
  where initialAtt = (ienv =. M.empty *. emptyAtt)


notImpl = error "not yet implemented"
