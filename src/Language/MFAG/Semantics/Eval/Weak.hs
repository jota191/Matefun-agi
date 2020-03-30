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

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Language.MFAG.Semantics.Eval.Weak.Exp where

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
$(closeNTs [''Nt_Exp, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu,  ''Nt_FDef])


-- Val implements Num interface
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

-- Val implements Fractional interface
instance Fractional Val where
  (ValR a) / (ValR b) = ValR $ a / b
  (ValR a) / (ValZ b) = ValR $ a / fromInteger b
  (ValZ a) / (ValR b) = ValR $ fromInteger a / b
  (ValZ a) / (ValZ b) = case (mod a b) of
                          0 -> ValZ $ a `div` b
                          _ -> ValR $ fromInteger a / fromInteger b
  recip (ValR a) = ValR (1 / a)
  recip (ValZ 1) = ValZ 1
  recip (ValZ a) = ValR (1 / fromInteger a)

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
-- $(attLabels [("idVal", ''Val)])
-- asp_idVal
--   =   (syn idVal p_ValZ (ValZ <$> ter ch_valZ_t))
--   .+: (syn idVal p_ValR (ValR <$> ter ch_valR_t))
--   .+: (syn idVal p_ValC (ValC <$> ter ch_valC_t))
--   .+: (syn idVal p_ValC2 (ValC <$> ter ch_valC2_t))
--   .+: emptyAspect
------------------------------------------------

asp_eval_Exp
  =   (syn seval p_Lit   $ ter ch_lit_t)
  .+: (syn seval p_Var   $ do env     <- at lhs ienv
                              ch_nam  <- ter ch_var_t
                              return $ fromJust $ M.lookup ch_nam env
      )
  .+: (syn seval p_OpInf $ do l  <- at ch_op_inf_l seval
                              r  <- at ch_op_inf_r seval
                              op <- ter ch_op_inf_op
                              return $ computeInf l op r
      )
  .+: (syn seval p_OpPre $ do op <- ter ch_op_pre_op
                              e  <- at ch_op_pre_e seval
                              return $ computePre op e
      )
  .+: (syn seval p_App   $ notImpl)
  .+: emptyAspect

computeInf l op r
  = case op of
      "+" -> l + r -- recall: Val implements Num
      "-" -> l - r
      "/" -> l / r
      "^" -> case r of
               ValZ r' -> l ^ r'
               _ -> error "exponent is not integral"
      _   -> error "operator not yet implemented"

computePre op e
  = case op of
      "-" -> -e
      _ -> error "operator not yet implemented"

eval_Exp e
  = sem_Exp (asp_eval_Exp .:+: asp_env_Exp) e initialAtt #. seval
  where initialAtt = (ienv =. M.empty *. emptyAtt)


notImpl = error "not yet implemented"


logo2 =
 "   __  ___        __         ____                    ___   _____   ____\n"++
 "  /  |/  / ___ _ / /_ ___   / __/ __ __  ___  ____  / _ | / ___/  /  _/\n"++
 " / /|_/ / / _ `// __// -_) / _/  / // / / _ \\/___/ / __ |/ (_ /  _/ /  \n"++
 "/_/  /_/  \\_,_/ \\__/ \\__/ /_/    \\_,_/ /_//_/     /_/ |_|\\___/  /___/  \n"
