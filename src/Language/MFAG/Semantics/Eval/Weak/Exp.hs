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


$(attLabels [("idExp", ''Exp)])

-- | function environment type
type TGamma = [FDef] -- TODO: Refine


-- | lookup a function by name in a gamma environment
lookupFun ::  NFun -> TGamma -> Maybe FDef
lookupFun f []
  = Nothing
lookupFun f (fdef@(FDef f' _ _) : fdefs)
  | f == f'   = Just fdef
  | otherwise = lookupFun f fdefs


-- | function environment
$(attLabels [("igamma", ''TGamma)])

-- by now, environments are only values, no fun env
type Env = M.Map NVar Val

-- eval attribute
$(attLabels [("ienv", ''Env)])

--  First, let us define a simple evaluation, partial since terms
-- could be ill formed

-- evaluation attribute
$(attLabels [("seval", ''Val)])


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
  .+: (syn seval p_App $ at ch_app_e seval
      )
  .+: emptyAspect

computeInf l op r
  = case op of
      "+" -> l + r -- recall: Val implements Num
      "-" -> l - r
      "/" -> l / r
      "*" -> l / r
      "^" -> case r of
               ValZ r' -> l ^ r'
               _ -> error "exponent is not integral"
      _   -> error "operator not yet implemented"

computePre op e
  = case op of
      "-" -> -e
      _ -> error "operator not yet implemented"

-- asp_env_Exp
--   =   (inh ienv p_OpInf ch_op_inf_l $ at lhs ienv)
--   .+: (inh ienv p_OpInf ch_op_inf_r $ at lhs ienv)
--   .+: (inh ienv p_OpPre ch_op_pre_e $ at lhs ienv)
--   -- .+: (inh ienv p_App ch_app_e$ return undefined)
--   .+: rule_ienv_App
--   .+: emptyAspect



-- asp_env_Exp
--   =   (inh ienv p_OpInf ch_op_inf_l $ at lhs ienv)
--   .+: (inh ienv p_OpInf ch_op_inf_r $ at lhs ienv)
--   .+: (inh ienv p_OpPre ch_op_pre_e $ at lhs ienv)
--   -- .+: (inh ienv p_App ch_app_e$ return undefined)
--   .+: rule_ienv_App
--   .+: emptyAspect

-- ---------identity-------------------------------
-- -- $(attLabels [("idVal", ''Val)])
-- -- asp_idVal
-- --   =   (syn idVal p_ValZ (ValZ <$> ter ch_valZ_t))
-- --   .+: (syn idVal p_ValR (ValR <$> ter ch_valR_t))
-- --   .+: (syn idVal p_ValC (ValC <$> ter ch_valC_t))
-- --   .+: (syn idVal p_ValC2 (ValC <$> ter ch_valC2_t))
-- --   .+: emptyAspect
-- ------------------------------------------------

-- asp_eval_Exp
--   =   (syn seval p_Lit   $ ter ch_lit_t)
--   .+: (syn seval p_Var   $ do env     <- at lhs ienv
--                               ch_nam  <- ter ch_var_t
--                               return $ fromJust $ M.lookup ch_nam env
--       )
--   .+: (syn seval p_OpInf $ do l  <- at ch_op_inf_l seval
--                               r  <- at ch_op_inf_r seval
--                               op <- ter ch_op_inf_op
--                               return $ computeInf l op r
--       )
--   .+: (syn seval p_OpPre $ do op <- ter ch_op_pre_op
--                               e  <- at ch_op_pre_e seval
--                               return $ computePre op e
--       )
--   .+: (syn seval p_App $ at ch_app_e seval)
--   .+: emptyAspect



-- notImpl = error "not yet implemented"


-- -- | function environment flows down
-- asp_gamma_Exp
--   =   (inh igamma p_OpInf  ch_op_inf_l    $ at lhs igamma)
--   .+: (inh igamma p_OpInf  ch_op_inf_r    $ at lhs igamma)
--   .+: (inh igamma p_OpPre  ch_op_pre_e    $ at lhs igamma)
--   .+: (inh igamma p_App    ch_app_e       $ at lhs igamma)
--   -- .+: (inh igamma p_Ecu    ch_ecu_r       $ at lhs igamma)
--   -- .+: (inh igamma p_FDef   ch_fun_body    $ at lhs igamma)
--   .+: emptyAspect



-- asp_Exp = (asp_eval_Exp .:+: asp_env_Exp .:+: asp_gamma_Exp .:+: asp_id_Exp)

-- -- eval_Exp e
-- --   = sem_Exp asp_Exp e initialAtt #. seval
-- --   where initialAtt = (igamma =. [] *. ienv =. M.empty *. emptyAtt)
--                       -- ordering the other way fails


-- asp_env_ExpG
--   =   (inh ienv p_ExpGIf ch_expGIf_e    $ at lhs ienv)
--   .+: (inh ienv p_ExpGIf ch_expGIf_cond $ at lhs ienv)
--   .+: (inh ienv p_ExpGIf ch_expGIf_tail $ at lhs ienv)
--   .+: (inh ienv p_ExpGOr ch_expGOr_e    $ at lhs ienv)
--   .+: emptyAspect

-- asp_eval_ExpG
--   =   (syn seval p_ExpGIf $ at ch_expGIf_e seval)
--   .+: (syn seval p_ExpGOr $ at ch_expGOr_e seval)
--   .+: (syn ienv p_Top $ undefined) -- TODO: why this is needed? -only one-
--   -- .+: (syn igamma p_Top $ undefined) -- TODO: why this is needed? -only one-
--   -- .+: (syn seval p_Top $ undefined) -- TODO: why this is needed? -only one-
--   .+: emptyAspect


-- asp_gamma_ExpG
--   =  (inh igamma p_ExpGIf ch_expGIf_e    $ at lhs igamma)
--  -- .+: (inh igamma p_ExpGIf ch_expGIf_cond $ at lhs igamma)
--  .+: (inh igamma p_ExpGIf ch_expGIf_tail $ at lhs igamma)
--  .+: (inh igamma p_ExpGOr ch_expGOr_e    $ at lhs igamma)
--  .+: emptyAspect

-- asp_ExpG_alone = asp_eval_ExpG  .:+: asp_env_ExpG .:+: asp_gamma_ExpG
--      -- TODO: this order is important (it should not)
-- asp_ExpG
--   = asp_Exp .:+: asp_ExpG_alone

-- -- eval_ExpG e
-- --   = sem_ExpG (asp_ExpG)
-- --     e initialAtt #. seval
-- --   where initialAtt = (igamma =. [] *. ienv =. M.empty *. emptyAtt)

-- e2 = ExpGOr (OpInf e1 "*" e1)

-- logo2 =
--  "   __  ___        __         ____                    ___   _____   ____\n"++
--  "  /  |/  / ___ _ / /_ ___   / __/ __ __  ___  ____  / _ | / ___/  /  _/\n"++
--  " / /|_/ / / _ `// __// -_) / _/  / // / / _ \\/___/ / __ |/ (_ /  _/ /  \n"++
--  "/_/  /_/  \\_,_/ \\__/ \\__/ /_/    \\_,_/ /_//_/     /_/ |_|\\___/  /___/\n"


-- asp_id_Exp
--   =   (syn idExp p_Lit   $ Lit   <$> ter ch_lit_t)
--   .+: (syn idExp p_Var   $ Var   <$> ter ch_var_t)
--   .+: (syn idExp p_OpInf $ OpInf <$> at ch_op_inf_l idExp
--                          <*> ter ch_op_inf_op <*> at ch_op_inf_l idExp)
--   .+: (syn idExp p_OpPre $ OpPre <$> ter ch_op_pre_op
--                          <*> at ch_op_pre_e idExp)
--   .+: (syn idExp p_App   $ App   <$> ter ch_app_f <*> at ch_app_e idExp)
--   .+: emptyAspect



-- asp_seval_Ecu
--   = (syn seval p_Ecu $ at ch_ecu_r seval) .+: emptyAspect

-- asp_ienv_Ecu
--   = (inh ienv p_Ecu ch_ecu_r $ at lhs ienv) .+: emptyAspect

-- asp_igamma_Ecu
--   = (inh igamma p_Ecu ch_ecu_r $ at lhs igamma) .+: emptyAspect

-- assert :: Monad m => Bool -> String -> m ()
-- assert True  = \_ -> return ()
-- assert False = error

-- --asp_igamma_App
-- --  = singAsp $ (inh igamma p_App ch_app_e $ at lhs igamma)

-- rule_seval_App
--   = (syn seval p_App $ at ch_app_e seval)

-- rule_ienv_App
--   = (inh ienv p_App ch_app_e
--       $ do n_fun   <- ter ch_app_f
--            p_gamma <- at lhs igamma
--            p_env   <- at lhs ienv
--            e_val   <- at ch_app_e seval
--            let fun = lookupFun n_fun p_gamma
--            case fun of
--              Nothing   -> error "function not defined"
--              Just (FDef _ sig (Ecu vars e))
--                -> do assert (arity e_val == length vars) ""
--                        -- TODO             
--                      return (fromList (zipWith (,) vars (untup e_val))
--                        `M.union` p_env))

-- asp_All
--   = asp_ExpG .:+: -- (asp_seval_Ecu .:+:
--   asp_ienv_Ecu .:+:
--                    asp_igamma_Ecu -- )
  


-- -- | finally, the proper evaluatior
-- eval :: TGamma -> Exp -> Val
-- eval gamma e
--  = sem_Exp asp_ExpG e ((igamma =. gamma) *. (ienv =. M.empty) *. emptyAtt)
--  #. seval
