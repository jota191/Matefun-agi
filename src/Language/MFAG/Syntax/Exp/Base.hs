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

module Language.MFAG.Syntax.Exp.Base where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals

import Language.MFAG.Syntax.Set.Base as Set


-- Definition of signatures, type of functions
$(addNont "Sig")
$(addProd "Sig" ''Nt_Sig [("dom", NonTer ''Set.Nt_Set),
                          ("cod", NonTer ''Set.Nt_Set)])


{- TODO: no se si va aca, pero es un buen test -}
-- $(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])
--  Obs: The datatypes are defined here, in this module, this will be
-- useful..

  
-- -- | List of variables
-- $(addNont "NVars")
-- since they are a list of terminals, to make a grammar is too much
-- bureaucracy, I'll keep it simple. NVars is defined in Terminals.hs



-- TODO: more values, for now we are ok
$(addNont "Exp")
$(addProd "Lit"   ''Nt_Exp [("lit_t",     Ter    ''Val)])
$(addProd "Var"   ''Nt_Exp [("var_t",     Ter    ''NVar)])
$(addProd "OpInf" ''Nt_Exp [("op_inf_l",  NonTer ''Nt_Exp),
                            ("op_inf_op", Ter    ''BOp),
                            ("op_inf_r",  NonTer ''Nt_Exp)])
$(addProd "OpPre" ''Nt_Exp [("op_pre_op", Ter    ''UOp),
                            ("op_pre_e",  NonTer ''Nt_Exp)])

-- | Application, that is, an abstract call to a function.
$(addProd "App"   ''Nt_Exp [("app_f",     Ter    ''NFun),
                            ("app_e",     NonTer ''Nt_Exp)])


-- $(addProd "Tuple")

-- $(addProd "Tuple_Cons" ''Nt_Exp [("tuple_h", Nonter ''Nt_Exp)]


$(addNont "Cond")
$(addProd "Top" ''Nt_Cond [])

-- function body definiitons (cases)
$(addNont "ExpG")

$(addProd "ExpGIf" ''Nt_ExpG [("expGIf_e", NonTer ''Nt_Exp),
                              ("expGIf_cond", NonTer ''Nt_Cond),
                              ("expGIf_tail", NonTer ''Nt_ExpG)])
$(addProd "ExpGOr" ''Nt_ExpG [("expGOr_e", NonTer ''Nt_Exp)])

-- algorithm definitions
$(addNont "Ecu")
$(addProd "Ecu" ''Nt_Ecu [("ecu_l", Ter ''NVars),
                          ("ecu_r", NonTer ''Nt_ExpG)])

-- function definitons
$(addNont "FDef")
$(addProd "FDef" ''Nt_FDef [("nfun", Ter ''NFun),
                            ("fun_sig",  NonTer ''Nt_Sig),
                            ("fun_body", NonTer ''Nt_Ecu)])

-- function definitons
$(addNont "FDef")
$(addProd "FDef" ''Nt_FDef [("nfun", Ter ''NFun),
                            ("fun_sig",  NonTer ''Nt_Sig),
                            ("fun_body", NonTer ''Nt_Ecu)])

-- | Unfolded application
$(addProd "AppU"   ''Nt_Exp [("appu_ecu",   NonTer ''Nt_Ecu),
                             ("appu_e",     NonTer ''Nt_Exp)])
