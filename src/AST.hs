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

module AST where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
--import Data.GenRec -- should be exported by AAG, perhaps?

import Terminals

import Data.Singletons

--import Data.Singletons
-- cond

$(addNont "Exp")
$(addProd "Lit"   ''Nt_Exp [("lit_t",     Ter    ''Val)])
$(addProd "Var"   ''Nt_Exp [("var_t",     Ter    ''NVar)])
$(addProd "Op_Inf" ''Nt_Exp [("op_inf_l",  NonTer ''Nt_Exp),
                            ("op_inf_op", Ter    ''BOp),
                            ("op_inf_r",  NonTer ''Nt_Exp)])

$(addNont "Cond")
$(addProd "Top" ''Nt_Cond [])
$(addProd "Equa" ''Nt_Cond [("equa_l", NonTer ''Nt_Exp),
                            ("equa_op", Ter   ''EOp),
                            ("equa_r", NonTer ''Nt_Exp)])
$(addProd "And" ''Nt_Cond [("and_l", NonTer ''Nt_Cond),
                           ("and_r", NonTer ''Nt_Cond)])
$(addProd "Neg" ''Nt_Cond [("neg_e", NonTer ''Nt_Cond)])

$(addNont "Set")
$(addProd "Set" ''Nt_Set [("set_sort", Ter ''Sort),
                          ("set_xs", Ter ''NVars),
                          ("set_ref", NonTer ''Nt_Cond)])

-- Definition of signatures, type of functions
$(addNont "Sig")
$(addProd "Sig" ''Nt_Sig [("sig_dom", NonTer ''Nt_Set),
                          ("sig_cod", NonTer ''Nt_Set)])


-- | Application, that is, an abstract call to a function.
$(addProd "App"   ''Nt_Exp [("app_fun",     Ter    ''NFun),
                            ("app_arg",     NonTer ''Nt_Exp)])


-- function body definiitons (cases)
$(addNont "ExpG")

$(addProd "If" ''Nt_ExpG [("if_exp", NonTer ''Nt_Exp),
                           ("if_cond", NonTer ''Nt_Cond),
                           ("if_else", NonTer ''Nt_ExpG)])
$(addProd "Or" ''Nt_ExpG [("or_exp", NonTer ''Nt_Exp)])


-- algorithm definitions
$(addNont "Ecu")
$(addProd "Ecu" ''Nt_Ecu [("ecu_l", Ter ''NVars),
                          ("ecu_r", NonTer ''Nt_ExpG)])

-- function definitons
$(addNont "FDef")
$(addProd "FDef" ''Nt_FDef [("fDef_name", Ter ''NFun),
                            ("fDef_sig",  NonTer ''Nt_Sig),
                            ("fDef_body", NonTer ''Nt_Ecu)])

-- base expression syntax generation
$(closeNTs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
            ''Nt_ExpG, ''Nt_Cond,
            ''Nt_Ecu, ''Nt_FDef])
$(mkSemFuncs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
              ''Nt_ExpG, ''Nt_Cond,
              ''Nt_Ecu, ''Nt_FDef])

