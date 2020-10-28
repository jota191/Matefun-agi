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

module ASTSpec where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Terminals


-- $(addNont "Exp")
-- $(addProd "Lit"   ''Nt_Exp [("lit_t",     Ter    ''Val)])
-- $(addProd "Var"   ''Nt_Exp [("var_t",     Ter    ''NVar)])
-- $(addProd "Op_Inf" ''Nt_Exp [("op_inf_l",  NonTer ''Nt_Exp),
--                             ("op_inf_op", Ter    ''BOp),
--                             ("op_inf_r",  NonTer ''Nt_Exp)])

-- $(addNont "Cond")
-- $(addProd "Top" ''Nt_Cond [])
-- $(addProd "Equa" ''Nt_Cond [("equa_l", NonTer ''Nt_Exp),
--                             ("equa_op", Ter   ''EOp),
--                             ("equa_r", NonTer ''Nt_Exp)])
-- $(addProd "And" ''Nt_Cond [("and_l", NonTer ''Nt_Cond),
--                            ("and_r", NonTer ''Nt_Cond)])
-- $(addProd "Neg" ''Nt_Cond [("neg_e", NonTer ''Nt_Cond)])

-- $(addNont "Set")
-- $(addProd "Set" ''Nt_Set [("set_sort", Ter ''Sort),
--                           ("set_xs", Ter ''NVars),
--                           ("set_ref", NonTer ''Nt_Cond)])

-- -- Definition of signatures, type of functions
-- $(addNont "Sig")
-- $(addProd "Sig" ''Nt_Sig [("sig_dom", NonTer ''Nt_Set),
--                           ("sig_cod", NonTer ''Nt_Set)])


-- -- | Application, that is, an abstract call to a function.
-- $(addProd "App"   ''Nt_Exp [("app_fun",     Ter    ''NFun),
--                             ("app_arg",     NonTer ''Nt_Exp)])


-- -- function body definiitons (cases)
-- $(addNont "ExpG")

-- $(addProd "If" ''Nt_ExpG [("if_exp", NonTer ''Nt_Exp),
--                            ("if_cond", NonTer ''Nt_Cond),
--                            ("if_else", NonTer ''Nt_ExpG)])
-- $(addProd "Or" ''Nt_ExpG [("or_exp", NonTer ''Nt_Exp)])


-- -- algorithm definitions
-- $(addNont "Ecu")
-- $(addProd "Ecu" ''Nt_Ecu [("ecu_l", Ter ''NVars),
--                           ("ecu_r", NonTer ''Nt_ExpG)])

-- -- function definitons
-- $(addNont "FDef")
-- $(addProd "FDef" ''Nt_FDef [("fDef_name", Ter ''NFun),
--                             ("fDef_sig",  NonTer ''Nt_Sig),
--                             ("fDef_body", NonTer ''Nt_Ecu)])

-- -- base expression syntax generation
-- $(closeNTs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
--             ''Nt_ExpG, ''Nt_Cond,
--             ''Nt_Ecu, ''Nt_FDef])
-- $(mkSemFuncs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
--               ''Nt_ExpG, ''Nt_Cond,
--               ''Nt_Ecu, ''Nt_FDef])


nt_Exp = Label :: Label ('NT "Exp")
type Nt_Exp = 'NT "Exp"

type P_Lit = 'Prd "Lit" Nt_Exp
p_Lit = Label :: Label ('Prd "Lit" Nt_Exp)
instance Prods Nt_Exp "Lit" '[ '("lit_t", "Val")]
ch_lit_t = Label :: Label ('Chi "lit_t" P_Lit (Terminal Val))

type P_Var = 'Prd "Var" Nt_Exp
p_Var = Label :: Label ('Prd "Var" Nt_Exp)
instance Prods Nt_Exp "Var" '[ '("var_t", "NVar")]
ch_var_t = Label :: Label ('Chi "var_t" P_Var (Terminal NVar))

type P_Op_Inf = 'Prd "Op_Inf" Nt_Exp
p_Op_Inf = Label :: Label ('Prd "Op_Inf" Nt_Exp)
instance Prods Nt_Exp "Op_Inf" '[ '("op_inf_l", "Nt_Exp"),
                                  '("op_inf_op", "BOp"),
                                  '("op_inf_r", "Nt_Exp")]
ch_op_inf_l
  = Label :: Label ('Chi "op_inf_l" P_Op_Inf (NonTerminal Nt_Exp))
ch_op_inf_op
  = Label :: Label ('Chi "op_inf_op" P_Op_Inf (Terminal BOp))
ch_op_inf_r
  = Label :: Label ('Chi "op_inf_r" P_Op_Inf (NonTerminal Nt_Exp))

nt_Cond = Label :: Label ('NT "Cond")
type Nt_Cond = 'NT "Cond"

type P_Top = 'Prd "Top" Nt_Cond
p_Top = Label :: Label ('Prd "Top" Nt_Cond)
instance Prods Nt_Cond "Top" '[]

type P_Equa = 'Prd "Equa" Nt_Cond
p_Equa = Label :: Label ('Prd "Equa" Nt_Cond)
instance Prods Nt_Cond "Equa" '[ '("equa_l", "Nt_Exp"),
                                 '("equa_op", "EOp"),
                                 '("equa_r", "Nt_Exp")]
ch_equa_l
  = Label :: Label ('Chi "equa_l" P_Equa (NonTerminal Nt_Exp))
ch_equa_op = Label :: Label ('Chi "equa_op" P_Equa (Terminal EOp))
ch_equa_r
  = Label :: Label ('Chi "equa_r" P_Equa (NonTerminal Nt_Exp))

type P_And = 'Prd "And" Nt_Cond
p_And = Label :: Label ('Prd "And" Nt_Cond)
instance Prods Nt_Cond "And" '[ '("and_l", "Nt_Cond"),
                                '("and_r", "Nt_Cond")]
ch_and_l
  = Label :: Label ('Chi "and_l" P_And (NonTerminal Nt_Cond))
ch_and_r
  = Label :: Label ('Chi "and_r" P_And (NonTerminal Nt_Cond))
  
type P_Neg = 'Prd "Neg" Nt_Cond
p_Neg = Label :: Label ('Prd "Neg" Nt_Cond)
instance Prods Nt_Cond "Neg" '[ '("neg_e", "Nt_Cond")]
ch_neg_e
  = Label :: Label ('Chi "neg_e" P_Neg (NonTerminal Nt_Cond))

nt_Set = Label :: Label ('NT "Set")
type Nt_Set = 'NT "Set"

type P_Set = 'Prd "Set" Nt_Set
p_Set = Label :: Label ('Prd "Set" Nt_Set)
instance Prods Nt_Set "Set" '[ '("set_sort", "Sort"),
                               '("set_xs", "NVars"),
                               '("set_ref", "Nt_Cond")]
ch_set_sort
  = Label :: Label ('Chi "set_sort" P_Set (Terminal Sort))
ch_set_xs = Label :: Label ('Chi "set_xs" P_Set (Terminal NVars))
ch_set_ref
  = Label :: Label ('Chi "set_ref" P_Set (NonTerminal Nt_Cond))

nt_Sig = Label :: Label ('NT "Sig")
type Nt_Sig = 'NT "Sig"


type P_Sig = 'Prd "Sig" Nt_Sig
p_Sig = Label :: Label ('Prd "Sig" Nt_Sig)
instance Prods Nt_Sig "Sig" '[ '("sig_dom", "Nt_Set"),
                               '("sig_cod", "Nt_Set")]
ch_sig_dom
  = Label :: Label ('Chi "sig_dom" P_Sig (NonTerminal Nt_Set))
ch_sig_cod
  = Label :: Label ('Chi "sig_cod" P_Sig (NonTerminal Nt_Set))


type P_App = 'Prd "App" Nt_Exp
p_App = Label :: Label ('Prd "App" Nt_Exp)
instance Prods Nt_Exp "App" '[ '("app_fun", "NFun"),
                               '("app_arg", "Nt_Exp")]
ch_app_fun = Label :: Label ('Chi "app_fun" P_App (Terminal NFun))
ch_app_arg
  = Label :: Label ('Chi "app_arg" P_App (NonTerminal Nt_Exp))

nt_ExpG = Label :: Label ('NT "ExpG")
type Nt_ExpG = 'NT "ExpG"

type P_If = 'Prd "If" Nt_ExpG
p_If = Label :: Label ('Prd "If" Nt_ExpG)
instance Prods Nt_ExpG "If" '[ '("if_exp", "Nt_Exp"),
                               '("if_cond", "Nt_Cond"),
                               '("if_else", "Nt_ExpG")]
ch_if_exp
  = Label :: Label ('Chi "if_exp" P_If (NonTerminal Nt_Exp))
ch_if_cond
  = Label :: Label ('Chi "if_cond" P_If (NonTerminal Nt_Cond))
ch_if_else
  = Label :: Label ('Chi "if_else" P_If (NonTerminal Nt_ExpG))

type P_Or = 'Prd "Or" Nt_ExpG
p_Or = Label :: Label ('Prd "Or" Nt_ExpG)
instance Prods Nt_ExpG "Or" '[ '("or_exp", "Nt_Exp")]
ch_or_exp
  = Label :: Label ('Chi "or_exp" P_Or (NonTerminal Nt_Exp))


nt_Ecu = Label :: Label ('NT "Ecu")
type Nt_Ecu = 'NT "Ecu"

type P_Ecu = 'Prd "Ecu" Nt_Ecu
p_Ecu = Label :: Label ('Prd "Ecu" Nt_Ecu)
instance Prods Nt_Ecu "Ecu" '[ '("ecu_l", "NVars"),
                               '("ecu_r", "Nt_ExpG")]
ch_ecu_l = Label :: Label ('Chi "ecu_l" P_Ecu (Terminal NVars))
ch_ecu_r
  = Label :: Label ('Chi "ecu_r" P_Ecu (NonTerminal Nt_ExpG))


nt_FDef = Label :: Label ('NT "FDef")
type Nt_FDef = 'NT "FDef"

type P_FDef = 'Prd "FDef" Nt_FDef
p_FDef = Label :: Label ('Prd "FDef" Nt_FDef)
instance Prods Nt_FDef "FDef" '[ '("fDef_name", "NFun"),
                                 '("fDef_sig", "Nt_Sig"),
                                 '("fDef_body", "Nt_Ecu")]
ch_fDef_name
  = Label :: Label ('Chi "fDef_name" P_FDef (Terminal NFun))
ch_fDef_sig
  = Label :: Label ('Chi "fDef_sig" P_FDef (NonTerminal Nt_Sig))
ch_fDef_body
  = Label :: Label ('Chi "fDef_body" P_FDef (NonTerminal Nt_Ecu))


