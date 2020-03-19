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

module Language.MFAG.Semantics.Eval.Weak.FDef where

import Language.MFAG.Semantics.Eval.Weak.Exp
import Language.MFAG.Semantics.Eval.Weak.ExpG
import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Syntax.Terminals

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

--import Data.Set as S

