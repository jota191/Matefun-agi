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


module Language.MFAG.Attributes where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import Language.MFAG.SemFuncs
import Language.MFAG.Utils
import Language.MFAG.Delta

import Control.Applicative
import Prelude hiding (exp)
import qualified Data.Map as M

import Control.Monad
import Data.Singletons

type GammaT = M.Map NVar Val

-- Γ attribute, i.e. vatiable environment
$(attLabels [("igamma" , ''GammaT)])

-- evaluation attribute
$(attLabels [("seval" , ''Val)])

-- PrettyPrint attribute
$(attLabels [("spp" , ''String)])
