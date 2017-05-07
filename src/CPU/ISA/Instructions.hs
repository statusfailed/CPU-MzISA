{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
module CPU.ISA.Instructions where

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Vector (Vector(..))
import qualified Data.Vector as Vec

import Data.Word

import Control.Applicative
import Control.Monad.State
import Control.Lens
import Control.Lens.TH

-- 256 64-bit registers
-- Special registers:
--    * 255 => program counter
--    * 254 => zero    register
type RegisterName  = Word8
type RegisterValue = Word32
type Immediate16   = Word16

-- A single machine instruction
data Instruction
  = Imm     RegisterName Word16
  | UAdd    RegisterName RegisterName RegisterName -- reg := op1 + op2 (unsigned)
  | USub    RegisterName RegisterName RegisterName -- reg := op1 - op2 (unsigned)
  | Store   RegisterName RegisterName
  | Load    RegisterName RegisterName
  deriving (Eq, Read, Show)
