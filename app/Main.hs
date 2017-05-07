{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
module Main where

import qualified Data.Map as Map
import Control.Monad.State
import CPU

fibs :: [Instruction]
fibs =
  [ Imm   10 0x1000 -- memory-mapped output device
  , Store 10 10     -- test output
  , Imm   11 5      -- Backward skip (need to set this up; we have no immediates for add/sub)
  , Imm   0 0       -- need a zero constant to fake "Mov"

  , Imm   1 1       -- fibonacci state registers
  , Imm   2 1
  , UAdd  3 2 1     -- r3 = r2 + r1
  , UAdd  1 2 0     -- r1 := r2       (mov)
  , UAdd  2 3 0     -- r2 := r3       (mov)
  , Store 10 2      -- output(r2)
  , USub  255 255 11 -- pc := pc - 5
  ]

testPC = [ Imm 0 1, USub 255 255 0 ]

-- Boot the CPU with a particular program
boot :: [Instruction] -> CPUState
boot prog = initialCPU { _memory = Map.fromList $ zip [0..] (map encode prog) }

loop :: CPUState -> IO ()
loop cpu = do
  print cpu
  print "\n-----------------------"
  getLine
  cpu' <- execStateT step cpu
  loop cpu'

main :: IO ()
main = loop (boot fibs)
