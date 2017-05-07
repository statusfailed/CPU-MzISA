{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
module CPU.Simulator where

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Vector (Vector(..))
import qualified Data.Vector as Vec

import Data.Word

import Control.Applicative
import Control.Monad.State
import Control.Lens
import Control.Lens.TH

import CPU.ISA

type RegisterBank  = Vector RegisterValue

-- 64-bit word-addressed memory
type Address = Word32
type Cell    = Word32
type Memory  = Map Address Cell

data CPUState = CPUState
  { _registers  :: RegisterBank
  , _memory     :: Memory
  } deriving (Eq, Read, Show)
makeLenses ''CPUState

initialCPU :: CPUState
initialCPU = CPUState
  { _registers = Vec.replicate 256 (0 :: RegisterValue)
  , _memory    = Map.empty
  }

-- | Lens into a particular register. We use 'Vec.unsafeIndex' because there are
-- exactly 255 registers, and 'RegisterName' is a 'Word8'.
reg :: RegisterName -> Lens' CPUState RegisterValue
reg n = lens
  (flip Vec.unsafeIndex i . _registers)
  (\s b -> s { _registers = Vec.update (_registers s) (Vec.singleton (i, b)) })
  where i = fromIntegral n

-- Access memory (can't be used to set, because ix is a traversal. TODO?)
mem :: Address -> Traversal CPUState CPUState Word32 Word32
mem addr = memory . ix addr

binop :: MonadState s f => (a -> b -> r) -> Getting a s a -> Getting b s b -> f r
binop f x y = liftA2 f (use x) (use y)

-- | Apply a single instruction to modify the CPU state
apply :: (MonadIO m, MonadState CPUState m) => Instruction -> m ()
apply (Imm dest val)      = reg dest .= fromIntegral val
apply (UAdd dest op1 op2) = reg dest <~ binop (+) (reg op1) (reg op2)
apply (USub dest op1 op2) = reg dest <~ binop (-) (reg op1) (reg op2)
apply (Store r_addr r_value) = do
  addr <- use (reg r_addr)
  liftIO . print $ "Store instruction for " ++ show r_addr ++ " :: "  ++ show addr
  case addr of
    -- 0x1000 is memory-mapped output peripheral. Writes a single 32-bit int to screen.
    0x1000 -> do
      liftIO $ print "======== OUTPUT ========="
      liftIO . print =<< use (reg r_value)
      liftIO $ print "========================="

    -- Otherwise, actually store a value in memory.
    addr -> memory . at addr <~ (fmap Just . use) (reg r_value)

-- | Step the CPU forward once, reading instruction from memory, decoding it,
-- and incrementing the program counter.
step :: (MonadIO m, MonadState CPUState m) => m Bool
step = do
  pc_value  <- use (reg 255) -- decode instruction at PC ptr
  raw_instr <- fmap (maybe 0 id) $ use (memory . at pc_value)
  case decode raw_instr of
    Left err -> error "INVALID INSTRUCTION"
    Right i  -> do
      liftIO $ do
        print $ "pc_value  " ++ show pc_value
        print $ "raw_instr " ++ show raw_instr
        print $ "i         " ++ show i

      apply i
      reg 255 += 1 -- PC += 1
      return True
