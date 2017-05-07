module CPU.ISA.Encoding where

import Data.Word
import Data.Bits
import CPU.ISA.Instructions

-- TODO: remove debug!
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

binary n = showIntAtBase 2 intToDigit n ""

bytesToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32 a b c d = shiftL a' 24 .|. shiftL b' 16 .|. shiftL c' 8 .|. d'
  where [a', b', c', d'] = map fromIntegral [a, b, c, d]

byteAt :: Int -> Word32 -> Word8
byteAt n w = fromIntegral $ shiftR w n .&. 0xFF

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes w =
  ( byteAt 24 w
  , byteAt 16 w
  , byteAt 8  w
  , byteAt 0  w
  )
  where
    -- mask of byte starting at bit n, and shift back to first byte

reserved :: Word8
reserved = 0

encode :: Instruction -> Word32
encode (Imm  dest value  ) = bytesToWord32 0 dest 0 0 .|. fromIntegral value
encode (UAdd dest op1 op2) = bytesToWord32 1 dest op1 op2
encode (USub dest op1 op2) = bytesToWord32 2 dest op1 op2
encode (Store addr value)  = bytesToWord32 3 addr value reserved
encode (Load  dest addr )  = bytesToWord32 4 dest addr  reserved

decode :: Word32 -> Either String Instruction
decode word = case opcode of
    0 -> Right $ Imm b1 $ shiftL (fromIntegral b2) 8 .|. fromIntegral b3
    1 -> Right $ UAdd b1 b2 b3
    2 -> Right $ USub b1 b2 b3
    3 -> Right $ Store b1 b2
    4 -> Right $ Load  b1 b2
    n -> Left $ "No such opcode: " ++ show n
  where (opcode, b1, b2, b3) = word32ToBytes word
