{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.TypeNats
import Control.Monad
import Numeric
import Data.Proxy

newtype Modular (n :: Nat) = Modular { getModular :: Word } deriving (Eq,Show)

instance KnownNat n => Num (Modular n) where
  Modular x + Modular y = Modular ((x + y) `rem` m)
    where m = fromIntegral (natVal (Proxy @n))
  Modular x - Modular y = Modular ((m + x - y) `rem` m)
    where m = fromIntegral (natVal (Proxy @n))
  Modular x * Modular y = Modular ((x * y) `rem` m)
    where m = fromIntegral (natVal (Proxy @n))
  negate (Modular x) = Modular ((m - x) `rem` m)
    where m = fromIntegral (natVal (Proxy @n))
  fromInteger n = Modular (fromInteger (n `mod` m))
    where m = fromIntegral (natVal (Proxy @n))
  abs = undefined; signum = undefined

data Instruction = MATH { dest :: !(Modular 2), src1 :: !(Modular 4), src2 :: !(Modular 4) }
                 | LOGIC { dest :: !(Modular 2), src1 :: !(Modular 4), src2 :: !(Modular 4) }
                 | SCIENCE { imm :: !Int }
                 | PHYSICS { imm :: !Int }
                 | BAIL
                 deriving (Eq,Show)

encodeInstruction :: Instruction -> Word8
encodeInstruction (MATH { dest, src1, src2 }) = 0x20 .|. (fromIntegral $ getModular dest `shiftL` 4) .|. (fromIntegral $ getModular src1 `shiftL` 2) .|. fromIntegral (getModular src2)
encodeInstruction (LOGIC { dest, src1, src2 }) = 0x40 .|. (fromIntegral $ getModular dest `shiftL` 4) .|. (fromIntegral $ getModular src1 `shiftL` 2) .|. fromIntegral (getModular src2)
encodeInstruction (SCIENCE { imm }) = 0x00 .|. fromIntegral ((fromIntegral imm :: Word) .&. 0x1f)
encodeInstruction (PHYSICS { imm }) = 0x60 .|. fromIntegral ((fromIntegral imm :: Word) .&. 0x1f)
encodeInstruction BAIL = 0x80

data State = State { memory :: U.Vector Word8 -- length: 256
                   , ip :: Int -- the instruction pointer (non-negative)
                   , is :: Int -- the instruction speed (-16 .. +15)
                   , sR :: U.Vector Word8 -- length: 4
                   , dR :: U.Vector Word8 -- length: 2
                   }
             deriving (Eq,Show)

nextIP :: V.Vector Instruction -> State -> Int
nextIP code (State { memory, ip, is, sR, dR }) = ((is + ip) `mod` 2^32) `mod` length code

data Result = Continue | Halt | Bail
{-
next :: Instruction -> State -> (Result,State)
next (MATH { dest, src1, src2 }) state@(State { memory, ip, is, sR, dR })
  = let memory' = memory U.// [(dR U.! getModular (dest + 1), (sR U.! getModular (src1 + 1)) - (sR U.! getModular (src2 + 1)))
                              ,(dR U.! getModular dest, (sR U.! getModular src1) + (sR U.! getModular src2))
                              ]
    in (Continue,State { memory = memory', ip = nextIP state, is = is, sR = sR, dR = dR })
next (LOGIC { dest, src1, src2 }) state@(State { memory, ip, is, sR, dR })
  = let memory' = memory U.// [(dR U.! getModular (dest + 1), (sR U.! getModular (src1 + 1)) `xor` (sR U.! getModular (src2 + 1)))
                              ,(dR U.! getModular dest, (sR U.! getModular src1) .&. (sR U.! getModular src2))
                              ]
    in (Continue,State { memory = memory', ip = nextIP state, is = is, sR = sR, dR = dR })
next (SCIENCE { imm }) state@(State { memory, ip, is, sR, dR })
  = let is' = if memory U.! (sR U.! 0) == 0 then
                is
              else
                imm
        state' = state { is = is' }
    in if is' == 0 then
         (Halt,state) -- HALT
       else
         (Continue,state' { ip = nextIP state' })
next (PHYSICS { imm }) state@(State { memory, ip, is, sR, dR })
  = let sR' = sr U.// [(0, (sR U.! 0) + fromIntegral imm)]
        l = U.fromList 
-}
main :: IO ()
main = do -- STOP
          let instrs = [PHYSICS 16
                       ,SCIENCE 0
                       ]
          -- STOP1
          let instrs = [PHYSICS 1
                       ,PHYSICS 1
                       ,SCIENCE 0
                       ]
          -- STOP127
          let instrs = []
          forM_ instrs $ \instr ->
            putStrLn $ showHex (encodeInstruction instr) ""
