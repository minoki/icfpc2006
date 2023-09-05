{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.Maybe (Maybe(Nothing))
import Data.Foldable
import qualified Data.List as List

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

getModularAsInt :: Modular n -> Int
getModularAsInt (Modular x) = fromIntegral x

data Instruction = MATH { dest :: !(Modular 2), src1 :: !(Modular 4), src2 :: !(Modular 4) }
                 | LOGIC { dest :: !(Modular 2), src1 :: !(Modular 4), src2 :: !(Modular 4) }
                 | SCIENCE { imm :: !Int } -- imm: signed 5-bit number
                 | PHYSICS { imm :: !Int } -- imm: signed 5-bit number
                 | BAIL
                 deriving (Eq,Show)

encodeInstruction :: Instruction -> Word8
encodeInstruction (MATH { dest, src1, src2 }) = 0x20 .|. (fromIntegral $ getModular dest `shiftL` 4) .|. (fromIntegral $ getModular src1 `shiftL` 2) .|. fromIntegral (getModular src2)
encodeInstruction (LOGIC { dest, src1, src2 }) = 0x40 .|. (fromIntegral $ getModular dest `shiftL` 4) .|. (fromIntegral $ getModular src1 `shiftL` 2) .|. fromIntegral (getModular src2)
encodeInstruction (SCIENCE { imm }) = 0x00 .|. fromIntegral ((fromIntegral imm :: Word) .&. 0x1f)
encodeInstruction (PHYSICS { imm }) = 0x60 .|. fromIntegral ((fromIntegral imm :: Word) .&. 0x1f)
encodeInstruction BAIL = 0x80

data State = State { code :: !(V.Vector Instruction)
                   , memory :: !(U.Vector Word8) -- length: 256
                   , ip :: !Int -- the instruction pointer (non-negative)
                   , is :: !Int -- the instruction speed (-16 .. +15)
                   , sR :: !(U.Vector Word8) -- length: 4
                   , dR :: !(U.Vector Word8) -- length: 2
                   }
             deriving (Eq,Show)

nextIP :: State -> Int
nextIP (State { code, memory, ip, is, sR, dR }) = ((is + ip) `mod` 2^32) `mod` V.length code

data Result = Continue | Halt | Bail

next :: Instruction -> State -> (Result,State)
next (MATH { dest, src1, src2 }) state@(State { memory, ip, is, sR, dR })
  = let memory' = memory U.// [(fromIntegral $ dR U.! getModularAsInt (dest + 1), (sR U.! getModularAsInt (src1 + 1)) - (sR U.! getModularAsInt (src2 + 1)))
                              ,(fromIntegral $ dR U.! getModularAsInt dest, (sR U.! getModularAsInt src1) + (sR U.! getModularAsInt src2))
                              ]
    in (Continue, state { memory = memory', ip = nextIP state, is = is, sR = sR, dR = dR })
next (LOGIC { dest, src1, src2 }) state@(State { memory, ip, is, sR, dR })
  = let memory' = memory U.// [(fromIntegral $ dR U.! getModularAsInt (dest + 1), (sR U.! getModularAsInt (src1 + 1)) `xor` (sR U.! getModularAsInt (src2 + 1)))
                              ,(fromIntegral $ dR U.! getModularAsInt dest, (sR U.! getModularAsInt src1) .&. (sR U.! getModularAsInt src2))
                              ]
    in (Continue, state { memory = memory', ip = nextIP state, is = is, sR = sR, dR = dR })
next (SCIENCE { imm }) state@(State { memory, ip, is, sR, dR })
  = let is' = if memory U.! (fromIntegral $ sR U.! 0) == 0 then
                is
              else
                imm
        state' = state { is = is' }
    in if is' == 0 then
         (Halt, state) -- HALT
       else
         (Continue, state' { ip = nextIP state' })
next (PHYSICS { imm }) state@(State { memory, ip, is, sR, dR })
  = let sR' = sR U.// [(0, fromIntegral (fromIntegral (sR U.! 0) + imm))]
        -- 0: dR[1], 1: dR[0], 2: sR[3], 3: sR[2], 4: sR[1], 5: sR[0]
        l = [i | i <- [0,1,2,3,4], testBit imm i]
        cs = 5 : l
        cd = l ++ [5]
        r = U.reverse dR <> U.reverse sR'
        r' = r U.// [(dst, r U.! src) | (src, dst) <- zip cs cd]
        dR' = U.reverse (U.take 2 r')
        sR'' = U.reverse (U.drop 2 r')
    in (Continue, state { ip = nextIP state, sR = sR'', dR = dR' })
next BAIL state = (Bail, state)

run :: Int -> State -> Maybe State
run 0 !_ = Nothing
run limit state = case next (code state V.! ip state) state of
                    (Continue, state') -> run (limit - 1) state'
                    (Halt, state') -> Just state'
                    (Bail, _) -> Nothing

testCopyMem :: [Instruction] -> Bool
testCopyMem code_ = test 1 -- && test 7 && test 10 && test 77 && test 255
  where v = V.fromList code_
        test a = let state = State { code = v
                                   , memory = U.replicate 256 0 U.// [(0, a), (1, 1)]
                                   , ip = 0
                                   , is = 1
                                   , sR = U.replicate 4 0
                                   , dR = U.replicate 2 0
                                   }
                 in case run 1000 state of
                      Nothing -> False
                      Just state' -> U.any (== a) (sR state' <> dR state')

oneStepPhysics :: (U.Vector Word8, U.Vector Word8) -> Int -> (U.Vector Word8, U.Vector Word8)
oneStepPhysics (!sR, !dR) !imm
  = let sR' = sR U.// [(0, fromIntegral (fromIntegral (sR U.! 0) + imm))]
        -- 0: dR[1], 1: dR[0], 2: sR[3], 3: sR[2], 4: sR[1], 5: sR[0]
        l = [i | i <- [0,1,2,3,4], testBit imm i]
        cs = 5 : l
        cd = l ++ [5]
        r = U.reverse dR <> U.reverse sR'
        r' = r U.// [(dst, r U.! src) | (src, dst) <- zip cs cd]
        dR' = U.reverse (U.take 2 r')
        sR'' = U.reverse (U.drop 2 r')
    in (sR'', dR')

searchPhysics :: Int -> Int -> (U.Vector Word8 -> U.Vector Word8 -> Bool) -> U.Vector Word8 -> U.Vector Word8 -> Maybe [Int]
searchPhysics maxLen i f sR dR
  | maxLen <= i = Nothing
  | otherwise = let result = do s <- replicateM i [-16..15]
                                let (sR', dR') = foldl' oneStepPhysics (sR, dR) s
                                guard (f sR' dR')
                                pure s
                in case result of
                     [] -> searchPhysics maxLen (i + 1) f sR dR
                     r : _ -> Just r

searchPhysicsMulti :: Int -> Int -> (U.Vector Word8 -> U.Vector Word8 -> Bool) -> U.Vector Word8 -> U.Vector Word8 -> [[Int]]
searchPhysicsMulti maxLen i f sR dR
  | maxLen <= i = []
  | otherwise = let result = do s <- replicateM i [-16..15]
                                let (sR', dR') = foldl' oneStepPhysics (sR, dR) s
                                guard (f sR' dR')
                                pure s
                in result ++ searchPhysicsMulti maxLen (i + 1) f sR dR

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
          -- STOP128
          let instrs = [PHYSICS (-16)
                       ,SCIENCE 0
                       ]
          -- STOP127
          let instrs = [PHYSICS (-16)
                       -- sR[0] <- -16 === 240
                       -- sR[1], sR[0] <- 240, 0
                       ,PHYSICS (-16)
                       -- sR <- -16 === 240
                       -- sR[1], sR[0] <- 240, 240
                       ,PHYSICS (-16)
                       -- sR <- 240-16 = 224
                       -- sR[1], sR[0] <- 224, 240
                       ,PHYSICS (-16)
                       -- sR <- 240-16 = 224
                       -- sR[1], sR[0] <- 224, 224
                       ,PHYSICS (-16)
                       -- sR <- 224-16 = 208
                       -- sR[1], sR[0] <- 208, 224
                       ,PHYSICS (-16)
                       -- sR <- 224-16 = 208
                       -- sR[1], sR[0] <- 208, 208
                       ]

          -- swapreg
          -- print $ searchPhysics 5 0 (\sR dR -> dR == U.fromList [5, 4]) (U.fromList [0, 1, 2, 3]) (U.fromList [4, 5])
          let instrs = [PHYSICS (-16), PHYSICS 3, SCIENCE 0]
          -- forM_ instrs $ \instr ->
          --  putStrLn $ showHex (encodeInstruction instr) ""
          -- print $ searchPhysics 7 0 (\sR dR -> U.any (== 0) sR && U.any (== 1) sR && U.any (== 0) dR && U.any (/= 0) dR) (U.fromList [0, 0, 0, 0]) (U.fromList [0, 0])

          -- swapreg2
          let instrs = [PHYSICS 2, PHYSICS (-13), PHYSICS 1, PHYSICS 13, SCIENCE 0]
--          forM_ instrs $ \instr ->
--            putStrLn $ showHex (encodeInstruction instr) ""
{-
          print $ searchPhysicsMulti 5 0 (\sR dR -> dR == U.fromList [50, 40]) (U.fromList [0, 10, 20, 30]) (U.fromList [40, 50])
                  `List.intersect` searchPhysicsMulti 5 0 (\sR dR -> dR == U.fromList [50, 33]) (U.fromList [0, 10, 29, 13]) (U.fromList [33, 50])
                  `List.intersect` searchPhysicsMulti 5 0 (\sR dR -> dR == U.fromList [9, 4]) (U.fromList [0, 15, 20, 7]) (U.fromList [4, 9])
-}

          -- addmem
          -- print $ searchPhysics 5 0 (\sR dR -> dR U.! 0 == 2 && sR U.! 0 == 0 && sR U.! 2 == 1) (U.fromList [0, 1, 2, 3]) (U.fromList [4, 5])

          -- swapmem
          -- print $ searchPhysics 5 0 (\sR dR -> U.any (== 0) dR && U.any (== 0) sR && U.any (== 1) sR) (U.fromList [0, 1, 2, 3]) (U.fromList [4, 5])
          let (sR, dR) = foldl' oneStepPhysics (U.fromList [0, 1, 2, 3], U.fromList [4, 5]) [1,-13,-1,-4]
          print (sR, dR)
          print $ searchPhysics 5 0 (\sR dR -> U.any (== 1) dR && U.all (/= 0) dR && U.any (== 0) sR && U.any (== 1) sR) sR dR
          let (sR', dR') = foldl' oneStepPhysics (sR, dR) [-15,-2]
          print (sR', dR')
          print $ searchPhysics 6 0 (\sR dR -> U.any (== 0) dR && U.all (/= 1) dR &&U.any (== 0) sR && U.any (== 1) sR) sR' dR'
          let (sR'', dR'') = foldl' oneStepPhysics (sR', dR') [-16,-1]
          print (sR'', dR'')

{-
          -- copymem
          let srcSet = [Modular i | i <- [0..3]]
          let instrs = case [ c
                            | imm1 <- [-16..15]
                            , imm2 <- [-16..15]
                            , imm3 <- [-16..15]
                            , imm4 <- [-16..15]
                            , imm5 <- [-16..15]
                            , dest <- [Modular 0, Modular 1]
                            , src1 <- srcSet
                            , src2 <- srcSet
                            , src1 /= src2
                            , let c = [SCIENCE 0, PHYSICS imm1, PHYSICS imm2, PHYSICS imm3, PHYSICS imm4, PHYSICS imm5, MATH dest src1 src2]
                            , testCopyMem c
                            ] of
                         [] -> Nothing
                         c : _ -> Just c
          print instrs
          case instrs of
            Nothing -> pure ()
            Just instrs -> forM_ instrs $ \instr ->
                             putStrLn $ showHex (encodeInstruction instr) ""
-}
          pure ()
