{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Data.Int
import Data.Word
import Data.Bits
import GHC.TypeNats
import Control.Monad
import Numeric
import Data.Proxy
import Data.Maybe (Maybe(Nothing))
import Text.Read (readMaybe)
import System.Environment

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

instance KnownNat n => Read (Modular n) where
  readsPrec p s = [(fromInteger i,r) | (i,r) <- readsPrec p s]

getModularAsInt :: Modular n -> Int
getModularAsInt (Modular x) = fromIntegral x

data Instruction = MATH !(Modular 2) !(Modular 4) !(Modular 4)
                 | LOGIC !(Modular 2) !(Modular 4) !(Modular 4)
                 | SCIENCE !Int -- imm: signed 5-bit number
                 | PHYSICS !Int -- imm: signed 5-bit number
                 | BAIL
                 deriving (Eq,Show,Read)

encodeInstruction :: Instruction -> Word8
encodeInstruction (MATH dest src1 src2) = 0x20 .|. (fromIntegral $ getModular dest `shiftL` 4) .|. (fromIntegral $ getModular src1 `shiftL` 2) .|. fromIntegral (getModular src2)
encodeInstruction (LOGIC dest src1 src2) = 0x40 .|. (fromIntegral $ getModular dest `shiftL` 4) .|. (fromIntegral $ getModular src1 `shiftL` 2) .|. fromIntegral (getModular src2)
encodeInstruction (SCIENCE imm) = 0x00 .|. fromIntegral ((fromIntegral imm :: Word) .&. 0x1f)
encodeInstruction (PHYSICS imm) = 0x60 .|. fromIntegral ((fromIntegral imm :: Word) .&. 0x1f)
encodeInstruction BAIL = 0x80

main :: IO ()
main = do
  a <- getArgs
  ll <- lines <$> getContents
  case a of
    name : _ -> putStrLn $ "/bin/umodem " ++ name ++ ".bal STOP"
    [] -> pure ()
  forM_ ll $ \l -> do
    case readMaybe l :: Maybe Instruction of
      Just i -> let e = encodeInstruction i
                in putStrLn $ (if e < 0x10 then "0" else "") ++ showHex e ""
      Nothing -> pure ()
  case a of
    name : _ -> putStrLn $ "STOP\ncertify " ++ name ++ " " ++ name ++ ".bal"
    [] -> pure ()
