{-# LANGUAGE BangPatterns #-}
module UniversalMachine where
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.ByteString             as BS
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word

data State s = State { pc        :: !Int
                     , arrays    :: !(VM.MVector s (UM.MVector s Word32))
                     , registers :: !(UM.MVector s Word32) -- length = 8
                     , freelist  :: [Int]
                     }

programFromByteString :: BS.ByteString -> U.Vector Word32
programFromByteString programBE = case BS.length programBE `quotRem` 4 of
                                    (n,0) -> U.generate n $ \i -> (fromIntegral (BS.index programBE (4 * i)) `unsafeShiftL` 24)
                                                                  .|. (fromIntegral (BS.index programBE (4 * i + 1)) `unsafeShiftL` 16)
                                                                  .|. (fromIntegral (BS.index programBE (4 * i + 2)) `unsafeShiftL` 8)
                                                                  .|. fromIntegral (BS.index programBE (4 * i + 3))
                                    _ -> error "invalid input size"

initialState :: PrimMonad m => U.Vector Word32 -> m (State (PrimState m))
initialState !program = do
  array0 <- U.thaw program
  arrays <- VM.replicate 1 array0
  registers <- UM.replicate 8 0
  return (State { pc = 0, arrays = arrays, registers = registers, freelist = [] })

{-# INLINE run #-}
{-# SPECIALIZE run :: IO (Maybe Word8) -> (Word8 -> IO ()) -> State RealWorld -> IO (State RealWorld) #-}
run :: PrimMonad m => m (Maybe Word8) -> (Word8 -> m ()) -> State (PrimState m) -> m (State (PrimState m))
run getChar putChar !state@(State !pc !arrays !registers !freelist) = do
  !array0 <- VM.read arrays 0
  let readReg !a = UM.unsafeRead registers a
      writeReg !a !v = UM.unsafeWrite registers a v
  let go !pc = do
        !op <- UM.read array0 pc
        let a, b, c :: Int
            a = fromIntegral ((op `unsafeShiftR` 6) .&. 7)
            b = fromIntegral ((op `unsafeShiftR` 3) .&. 7)
            c = fromIntegral (op .&. 7)
        case op `unsafeShiftR` 28 of
          0 -> -- Conditional Move
            do v <- readReg c
               when (v /= 0) $ readReg b >>= writeReg a
               go (pc + 1)
          1 -> -- Array Index
            do i <- readReg b
               !arr_i <- VM.read arrays (fromIntegral i)
               j <- readReg c
               v <- UM.read arr_i (fromIntegral j)
               writeReg a v
               go (pc + 1)
          2 -> -- Array Amendment
            do i <- readReg a
               !arr_i <- VM.read arrays (fromIntegral i)
               j <- readReg b
               v <- readReg c
               UM.write arr_i (fromIntegral j) v
               go (pc + 1)
          3 -> -- Addition
            do x <- readReg b
               y <- readReg c
               writeReg a (x + y)
               go (pc + 1)
          4 -> -- Multiplication
            do x <- readReg b
               y <- readReg c
               writeReg a (x * y)
               go (pc + 1)
          5 -> -- Division
            do x <- readReg b
               y <- readReg c
               writeReg a (x `quot` y)
               go (pc + 1)
          6 -> -- Not-And
            do x <- readReg b
               y <- readReg c
               writeReg a (complement (x .&. y))
               go (pc + 1)
          7 -> -- Halt
            return state
          8 -> -- Allocation
            do capacity <- readReg c
               newArr <- UM.replicate (fromIntegral capacity) 0
               (!i,arrays',freelist') <- case freelist of
                                           [] -> do let !n = VM.length arrays
                                                    arrays' <- VM.grow arrays 1
                                                    VM.write arrays' n newArr
                                                    return (n, arrays', [])
                                           x:xs -> do VM.write arrays x newArr
                                                      return (x, arrays, xs)
               writeReg b (fromIntegral i)
               run getChar putChar (State (pc + 1) arrays' registers freelist')
          9 -> -- Abandonment
            do i <- readReg c
               VM.write arrays (fromIntegral i) undefined
               run getChar putChar (State (pc + 1) arrays registers (fromIntegral i : freelist))
          10 -> -- Output
            do v <- readReg c
               -- assert: v <= 255
               putChar (fromIntegral v)
               go (pc + 1)
          11 -> -- Input
            do v <- getChar
               writeReg c $! case v of
                               Nothing -> 0xFFFFFFFF
                               Just x  -> fromIntegral x
               go (pc + 1)
          12 -> -- Load Program
            do i <- readReg b
               pc' <- fromIntegral <$> readReg c
               if i /= 0 then do
                 arr_i <- VM.read arrays (fromIntegral i)
                 arr_0 <- UM.clone arr_i
                 VM.write arrays 0 arr_0
                 run getChar putChar (State pc' arrays registers freelist) -- replace 0 array
                 else
                 go pc'
          13 -> -- Orthography
            do let !a = fromIntegral ((op `unsafeShiftR` 25) .&. 7)
                   !value = op .&. 0x1FFFFFF
               writeReg a value
               go (pc + 1)
          _ -> error "Invalid Instruction"
  go pc
