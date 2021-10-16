module Main where
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Cont
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import           Data.Vector.Unboxed        as U
import           Data.Word
import           Prelude                    hiding (getChar, putChar)
import           System.Environment
import           System.IO                  hiding (getChar, putChar)
import           UniversalMachine
import Control.Monad.Trans.Accum

{-
getChar :: IO (Maybe Word8)
getChar = do result <- BS.hGet stdin 1
             return $! if BS.null result then
                         Nothing
                       else
                         Just $! (BS.index result 0)

putChar :: Word8 -> IO ()
putChar c = BS.putStr (BS.singleton c)

type M a = StateT {- input supply -} [Word8] (AccumT {- output -} [Word8] ContT {- message -} (Maybe [Word8]) IO) a

getChar :: M (Maybe Word8)
getChar = do supply <- get
             case supply of
               x:xs -> do put xs
                          return (Just x)
               [] -> return Nothing
-}

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Hello, Haskell!"
            filename:_ -> do programBE <- BS.readFile filename
                             state0 <- initialState (programFromByteString programBE)
                             let initialSupply = BS.unpack (BSC.pack "guest\nexit\n")
                             message <- evalContT $ callCC $ \cont -> do let getChar = do supply <- get
                                                                                          case supply of
                                                                                            x:xs -> do put xs
                                                                                                       return (Just x)
                                                                                            [] -> do written <- lift look
                                                                                                     lift $ lift $ cont (Just written)
                                                                         let putChar c = lift $ add [c]
                                                                         message <- execAccumT (evalStateT (run getChar putChar state0) initialSupply) []
                                                                         return (Just message)
                             case message of
                               Nothing -> putStrLn "Halted."
                               Just m -> BS.putStr (BS.pack m)
                             return ()
