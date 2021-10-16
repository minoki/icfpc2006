module Main where
import           Data.Bits
import qualified Data.ByteString     as BS
import           Data.Vector.Unboxed as U
import           Data.Word
import           Prelude             hiding (getChar, putChar)
import           System.Environment
import           System.IO           hiding (getChar, putChar)
import           UniversalMachine

getChar :: IO (Maybe Word8)
getChar = do result <- BS.hGet stdin 1
             return $! if BS.null result then
                         Nothing
                       else
                         Just $! (BS.index result 0)

putChar :: Word8 -> IO ()
putChar c = BS.putStr (BS.singleton c)

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Hello, Haskell!"
            filename:_ -> do programBE <- BS.readFile filename
                             state0 <- initialState (programFromByteString programBE)
                             lastState <- run getChar putChar state0
                             return ()
