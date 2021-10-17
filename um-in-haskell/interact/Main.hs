{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Accum
import           Control.Monad.Trans.Cont
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import           Data.IORef
import qualified Data.List                  as List
import           Data.Vector.Unboxed        as U
import           Data.Word
import           Prelude                    hiding (getChar, putChar)
import           System.Environment
import           System.IO                  hiding (getChar, putChar)
import           UniversalMachine

newtype MyCont = MyCont (BS.ByteString -> ContT (Maybe MyCont) IO (Maybe MyCont))

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Hello, Haskell!"
            filename:_ -> do programBE <- BS.readFile filename
                             state0 <- initialState (programFromByteString programBE)
                             outputBuf <- newIORef []
                             cont <- evalContT $ callCC $ \(cont :: Maybe MyCont -> ContT (Maybe MyCont) IO BS.ByteString) -> do
                               let getStr = callCC $ \(cont' :: BS.ByteString -> ContT (Maybe MyCont) IO (Maybe MyCont)) -> do
                                     cont (Just (MyCont cont'))
                               let putChar c = modifyIORef' outputBuf (c:)
                               runSemiIO getStr putChar BS.empty state0
                               return Nothing
                             let collectOutput = do xs <- readIORef outputBuf
                                                    writeIORef outputBuf []
                                                    return (BS.pack (List.reverse xs))
                             contV <- newIORef cont
                             let interact input = do cont <- readIORef contV
                                                     case cont of
                                                       Nothing -> do putStrLn "VM halted"
                                                                     return BS.empty
                                                       Just (MyCont cont) -> do cont' <- evalContT $ cont input
                                                                                writeIORef contV cont'
                                                                                collectOutput
                             collectOutput >>= BSC.putStr
                             putStrLn "---"
                             output2 <- interact "guest\n"
                             BSC.putStr output2
                             putStrLn "---"
                             output3 <- interact "exit\n"
                             BSC.putStr output3
                             putStrLn "---"
