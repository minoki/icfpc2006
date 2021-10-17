{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Accum
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

{-
getChar :: IO (Maybe Word8)
getChar = do result <- BS.hGet stdin 1
             return $! if BS.null result then
                         Nothing
                       else
                         Just $! (BS.index result 0)

putChar :: Word8 -> IO ()
putChar c = BS.putStr (BS.singleton c)

type M a = StateT {- input supply -} [Word8] (AccumT {- output -} [Word8] (ContT {- message -} (Maybe [Word8]) IO)) a

getChar :: M (Maybe Word8)
getChar = do supply <- get
             case supply of
               x:xs -> do put xs
                          return (Just x)
               [] -> return Nothing
-}

-- newtype MyCont = MyCont (Maybe Word8 -> ContT (Either BS.ByteString (BS.ByteString, MyCont)) IO (Either BS.ByteString (BS.ByteString, MyCont)))
newtype MyCont = MyCont (Maybe Word8 -> AccumT BS.ByteString (ContT (Maybe MyCont, BS.ByteString) IO) (Maybe MyCont, BS.ByteString))

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Hello, Haskell!"
            filename:_ -> do programBE <- BS.readFile filename
                             state0 <- initialState (programFromByteString programBE)
                             -- let initialSupply = BS.unpack (BSC.pack "guest\nexit\n")
                             r <- evalContT $ do
                               liftIO $ putStrLn "start"
                               flip runAccumT BS.empty $ liftCallCC callCC $ \(cont :: Maybe MyCont -> AccumT BS.ByteString (ContT (Maybe MyCont, BS.ByteString) IO) (Maybe Word8)) -> do
                                 let getChar = liftCallCC callCC $ \(cont' :: Maybe Word8 -> AccumT BS.ByteString (ContT (Maybe MyCont, BS.ByteString) IO) (Maybe MyCont, BS.ByteString)) -> do
                                       cont (Just (MyCont cont'))
                                 let putChar c = add (BS.singleton c)
                                 run getChar putChar state0
                                 return Nothing
                             liftIO $ putStrLn "returned"
                             let go :: (Maybe MyCont, BS.ByteString) -> [Word8] -> IO ()
                                 go (Nothing, m) chars = do
                                   BSC.putStr m
                                   BSC.putStrLn "---"
                                   BSC.putStrLn ("Unconsumed input:" <> BS.pack chars)
                                   return ()
                                 go (Just (MyCont cont), m) [] = do
                                   BSC.putStr m
                                   BSC.putStrLn "---"
                                   BSC.putStrLn "Input exhausted"
                                   return ()
                                 go (Just (MyCont cont), m) (x:xs) = do
                                   BSC.putStrLn m
                                   r <- evalContT $ evalAccumT (cont (Just x)) BS.empty
                                   go r xs
                             go r (BS.unpack "guest\nexit\n")
                             return ()
