{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8   as BS
import           Data.Char
import           Data.Functor
import qualified Data.List               as List
import qualified Data.Map.Strict         as Map
import           System.IO
import           System.IO.Error
import           System.Process
import           System.Timeout          (timeout)
import           Text.Parsec             ((<?>))
import qualified Text.Parsec             as Parsec
import           Text.Parsec.Char        (anyChar, char, noneOf)
import           Text.Parsec.Combinator  (manyTill)
import           Text.Parsec.Language    (haskellStyle)
import           Text.Parsec.String      (Parser, parseFromFile)
import           Text.Parsec.Token       (TokenParser (..))
import qualified Text.Parsec.Token       as PT

PT.TokenParser { PT.reserved = reserved, PT.lexeme = lexeme, PT.parens = parens } = PT.makeTokenParser haskellStyle

stringLiteral :: Parser String
stringLiteral = lexeme $ do char '"'
                            manyTill stringChar (char '"') -- allow newline
                              where
                                stringChar = (do char '\\'
                                                 anyChar
                                             ) <|> noneOf "\\\""

data Condition = Pristine {- item name -} String
               | Broken [Condition] Condition
               deriving Show

data Item = Item { name        :: String
                 , description :: String
                 , adjectives  :: [String]
                 , condition   :: Condition
                 , piled_on    :: Maybe Item
                 }
            deriving Show

parensWord :: String -> Parser a -> Parser a
parensWord word p = parens (reserved word >> p) <?> ("(" ++ word ++ ")")

kindP :: Parser Condition
kindP = parensWord "kind" $ do name <- parensWord "name" stringLiteral
                               conditionP name

kindsP :: Parser [Condition]
kindsP = (parens $ do k1 <- kindP
                      ks <- kindsP
                      pure (k1 : ks)
         ) <|> pure []

conditionP :: String -> Parser Condition
conditionP itemName = parensWord "condition"
                      (parens ((reserved "pristine" $> Pristine itemName)
                                <|> do reserved "broken"
                                       innerCond <- conditionP itemName
                                       missing <- parensWord "missing" kindsP
                                       return $ Broken missing innerCond
                              )
                       <?> "(pristine) or (broken)"
                      )

itemP :: Parser Item
itemP = parensWord "item" $ do name <- parensWord "name" stringLiteral
                               description <- parensWord "description" (stringLiteral <|> (reserved "redacted" >> pure "___REDACTED___"))
                               adjectives <- parensWord "adjectives" $ ((parens $ many $ parensWord "adjective" stringLiteral) <|> pure [])
                               condition <- conditionP name
                               piled_on <- parensWord "piled_on" ((Just <$> parens itemP) <|> pure Nothing)
                               pure $ Item { name = name
                                           , description = description
                                           , adjectives = adjectives
                                           , condition = condition
                                           , piled_on = piled_on
                                           }

lookResult :: Parser String
lookResult = parensWord "success"
             $ parensWord "command"
             $ parens
             $ do reserved "look" <|> reserved "go"
                  parensWord "room" $ do name <- parensWord "name" stringLiteral
                                         description <- parensWord "description" stringLiteral
                                         parensWord "items" ((Just <$> parens itemP) <|> pure Nothing)
                                         pure name

roomToIndex :: Map.Map String Int
roomToIndex = Map.fromList $ List.zip ["52nd Street and Dorchester Avenue"
                                      ,"52nd Street and Blackstone Avenue"
                                      ,"52nd Street and Harper Avenue"
                                      ,"53th Street and Dorchester Avenue"
                                      ,"53th Street and Blackstone Avenue"
                                      ,"53th Street and Harper Avenue"
                                      ,"54th Street and Dorchester Avenue"
                                      ,"54th Street and Blackstone Avenue"
                                      ,"54th Street and Harper Avenue"
                                      ,"54th Place and Dorchester Avenue"
                                      ,"54th Place and Blackstone Avenue"
                                      ,"54th Place and Harper Avenue"
                                      ] [0..]

gather :: Handle -> IO String
gather h = go ""
  where
    go acc = do mvar <- newEmptyMVar
                tid <- forkIO $ do result <- tryJust (guard . isEOFError) $ hGetLine h
                                   case result of
                                     Left err   -> putMVar mvar ""
                                     Right line -> putMVar mvar (line <> "\n")
                result <- timeout (1200 * 1000) (takeMVar mvar)
                killThread tid
                case result of
                  Nothing   -> return acc
                  Just ""   -> return acc
                  Just some -> go (acc <> some)


main = do
  (Just stdin, Just stdout, stderr, processHandle) <- createProcess (shell "../umix.sh") { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hPutStr stdin "howie\nxyzzy\n./adventure\n"
  keypad_commands <- readFile "keypad-commands.txt"
  hPutStr stdin keypad_commands
  uploader_commands <- readFile "uploader-commands.txt"
  hPutStr stdin uploader_commands
  hPutStr stdin "use uploader\n"
  rml <- readFile "gc2.rml"
  hPutStr stdin rml
  hPutStr stdin "speak Rotunda\ntake blueprint\nswitch sexp\n"
  hFlush stdin
  _content <- gather stdout
  putStrLn "---------"

  rml1 <- readFile "gc2-head.rml"
  rml2 <- readFile "gc2-tail.rml"
  let getValue info code = do
        let rml = rml1 <> "      let value = " <> code <> ".\n" <> rml2
        hPutStr stdin $ "use uploader\n" <> rml

        hPutStr stdin "break blueprint\n"
        hFlush stdin
        _content <- gather stdout

        hPutStr stdin "examine\n"
        hFlush stdin
        content <- gather stdout
        case Parsec.parse lookResult "output" content of
          Left err -> putStrLn info >> putStrLn content >> print err >> return Nothing
          Right result -> case Map.lookup result roomToIndex of
                            Just n  -> return (Just n)
                            Nothing -> putStrLn info >> putStrLn result >> return Nothing
  {-
  Just lenHi <- getValue "div (string_length (desc), 12)"
  Just lenLo <- getValue "mod (string_length (desc), 12)"
  let len = lenHi * 12 + lenLo
  putStrLn $ "length=" <> show len
  -}
  let len = 123
  let startIndex = 60

  forM_ [startIndex..len-1] $ \i -> do
    Just valHi <- getValue ("i=" <> show i) $ "div (string_charat (desc, " <> show i <> "), 12)"
    Just valLo <- getValue ("i=" <>show i) $ "mod (string_charat (desc, " <> show i <> "), 12)"
    let val = valHi * 12 + valLo
    putStr [chr val]
    hFlush System.IO.stdout
  putStrLn ""

  hPutStr stdin "exit\nexit\n"
  hFlush stdin
  content <- gather stdout
  putStr content
  exitCode <- waitForProcess processHandle
  print exitCode
