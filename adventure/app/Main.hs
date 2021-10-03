-- Input: switch sexp
module Main where
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as PT
import Text.Parsec.Token (TokenParser(..))
import Text.Parsec.String (Parser, parseFromFile)
import Data.Functor
import Control.Applicative

PT.TokenParser { PT.reserved = reserved, PT.stringLiteral = stringLiteral, PT.parens = parens } = PT.makeTokenParser haskellStyle

data Condition = Pristine {- item name -} String
               | Broken [Condition] Condition
               deriving Show

data Item = Item { name :: String
                 , description :: String
                 , adjectives :: [String]
                 , condition :: Condition
                 , piled_on :: Maybe Item
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
                               description <- parensWord "description" stringLiteral
                               adjectives <- parensWord "adjectives" $ ((parens $ many $ parensWord "adjective" stringLiteral) <|> pure [])
                               condition <- conditionP name
                               piled_on <- parensWord "piled_on" ((Just <$> parens itemP) <|> pure Nothing)
                               pure $ Item { name = name
                                           , description = description
                                           , adjectives = adjectives
                                           , condition = condition
                                           , piled_on = piled_on
                                           }

lookResult :: Parser (Maybe Item)
lookResult = parensWord "success"
             $ parensWord "command"
             $ parens
             $ do reserved "look" <|> reserved "go"
                  parensWord "room" $ do name <- parensWord "name" stringLiteral
                                         description <- parensWord "description" stringLiteral
                                         parensWord "items" ((Just <$> parens itemP) <|> pure Nothing)

main :: IO ()
main = do input <- getContents
          case Parsec.parse lookResult "input" input of
            Left err -> print err
            Right item -> print item
