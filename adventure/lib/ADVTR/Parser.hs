module ADVTR.Parser where
import           ADVTR.Types
import           Control.Applicative
import           Data.Functor
import qualified Data.Text.Short      as T
import qualified MultiSet
import           Text.Parsec          ((<?>))
import qualified Text.Parsec          as Parsec
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as PT

PT.TokenParser { PT.reserved = reserved, PT.stringLiteral = stringLiteral, PT.parens = parens } = PT.makeTokenParser haskellStyle

-- '(' <word> <p> ')'
parensWord :: String -> Parser a -> Parser a
parensWord word p = parens (reserved word >> p) <?> ("(" ++ word ++ ")")

kindP :: Parser Condition
kindP = parensWord "kind" $ do name <- parensWord "name" stringLiteral
                               conditionP (T.pack name)

kindsP :: Parser [Condition]
kindsP = (parens $ do k1 <- kindP
                      ks <- kindsP
                      pure (k1 : ks)
         ) <|> pure []

conditionP :: T.ShortText -> Parser Condition
conditionP itemName = parensWord "condition"
                      (parens ((reserved "pristine" $> Pristine itemName)
                                <|> do reserved "broken"
                                       innerCond <- conditionP itemName
                                       missing <- parensWord "missing" kindsP
                                       return $! Broken (MultiSet.fromList missing) innerCond
                              )
                       <?> "(pristine) or (broken)"
                      )

itemP :: Parser [Item]
itemP = parensWord "item" $ do name <- T.pack <$> parensWord "name" stringLiteral
                               _description <- parensWord "description" stringLiteral
                               adjectives <- parensWord "adjectives" $ ((Just . T.pack <$> (parens $ parensWord "adjective" stringLiteral)) <|> pure Nothing)
                               condition <- conditionP name
                               let item = Item { name = name
                                               , adjectives = adjectives
                                               , condition = condition
                                               }
                               (item :) <$> parensWord "piled_on" (parens itemP <|> pure [])

resultP :: Parser [Item]
resultP = parensWord "success"
          $ parensWord "command"
          $ parens
          $ do reserved "look" <|> reserved "go"
               parensWord "room" $ do _name <- parensWord "name" stringLiteral
                                      _description <- parensWord "description" stringLiteral
                                      parensWord "items" (parens itemP <|> pure [])
