{-# LANGUAGE NamedFieldPuns #-}
module ADVTR.Types where
import qualified Data.List       as List
import qualified Data.Text.Short as T
import           MultiSet

data Condition = Pristine {- item name -} !T.ShortText
               | Broken !(MultiSet.MultiSet Condition) !Condition
               deriving (Eq, Ord, Show)

depth :: Condition -> Int
depth (Pristine _)        = 0
depth (Broken _ baseItem) = 1 + depth baseItem

itemNameOfCondition :: Condition -> T.ShortText
itemNameOfCondition (Pristine name)     = name
itemNameOfCondition (Broken _ baseItem) = itemNameOfCondition baseItem

conditionToString :: Condition -> String
conditionToString (Pristine name) = T.unpack name
conditionToString (Broken missings base) = conditionToString base ++ " missing {" ++ List.concat (List.intersperse ", " $ map (\(cond,n) -> show n ++ " " ++ conditionToString cond) (MultiSet.toMultiplicityList missings)) ++ "}"

data Item = Item { name       :: !T.ShortText
                 , adjectives :: !(Maybe T.ShortText)
                 , condition  :: !Condition
                 }
            deriving (Eq,Show)

match :: Condition -- ^ target
      -> Condition
      -> Maybe [MultiSet Condition]
match (Pristine name) (Pristine name') | name == name' = Just []
                                       | otherwise = Nothing
match target@(Pristine name) (Broken conds cond) = (conds :) <$> match target cond
match (Broken {}) (Pristine _) = Nothing
match target@(Broken conds cond) item'@(Broken conds' cond')
  = case compare (depth target) (depth item') of
      LT -> case match target cond' of
              Just needed -> Just (conds' : needed)
              Nothing     -> Nothing
      EQ -> case match cond cond' of
              Nothing -> Nothing
              Just needed -> if conds `MultiSet.subset` conds' then
                               Just ((conds' `MultiSet.difference` conds) : needed)
                             else
                               Nothing
      GT -> Nothing

itemToString :: Item -> String
itemToString (Item { name, adjectives = Just adjective, condition }) = T.unpack adjective ++ " " ++ T.unpack name
itemToString (Item { name, adjectives = Nothing, condition }) = T.unpack name

data Command = CTake !Item
             | CIncinerate !Item
             | CCombine {- broken -} !Item {- part -} !Item
             deriving (Eq,Show)

commandToString :: Command -> String
commandToString (CTake item) = "take " ++ itemToString item
commandToString (CIncinerate item) = "incinerate " ++ itemToString item
commandToString (CCombine broken part) = "combine " ++ itemToString broken ++ " with " ++ itemToString part
