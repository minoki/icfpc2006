-- Input: switch sexp
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as PT
import Text.Parsec.String (Parser, parseFromFile)
import Data.Functor
import Control.Applicative
import Control.Monad
import System.Environment
import qualified Data.Map.Strict as Map
import qualified MultiSet as MultiSet
import MultiSet (MultiSet)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Trans
import qualified Data.List as List
import qualified Data.Text.Short as T

PT.TokenParser { PT.reserved = reserved, PT.stringLiteral = stringLiteral, PT.parens = parens } = PT.makeTokenParser haskellStyle

data Condition = Pristine {- item name -} !T.ShortText
               | Broken !(MultiSet.MultiSet Condition) !Condition {- item name -} !T.ShortText {- depth -} !Int
               deriving (Eq, Ord, Show)

depth :: Condition -> Int
depth (Pristine _) = 0
depth (Broken _ _ _ depth) = depth

itemNameOfCondition :: Condition -> T.ShortText
itemNameOfCondition (Pristine name) = name
itemNameOfCondition (Broken _ _ name _) = name

data Item = Item { name :: !T.ShortText
                 , adjectives :: !(Maybe T.ShortText)
                 , condition :: !Condition
                 }
            deriving (Eq,Show)

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
                                       return $! Broken (MultiSet.fromList missing) innerCond itemName (depth innerCond + 1)
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

match :: Condition -- ^ target
      -> Condition
      -> Maybe [MultiSet Condition]
match (Pristine name) (Pristine name') | name == name' = Just []
                                       | otherwise = Nothing
match target@(Pristine name) (Broken conds cond name' _) | name == name' = (conds :) <$> match target cond
                                                         | otherwise = Nothing
match (Broken {}) (Pristine _) = Nothing
match target@(Broken conds cond name depth) (Broken conds' cond' name' depth')
  | name == name' = case compare depth depth' of
                      LT -> case match target cond' of
                              Just needed -> Just (conds' : needed)
                              Nothing -> Nothing
                      EQ -> case match cond cond' of
                              Nothing -> Nothing
                              Just needed -> if conds `MultiSet.subset` conds' then
                                               Just ((conds' `MultiSet.difference` conds) : needed)
                                             else
                                               Nothing
                      GT -> Nothing
  | otherwise = Nothing
                   {-
        case conds `MultiSet.subsetAndDifference` conds' of
                   Just diff -> case match cond cond' of
                                  Nothing -> Nothing
                                  Just needed -> Just (diff : needed)
                   Nothing -> Nothing
-}

search :: Eq a => (a -> Bool) -> [a] -> [(a,[a],[a])]
search f xs = go [] xs
  where go rest [] = []
        go rest (x:xs) | f x = (x,rest,xs) : go (x:rest) xs
                       | otherwise = go (x:rest) xs

searchEx :: Eq a => (a -> Maybe b) -> [a] -> [(a,b,[a],[a])]
searchEx f xs = go [] xs
  where go rest [] = []
        go rest (x:xs) | Just y <- f x = (x,y,rest,xs) : go (x:rest) xs
                       | otherwise = go (x:rest) xs

data Recipe = RLeaf !Item
            | RCombine {- result -} !Item {- broken -} !Recipe {- part -} !Recipe
            deriving (Eq,Show)

type BuildRecipe a = StateT (Map.Map T.ShortText [Recipe]) [] a

recipeToItem :: Recipe -> Item
recipeToItem (RLeaf item) = item
recipeToItem (RCombine item _ _) = item

buildRecipe :: Condition -> BuildRecipe Recipe
buildRecipe target = do
  items <- gets (Map.findWithDefault [] (itemNameOfCondition target))
  (baseItem,missings,items1,items2) <- lift $ searchEx (\y -> match target $ condition $ recipeToItem y) items
  modify (Map.insert (itemNameOfCondition target) (items1 ++ items2))
  go (condition (recipeToItem baseItem)) baseItem missings
  where
    go !baseCond !baseItem [] = pure baseItem
    go !baseCond !baseItem (m:ms)
      | MultiSet.null m = pure baseItem
      | otherwise = do cond <- lift $ MultiSet.toList m
                       part <- buildRecipe cond
                       let !newCondition = case baseCond of
                                             Pristine _ -> error "unexpected"
                                             Broken set cond' name' d' -> let set' = MultiSet.deleteOne cond set
                                                                          in if MultiSet.null set' then
                                                                               cond'
                                                                             else
                                                                               Broken set' cond' name' d'
                           !combined = (recipeToItem baseItem) { condition = newCondition }
                       go newCondition (RCombine combined baseItem part) ms

itemToString :: Item -> String
itemToString (Item { name, adjectives = Just adjective, condition }) = T.unpack adjective ++ " " ++ T.unpack name
itemToString (Item { name, adjectives = Nothing, condition }) = T.unpack name

merge :: [a] -> [a] -> [[a]]
merge [] ys = [ys]
merge xs [] = [xs]
merge xs0@(x:xs) ys0@(y:ys) = (x:) <$> merge xs ys0 <|> (y:) <$> merge xs0 ys

recipeToCombine :: Recipe -> [[(Item,Item,Item)]]
recipeToCombine (RLeaf item) = [[]]
recipeToCombine (RCombine result broken part) = do commands <- recipeToCombine broken
                                                   commands' <- recipeToCombine part
                                                   commands'' <- merge commands commands'
                                                   pure $ commands'' ++ [(recipeToItem broken,recipeToItem part,result)]

combineToString :: (Item,Item,Item) -> String
combineToString (broken,part,result) = "combine " ++ itemToString broken ++ " with " ++ itemToString part

data Command = CTake !Item
             | CIncinerate !Item
             | CCombine {- broken -} !Item {- part -} !Item
             deriving (Eq,Show)

type ToCommand a = StateT ({- max inventory-} Int, {- inventory -} [Item], {- stack -} [Item]) (WriterT [Command] []) a

neededLater :: Item -> [(Item,Item,Item)] -> Bool
neededLater item [] = False
neededLater item ((x,y,_):cs) = x == item || y == item || neededLater item cs

takeItem :: Int -> Item -> [(Item,Item,Item)] -> ToCommand ()
takeItem !limit item todo = do (m, inventory, stack) <- get
                               if item `elem` inventory then
                                 pure ()
                               else do
                                 let !len_inventory = length inventory
                                 guard (len_inventory < limit)
                                 let !new_max_inventory = max m (len_inventory + 1)
                                 case stack of
                                   [] -> empty
                                   x:xs | x == item -> do tell [CTake x]
                                                          put (new_max_inventory, x:inventory, xs)
                                        | neededLater x todo -> do tell [CTake x]
                                                                   put (new_max_inventory, x:inventory, xs)
                                                                   takeItem limit item todo
                                        | otherwise -> do tell [CTake x,CIncinerate x]
                                                          put (new_max_inventory, inventory, xs)
                                                          takeItem limit item todo

solve :: Int -> [(Item,Item,Item)] -> ToCommand ()
solve !limit [] = pure ()
solve !limit ((broken,part,result):xs)
  = (do takeItem limit broken ((part,part,result):xs)
        takeItem limit part xs
        tell [CCombine broken part]
        (m, inventory, stack) <- get
        put (m, result : filter (\x -> x /= broken && x /= part) inventory, stack)
        solve limit xs
    ) <|> (do takeItem limit part ((broken,broken,result):xs)
              takeItem limit broken xs
              tell [CCombine broken part]
              (m, inventory, stack) <- get
              put (m, result : filter (\x -> x /= broken && x /= part) inventory, stack)
              solve limit xs
          )

commandToString :: Command -> String
commandToString (CTake item) = "take " ++ itemToString item
commandToString (CIncinerate item) = "incinerate " ++ itemToString item
commandToString (CCombine broken part) = "combine " ++ itemToString broken ++ " with " ++ itemToString part

main :: IO ()
main = do args <- getArgs
          case args of
            [filename,target] -> do
              input <- readFile filename
              case Parsec.parse resultP "input" input of
                Left err -> print err
                Right items -> do mapM_ print items
                                  putStrLn "---"
                                  let itemsSortedByName :: Map.Map T.ShortText [Recipe]
                                      itemsSortedByName = Map.map (List.map RLeaf) $ List.foldl' (\m item -> Map.insertWith (++) (name item) [item] m) Map.empty items
                                  let recipes :: [Recipe]
                                      recipes = evalStateT (buildRecipe (Pristine (T.pack target))) itemsSortedByName
                                  putStrLn $ "# of recipes: " ++ show (length recipes)
                                  let combinations :: [[(Item,Item,Item)]]
                                      combinations = do recipe <- recipes
                                                        recipeToCombine recipe
                                  putStrLn $ "# of combinations: " ++ show (length combinations)
                                  let go !limit | limit > 6 = putStrLn "Suitable solution not found"
                                                | otherwise = do
                                                    let result :: [((Int,[Item],[Item]),[Command])]
                                                        result = do combination <- combinations
                                                                    runWriterT (execStateT (solve limit combination) (0,[],items))
                                                    if null result then
                                                      go (limit + 1)
                                                    else do
                                                      let min_inventory = minimum $ map (\((m,_,_),_) -> m) result
                                                      putStrLn $ "required space: " ++ show min_inventory
                                                      putStrLn "---"
                                                      let result0 = head $ filter (\((m,_,_),_) -> m == min_inventory) result
                                                      forM_ (snd result0) $ \command -> do
                                                        putStrLn (commandToString command)
                                  go 4
            [] -> putStrLn "Usage: adventure [filename] [target]"
