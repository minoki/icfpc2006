{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
import           ADVTR.Parser
import           ADVTR.Types
import           System.Environment
import           Text.Parsec.String          (parseFromFile)
import qualified MultiSet                    as MultiSet
import qualified Data.Map.Strict             as Map
import qualified Data.Text.Short             as T
import           MultiSet                    (MultiSet)
import qualified Data.List                   as List
import           Control.Monad.State.Strict
import           Data.Maybe

data Recipe = RLeaf !Condition
            | RCombine {- result -} !Condition {- broken -} !Recipe {- parts -} [Recipe]
            deriving (Eq,Ord,Show)

type BuildRecipe a = StateT (Map.Map T.ShortText (MultiSet Condition)) [] a

recipeResult :: Recipe -> Condition
recipeResult (RLeaf item)        = item
recipeResult (RCombine item _ _) = item

recipeDependencies :: Recipe -> [Condition]
recipeDependencies recipe = go recipe []
  where
    go :: Recipe -> [Condition] -> [Condition]
    go (RLeaf item) items = item : items
    go (RCombine _ broken parts) items = List.foldl' (\items part -> go part items) (go broken items) parts

buildRecipe :: Condition -> BuildRecipe Recipe
buildRecipe target = do
  items <- gets (Map.findWithDefault MultiSet.empty (itemNameOfCondition target))
  (baseItem,items') <- lift $ MultiSet.takeOne items
  missings <- lift $ maybeToList $ match target baseItem
  modify (Map.insert (itemNameOfCondition target) items')
  go baseItem (RLeaf baseItem) missings
  where
    go !baseCond !baseItem [] = pure baseItem
    go !baseCond !baseItem (m:ms)
      | MultiSet.null m = pure baseItem
      | otherwise = do guard $ not (target `MultiSet.member` m)
                       let !combined = case baseCond of
                                         Pristine _ -> error "unexpected"
                                         Broken set cond' -> let set' = MultiSet.difference set m
                                                             in if MultiSet.null set' then
                                                                  cond'
                                                                else
                                                                  Broken set' cond'
                       parts <- mapM buildRecipe $ MultiSet.toList m
                       go combined (RCombine combined baseItem parts) ms

recipeToString :: Int -> Recipe -> String
recipeToString indent (RLeaf cond) = List.replicate (indent * 2) ' ' ++ "* " ++ conditionToString cond ++ "\n"
recipeToString indent (RCombine result broken parts) = List.replicate (indent * 2) ' ' ++ "* combine -> " ++ conditionToString result ++ "\n" ++ recipeToString (indent + 1) broken ++ concatMap (recipeToString (indent + 1)) parts

main :: IO ()
main = do args <- getArgs
          case args of
            [filename,target] -> do
              result <- parseFromFile resultP filename
              case result of
                Left err -> print err
                Right items -> do forM_ items $ \item -> do
                                    case item of
                                      Item { adjectives = Nothing, condition } -> putStrLn (conditionToString condition)
                                      Item { adjectives = Just adj, condition } -> putStrLn (T.unpack adj ++ " " ++ conditionToString condition)
                                  putStrLn "---"
                                  let itemsByName :: Map.Map T.ShortText (MultiSet.MultiSet Condition)
                                      itemsByName = MultiSet.fromList <$> List.foldl' (\m item -> Map.insertWith (++) (name item) [condition item] m) Map.empty items
                                  let recipes :: [Recipe]
                                      recipes = evalStateT (buildRecipe (Pristine (T.pack target))) itemsByName
                                  -- putStr (recipeToString 0 (head recipes))
                                  -- putStrLn $ "# of recipes: " ++ show (length recipes)
                                  forM_ (take 10 recipes) $ \recipe -> do
                                    putStrLn "---"
                                    putStr (recipeToString 0 recipe)
            [] -> putStrLn "Usage: adventure [filename] [target]"
