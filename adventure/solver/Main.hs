-- Input: switch sexp
{-# LANGUAGE BangPatterns #-}
module Main where
import           ADVTR.Parser
import           ADVTR.Types
import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS.Lazy
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Control.Monad.Writer.Strict
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import qualified Data.Text.Short             as T
import qualified MultiSet                    as MultiSet
import           System.Environment
import           Text.Parsec.String          (parseFromFile)

takeOneOfList :: [a] -> [(a,[a])]
takeOneOfList xs = go [] xs
  where go rest []     = []
        go rest (x:xs) = (x,rest++xs) : go (x:rest) xs

data Recipe = RLeaf !Condition
            | RCombine {- result -} !Condition {- broken -} !Recipe {- parts -} [Recipe]
            deriving (Eq,Ord,Show)

recipeResult :: Recipe -> Condition
recipeResult (RLeaf item)        = item
recipeResult (RCombine item _ _) = item

recipeDependencies :: Recipe -> [Condition]
recipeDependencies recipe = go recipe []
  where
    go :: Recipe -> [Condition] -> [Condition]
    go (RLeaf item) items = item : items
    go (RCombine _ broken parts) items = List.foldl' (\items part -> go part items) (go broken items) parts

type BuildRecipe a = StateT (Map.Map T.ShortText (MultiSet.MultiSet Condition)) [] a

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

type ToCommand a = RWST {- limit -} Int [Command] ({- max inventory-} Int, {- inventory -} [Item], {- stack -} [Item], {- keep -} MultiSet.MultiSet Condition) [] a

takeItem :: Condition -> ToCommand Item
takeItem item = do
  (m, inventory, stack, keep) <- get
  case List.find (\i -> condition i == item) inventory of
    Just i -> pure i
    Nothing -> do
      let !len_inventory = length inventory
      limit <- ask
      guard (len_inventory < limit)
      let !new_max_inventory = max m (len_inventory + 1)
      case stack of
        [] -> empty
        x:xs | condition x == item -> do
                 tell [CTake x]
                 put (new_max_inventory, x : inventory, xs, MultiSet.deleteOne (condition x) keep)
                 pure x
             | condition x `MultiSet.member` keep -> (do tell [CTake x]
                                                         put (new_max_inventory, x : inventory, xs, MultiSet.deleteOne (condition x) keep)
                                                         takeItem item
                                                     ) <|> do -- Hack for RS232 adapter
                                                              guard (name x == T.pack "D-9887-UUE")
                                                              tell [CTake x,CIncinerate x]
                                                              put (new_max_inventory, inventory, xs, keep)
                                                              takeItem item
             | otherwise -> do
                 tell [CTake x,CIncinerate x]
                 put (new_max_inventory, inventory, xs, keep)
                 takeItem item

reduce :: Recipe -> ToCommand (Maybe Recipe)
reduce (RLeaf item) = do takeItem item
                         pure Nothing
reduce (RCombine result (RLeaf broken) parts)
  = do (part,restParts) <- lift $ takeOneOfList parts
       case part of
         RLeaf part' -> do (broken', part'') <- (do broken' <- takeItem broken
                                                    part'' <- takeItem part'
                                                    pure (broken', part'')
                                                ) <|> do part'' <- takeItem part'
                                                         broken' <- takeItem broken
                                                         pure (broken', part'')
                           tell [CCombine broken' part'']
                           (m, inventory, stack, keep) <- get
                           let combined = combineResult broken' part'
                           put (m, combined : List.filter (\x -> x /= broken' && x /= part'') inventory, stack, keep)
                           pure $ Just $ if null restParts then
                                           RLeaf (condition combined)
                                         else
                                           RCombine result (RLeaf (condition combined)) restParts
         RCombine {} -> do part' <- reduce part
                           case part' of
                             Nothing -> error "impossible"
                             Just part'' -> pure $ Just $ RCombine result (RLeaf broken) (part'':restParts)
  where
    combineResult :: Item -> Condition -> Item
    combineResult broken part = case condition broken of
                                  Pristine _ -> error "invalid"
                                  Broken missings cond' -> let missings' = MultiSet.deleteOne part missings
                                                           in if MultiSet.null missings then
                                                                broken { condition = cond' }
                                                              else
                                                                broken { condition = Broken missings' cond' }
reduce (RCombine result broken@(RCombine {}) parts)
  = (do Just broken' <- reduce broken
        pure $ Just $ RCombine result broken' parts
    ) <|> (do (part,restParts) <- lift $ takeOneOfList parts
              case part of
                RLeaf {} -> empty
                RCombine {} -> do part' <- reduce part
                                  case part' of
                                    Nothing -> error "impossible"
                                    Just part'' -> pure $ Just $ RCombine result broken (part'':restParts)
          )

solve :: Recipe -> ToCommand ()
solve recipe = go recipe
  where
    go recipe = do r <- reduce recipe
                   case r of
                     Nothing -> pure ()
                     Just r' -> go r'

reduceN :: Int -> Recipe -> ToCommand ()
reduceN n recipe = go n recipe
  where
    go 0 recipe = pure ()
    go n recipe = do r <- reduce recipe
                     case r of
                       Nothing -> pure ()
                       Just r' -> go (n - 1) r'

lengthAtMost :: Int -> [a] -> Maybe Int
lengthAtMost !limit xs = go 0 xs
  where
    go !acc [] = Just acc
    go !acc (_:xs) | acc >= limit = Nothing
                   | otherwise = go (acc + 1) xs

main :: IO ()
main = do args <- getArgs
          case args of
            [filename,target] -> do
              result <- parseFromFile resultP filename
              case result of
                Left err -> print err
                Right items -> do mapM_ print items
                                  putStrLn "---"
                                  let itemsByName :: Map.Map T.ShortText (MultiSet.MultiSet Condition)
                                      itemsByName = MultiSet.fromList <$> List.foldl' (\m item -> Map.insertWith (++) (name item) [condition item] m) Map.empty items
                                  let recipes :: [Recipe]
                                      recipes = evalStateT (buildRecipe (Pristine (T.pack target))) itemsByName
                                  putStrLn $ "# of recipes: " ++ case lengthAtMost 100 recipes of
                                                                   Just n -> show n
                                                                   Nothing -> "more than 100"
                                  let initial_limit = 2
                                      go !limit | limit > 6 = putStrLn "Suitable solution not found"
                                                | otherwise = do
                                                    let result :: [((Int,[Item],[Item],MultiSet.MultiSet Condition),[Command])]
                                                        result = do recipe <- recipes
                                                                    let keep = MultiSet.fromList $ recipeDependencies recipe
                                                                    execRWST (solve recipe) limit (0,[],items,keep)
                                                    case result of
                                                      [] -> do putStrLn $ "Could not be done with space=" ++ show limit
                                                               go (limit + 1)
                                                      ((m,_,_,_),commands):_ -> do
                                                        putStrLn $ "Required space: " ++ show m
                                                        putStrLn "---"
                                                        forM_ commands $ \command -> do
                                                          putStrLn (commandToString command)
                                  go initial_limit
            [] -> putStrLn "Usage: solver [filename] [target]"
