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
import           MultiSet                    (MultiSet)
import qualified MultiSet                    as MultiSet
import           System.Environment
import           Text.Parsec.String          (parseFromFile)

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

takeOneOfList :: [a] -> [(a,[a])]
takeOneOfList xs = go [] xs
  where go rest []     = []
        go rest (x:xs) = (x,rest++xs) : go (x:rest) xs

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
      | otherwise = do let !combined = case baseCond of
                                         Pristine _ -> error "unexpected"
                                         Broken set cond' -> let set' = MultiSet.difference set m
                                                             in if MultiSet.null set' then
                                                                  cond'
                                                                else
                                                                  Broken set' cond'
                       parts <- mapM buildRecipe $ MultiSet.toList m
                       go combined (RCombine combined baseItem parts) ms

merge :: [a] -> [a] -> [[a]]
merge [] ys                 = [ys]
merge xs []                 = [xs]
merge xs0@(x:xs) ys0@(y:ys) = (x:) <$> merge xs ys0 <|> (y:) <$> merge xs0 ys

-- type ToCommand a = StateT ({- max inventory-} Int, {- inventory -} [Item], {- stack -} [Item], {- keep -} MultiSet.MultiSet Condition) (WriterT [Command] []) a
type ToCommand a = RWST Int [Command] ({- max inventory-} Int, {- inventory -} [Item], {- stack -} [Item], {- keep -} MultiSet.MultiSet Condition) [] a

takeItem :: Int -> Condition -> ToCommand Item
takeItem !limit item = do
  (m, inventory, stack, keep) <- get
  case List.find (\i -> condition i == item) inventory of
    Just i -> pure i
    Nothing -> do
      let !len_inventory = length inventory
      guard (len_inventory < limit)
      let !new_max_inventory = max m (len_inventory + 1)
      case stack of
        [] -> empty
        x:xs | condition x == item -> do
                 tell [CTake x]
                 put (new_max_inventory, x : inventory, xs, MultiSet.deleteOne (condition x) keep)
                 pure x
             | condition x `MultiSet.member` keep -> do
                 tell [CTake x]
                 put (new_max_inventory, x : inventory, xs, MultiSet.deleteOne (condition x) keep)
                 takeItem limit item
             | otherwise -> do
                 tell [CTake x,CIncinerate x]
                 put (new_max_inventory, inventory, xs, keep)
                 takeItem limit item

reduce :: Int -> Recipe -> ToCommand (Maybe Recipe)
reduce !limit (RLeaf item) = do takeItem limit item
                                pure Nothing
reduce !limit (RCombine result (RLeaf broken) parts)
  = do (part,restParts) <- lift $ takeOneOfList parts
       case part of
         RLeaf part' -> do (broken', part'') <- (do broken' <- takeItem limit broken
                                                    part'' <- takeItem limit part'
                                                    pure (broken', part'')
                                                ) <|> do part'' <- takeItem limit part'
                                                         broken' <- takeItem limit broken
                                                         pure (broken', part'')
                           tell [CCombine broken' part'']
                           (m, inventory, stack, keep) <- get
                           let combined = combineResult broken' part'
                           put (m, combined : List.filter (\x -> x /= broken' && x /= part'') inventory, stack, keep)
                           pure $ Just $ if null restParts then
                                           RLeaf (condition combined)
                                         else
                                           RCombine result (RLeaf (condition combined)) restParts
         RCombine {} -> do part' <- reduce limit part
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
reduce !limit (RCombine result broken@(RCombine {}) parts)
  = (do Just broken' <- reduce limit broken
        pure $ Just $ RCombine result broken' parts
    ) <|> (do (part,restParts) <- lift $ takeOneOfList parts
              case part of
                RLeaf {} -> empty
                RCombine {} -> do part' <- reduce limit part
                                  case part' of
                                    Nothing -> error "impossible"
                                    Just part'' -> pure $ Just $ RCombine result broken (part'':restParts)
          )

solve :: Int -> Recipe -> ToCommand ()
solve !limit recipe = go recipe
  where
    go recipe = do r <- reduce limit recipe
                   case r of
                     Nothing -> pure ()
                     Just r' -> go r'

reduceN :: Int -> Int -> Recipe -> ToCommand ()
reduceN n !limit recipe = go n recipe
  where
    go 0 recipe = pure ()
    go n recipe = do r <- reduce limit recipe
                     case r of
                       Nothing -> pure ()
                       Just r' -> go (n - 1) r'

solveGreedy :: Int -> Item -> Recipe -> ToCommand Recipe
solveGreedy !limit item (RLeaf item') = do
  if condition item == item' then
    pure (RLeaf item')
    else
    empty
solveGreedy !limit item (RCombine result (RLeaf broken) parts) = do
  (m, inventory, stack, keep) <- get
  if condition item == broken then
    case [ (part,restInv,restParts) | (part,restInv) <- takeOneOfList inventory, (RLeaf part',restParts) <- takeOneOfList parts, condition part == part' ] of
      [] -> empty
      (part,restInv,restParts):_ -> do let combined = combineResult item (condition part)
                                       tell [CCombine item part]
                                       put (m, restInv, stack, keep)
                                       pure $ if null restParts then
                                                RLeaf (condition combined)
                                              else
                                                RCombine result (RLeaf (condition combined)) restParts
    else do
    case List.find (\(i,_) -> condition i == broken) $ takeOneOfList inventory of
      Nothing -> empty
      Just (broken',restInv) -> case List.find (\(part,rest) -> part == RLeaf (condition item)) $ takeOneOfList parts of
                                  Nothing -> empty
                                  Just (_,restParts) -> do let combined = combineResult broken' (condition item)
                                                           tell [CCombine broken' item]
                                                           put (m, restInv, stack, keep)
                                                           pure $ if null restParts then
                                                                    RLeaf (condition combined)
                                                                  else
                                                                    RCombine result (RLeaf (condition combined)) restParts
  where
    combineResult :: Item -> Condition -> Item
    combineResult broken part = case condition broken of
                                  Pristine _ -> error "invalid"
                                  Broken missings cond' -> let missings' = MultiSet.deleteOne part missings
                                                           in if MultiSet.null missings then
                                                                broken { condition = cond' }
                                                              else
                                                                broken { condition = Broken missings' cond' }
solveGreedy !limit item (RCombine result broken parts) = (do r <- solveGreedy limit item broken
                                                             pure $ RCombine result r parts
                                                         )
                                                         <|>
                                                         (do (part,restParts) <- lift $ takeOneOfList parts
                                                             part' <- solveGreedy limit item part
                                                             pure $ RCombine result broken (part':restParts)
                                                         )
greedy :: Int -> Recipe -> ToCommand ()
greedy !limit (RLeaf item) = do takeItem limit item
                                pure ()
greedy !limit recipe = do
  (m, inventory, stack, keep) <- get
  let !len_inventory = length inventory
  guard (len_inventory < limit)
  let !new_max_inventory = max m (len_inventory + 1)
  case stack of
    [] -> empty
    x:xs | condition x `MultiSet.member` keep -> do
             put (new_max_inventory, inventory, xs, MultiSet.deleteOne (condition x) keep)
             newRecipe <- solveGreedy limit x recipe
             greedy limit newRecipe
         | otherwise -> do
             tell [CTake x,CIncinerate x]
             put (new_max_inventory, inventory, xs, keep)
             greedy limit recipe

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
                                  putStrLn $ "# of recipes: " ++ show (length recipes)
                                  let initial_limit = 2
                                      go !limit | limit > 6 = putStrLn "Suitable solution not found"
                                                | otherwise = do
                                                    {-
                                                    let n = length $ do recipe <- recipes
                                                                        let keep = MultiSet.fromList $ recipeDependencies recipe
                                                                        runWriterT (execStateT (reduceN 10 limit recipe) (0,[],items,keep))
                                                    putStrLn $ "n=" ++ show n
-}
                                                    let result :: [((Int,[Item],[Item],MultiSet.MultiSet Condition),[Command])]
                                                        result = do recipe <- recipes
                                                                    let keep = MultiSet.fromList $ recipeDependencies recipe
                                                                    execRWST (solve limit recipe) 0 (0,[],items,keep)
                                                                    -- runWriterT (execStateT (solve limit recipe) (0,[],items,keep))
                                                                    -- runWriterT (execStateT (greedy limit recipe) (0,[],items,keep))
                                                    if null result then do
                                                      putStrLn $ "Could not be done with space=" ++ show limit
                                                      go (limit + 1)
                                                    else do
                                                      let min_inventory = if limit == initial_limit then
                                                                            minimum $ map (\((m,_,_,_),_) -> m) result
                                                                          else
                                                                            limit
                                                      putStrLn $ "Required space: " ++ show min_inventory
                                                      putStrLn "---"
                                                      let result0 = head $ filter (\((m,_,_,_),_) -> m == min_inventory) result
                                                      forM_ (snd result0) $ \command -> do
                                                        putStrLn (commandToString command)
                                  go initial_limit
            [] -> putStrLn "Usage: adventure [filename] [target]"
