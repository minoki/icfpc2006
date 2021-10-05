module MultiSet where
import qualified Data.Map.Strict as Map

newtype MultiSet a = MultiSet (Map.Map a Int) deriving (Eq, Ord, Show)

null :: MultiSet a -> Bool
null (MultiSet m) = Map.null m

fromList :: Ord a => [a] -> MultiSet a
fromList xs = MultiSet $ Map.fromListWith (+) $ map (\x -> (x,1)) xs

-- if a `subset` b then Just (b `difference` a) else Nothing
subsetAndDifference :: Ord a => MultiSet a -> MultiSet a -> Maybe (MultiSet a)
subsetAndDifference (MultiSet a) (MultiSet b) = go [] (Map.toAscList a) (Map.toAscList b)
  where go acc [] ys = Just $ MultiSet $ Map.fromList (acc ++ ys)
        go acc (_:_) [] = Nothing
        go acc xs0@((x,n):xs) ((y,m):ys) = case compare x y of
                                             EQ -> if n <= m then
                                                     let k = m - n
                                                     in if k == 0 then
                                                          go acc xs ys
                                                        else
                                                          go ((x,k):acc) xs ys
                                                   else
                                                     Nothing
                                             LT -> Nothing
                                             GT -> go ((y,m):acc) xs0 ys

subset :: Ord a => MultiSet a -> MultiSet a -> Bool
subset (MultiSet a) (MultiSet b) = go (Map.toAscList a) (Map.toAscList b)
  where go [] _ = True
        go (_:_) [] = False
        go xs0@((x,n):xs) ((y,m):ys) = case compare x y of
                                         EQ -> n <= m && go xs ys
                                         LT -> False
                                         GT -> go xs0 ys

toList :: MultiSet a -> [a]
toList (MultiSet m) = go (Map.toAscList m)
  where go [] = []
        go ((x,n):xs) = go1 x n xs
        go1 _ 0 xs = go xs
        go1 x n xs = x : go1 x (n - 1) xs

size :: MultiSet a -> Int
size (MultiSet m) = Map.foldr (+) 0 m

deleteOne :: Ord a => a -> MultiSet a -> MultiSet a
deleteOne x (MultiSet m) = case Map.splitLookup x m of
                             (m1, Nothing, m2) -> MultiSet m
                             (m1, Just n, m2) | n <= 1 -> MultiSet (Map.union m1 m2)
                                              | otherwise -> MultiSet (Map.insert x (n-1) $ Map.union m1 m2)
