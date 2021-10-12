module MultiSet where
import qualified Data.Map.Strict as Map

newtype MultiSet a = MultiSet (Map.Map a Int) deriving (Eq, Ord, Show)

empty :: MultiSet a
empty = MultiSet Map.empty

null :: MultiSet a -> Bool
null (MultiSet m) = Map.null m

member :: Ord a => a -> MultiSet a -> Bool
member x (MultiSet m) = Map.member x m

fromList :: Ord a => [a] -> MultiSet a
fromList xs = MultiSet $ Map.fromListWith (+) $ map (\x -> (x,1)) xs

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet m) = MultiSet $ Map.insertWith (+) x 1 m

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
subset (MultiSet a) (MultiSet b) = Map.isSubmapOfBy (<=) a b

difference :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet a) (MultiSet b) = MultiSet (Map.differenceWith (\x y -> if x > y then Just $! (x - y) else Nothing) a b)

toList :: MultiSet a -> [a]
toList (MultiSet m) = go (Map.toAscList m)
  where go [] = []
        go ((x,n):xs) = go1 x n xs
        go1 _ 0 xs = go xs
        go1 x n xs = x : go1 x (n - 1) xs

size :: MultiSet a -> Int
size (MultiSet m) = Map.foldr (+) 0 m

deleteOne :: Ord a => a -> MultiSet a -> MultiSet a
deleteOne x (MultiSet m) = MultiSet (Map.update (\n -> if n > 1 then Just $! (n - 1) else Nothing) x m)

takeOne :: Ord a => MultiSet a -> [(a,MultiSet a)]
takeOne (MultiSet m) = [(x,deleteOne x (MultiSet m)) | x <- Map.keys m]

toMap :: MultiSet a -> Map.Map a Int
toMap (MultiSet m) = m

toMultiplicityList :: MultiSet a -> [(a,Int)]
toMultiplicityList (MultiSet m) = Map.toAscList m
