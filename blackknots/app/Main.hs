{-# LANGUAGE BangPatterns #-}
module Main where
import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Data.Foldable
import qualified Data.IntSet         as IntSet
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.List           as List
import           Data.Maybe
import qualified Data.Sequence       as Seq
import qualified Data.Vector.Unboxed as U
import           System.Environment
import           Text.Read

parseSpec :: String -> Maybe (Int,Int,Int)
parseSpec s = listToMaybe $ do (x,rest) <- reads s
                               ("->",rest') <- lex rest
                               ((y,z),"") <- reads rest'
                               return (x,y,z)

type Permutation = U.Vector Int
type Knots = [Int] -- (i i+1)

buildPermutation :: Permutation -> Knots
buildPermutation perm | U.length perm <= 1 = []
                      | otherwise = let n = U.length perm - 1
                                        t = U.last perm
                                        xs = [n-1,n-2..t]
                                        perm' = U.map (\x -> if x > t then x - 1 else x) $ U.init perm
                                    in buildPermutation perm' ++ xs

turns :: Seq.Seq Int -> Permutation -> Knots -> Seq.Seq Int
turns seq perm [] = seq
turns seq perm (x:xs) = let !i = perm U.! x
                        in turns (Seq.adjust (+1) i seq) (perm U.// [(x,perm U.! (x+1)),(x+1,i)]) xs

showKnot :: Int -> Int -> String
showKnot width i = List.replicate i '|' ++ "><" ++ List.replicate (width - 2 - i) '|'

showKnots :: Int -> Knots -> [String]
showKnots width knots = map (showKnot width) knots

sortKnots :: Knots -> Knots
sortKnots [] = []
sortKnots (x:xs) = let go set map [] acc = IntMap.lookupMin map
                       go set map (y:ys) acc | IntSet.member y set || IntSet.member (y-1) set || IntSet.member (y+1) set || y >= x - 1 = go (IntSet.insert y set) map ys (y:acc)
                                             | otherwise = go (IntSet.insert y set) (IntMap.insert y (reverse acc ++ ys) map) ys (y:acc)
                   in case go (IntSet.singleton x) IntMap.empty xs [] of
                        Just (y, ys)  -> y : sortKnots (x:ys)
                        Nothing -> x : sortKnots xs

showKnotsCompressed :: Int -> Knots -> [String]
showKnotsCompressed width knots = go $ sortKnots knots
  where
    go :: Knots -> [String]
    go [] = []
    go (x:xs) = let (rest,xs') = go2 (x+2) IntSet.empty IntMap.empty xs []
                in (List.replicate x '|' ++ "><" ++ rest) : go xs'
    go2 :: Int -> IntSet.IntSet -> IntMap.IntMap Knots -> Knots -> Knots -> (String, Knots)
    go2 !y0 set map [] acc = case IntMap.lookupMin map of
                               Nothing -> (List.replicate (width - y0) '|', reverse acc)
                               Just (y, xs) -> let (rest,xs') = go2 (y+2) IntSet.empty IntMap.empty xs []
                                               in (List.replicate (y - y0) '|' ++ "><" ++ rest, xs')
    go2 !y0 set map (x:xs) acc | IntSet.member x set || IntSet.member (x-1) set || IntSet.member (x+1) set || x < y0 = go2 y0 (IntSet.insert x set) map xs (x:acc)
                               | otherwise = go2 y0 (IntSet.insert x set) (IntMap.insert x (reverse acc ++ xs) map) xs (x:acc)

perms :: Permutation -> Knots -> [Permutation]
perms !p []     = [p]
perms !p (x:xs) = p : perms (p U.// [(x,p U.! (x+1)),(x+1,p U.! x)]) xs

data Way = None | Both | RightOnly deriving Eq

solve0 :: Int -> Seq.Seq Int -> [(Int,Permutation,Way)] -> Knots -> [Knots]
solve0 !width !target knots acc = goEven target 0 acc
  where
    goEven !target !i acc | i >= width - 1 = goOdd target 1 acc
                          | otherwise = do let !m = min (target `Seq.index` i) (target `Seq.index` (i + 1))
                                           t <- [m,m-1..0]
                                           goEven (if t == 0 then target else Seq.adjust' (subtract t) i $ Seq.adjust' (subtract t) (i + 1) target) (i + 2) (replicate (2 * t) i ++ acc)
    goOdd !target !i acc | i >= width - 1 = solve1 target knots acc
                         | otherwise = do let !m = min (target `Seq.index` i) (target `Seq.index` (i + 1))
                                          t <- [m,m-1..0]
                                          goOdd (if t == 0 then target else Seq.adjust' (subtract t) i $ Seq.adjust' (subtract t) (i + 1) target) (i + 2) (replicate (2 * t) i ++ acc)

adjust'' :: Int -> Int -> Seq.Seq Int -> Seq.Seq Int
adjust'' 0 _ seq = seq
adjust'' t i seq = Seq.adjust' (subtract t) i seq

solve1 :: Seq.Seq Int -> [(Int,Permutation,Way)] -> Knots -> [Knots]
solve1 !target knots acc
  | all (<= 0) target = if any (< 0) target then
                          error "invalid"
                        else
                          return {- $ Right -} (reverse acc ++ map (\(x,_,_) -> x) knots)
  | otherwise = case knots of
                  (x,perm,None):xs -> solve1 target xs (x:acc)
                  (!x,!perm,way):xs -> do let left target acc | x > 0 = do
                                                                  let !i0 = perm U.! (x-1)
                                                                      !i1 = perm U.! x
                                                                      !m = min (target `Seq.index` i0) (target `Seq.index` i1)
                                                                  t <- [m,m-1..0]
                                                                  pure (t, if t == 0 then target else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target) -- (replicate (2 * t) (x-1) ++ acc)
                                                                  -- right (if t == 0 then target else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target) (replicate (2 * t) (x-1) ++ acc)
                                                              | otherwise = pure (0, target) -- right target acc
                                              right target acc | x < U.length perm - 2 = do
                                                                   let !i0 = perm U.! (x+1)
                                                                       !i1 = perm U.! (x+2)
                                                                       !m = min (target `Seq.index` i0) (target `Seq.index` i1)
                                                                   t <- [m,m-1..0]
                                                                   pure (t, if t == 0 then target else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target)
                                                                   -- solve1 (if t == 0 then target else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target) xs (replicate (2 * t) (x+1) ++ acc)
                                                               | otherwise = pure (0, target) -- solve1 target xs acc
                                          -- left target (x:acc)
                                          (l,target') <- if way == RightOnly then [(0,target)] else left target acc
                                          (r,target'') <- right target' acc
                                          if l >= 1 && r >= 1 then do
                                            {- let !i0 = perm U.! (x-1)
!i1 = perm U.! (x+2)
!m = min (target'' `Seq.index` i0) (target'' `Seq.index` i1)
t <- [m,m-1..0] -}
                                            let target''' = target'' -- if t == 0 then target'' else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target''
                                                p0 = perm U.// [(x+1,perm U.! (x+2)),(x+2,perm U.! (x+1))]
                                                p1 = perm U.// [(x-1,perm U.! x),(x,perm U.! (x-1)),(x+1,perm U.! (x+2)),(x+2,perm U.! (x+1))]
                                                p2 = perm U.// [(x-1,perm U.! x),(x,perm U.! (x-1))]
                                            solve1 target''' ((x+1,p0,RightOnly) : (x-1,p1,Both) : (x+1,p2,None) : (x-1,perm,None) : xs) (replicate (2 * r - 2) (x+1) ++ replicate (2 * l - 2) (x-1) ++ (x:acc))
                                            else if l >= 1 then do
                                            let p1 = perm U.// [(x-1,perm U.! x),(x,perm U.! (x-1))]
                                            solve1 target'' ((x-1,p1,Both) : (x-1,perm,None) : xs) (replicate (2 * l - 2) (x-1) ++ (x:acc))
                                            else if r >= 1 then do
                                            let p1 = perm U.// [(x+1,perm U.! (x+2)),(x+2,perm U.! (x+1))]
                                            solve1 target'' ((x+1,p1,Both) : (x+1,perm,None) : xs) (replicate (2 * r - 2) (x+1) ++ (x:acc))
                                            else do
                                            solve1 target'' xs (replicate (2 * r) (x+1) ++ replicate (2 * l) (x-1) ++ (x:acc))
                  [] -> empty -- return $ Left (target, reverse acc)

main :: IO ()
main = do args <- getArgs
          case args of
            filename:_ -> do content <- readFile filename
                             let l = map (fromJust . parseSpec) $ lines content
                             let perm = U.fromList $ map (\(_,y,_) -> y) l
                             let size = U.length perm
                             let knots = buildPermutation perm
                             let targetTurns = Seq.fromList $ map (\(_,_,z) -> z) l
                             let turns0 = turns (Seq.replicate size 0) (U.enumFromN 0 size) knots
                             putStr $ unlines $ showKnots size knots
                             putStrLn "---"
                             putStr $ unlines $ showKnotsCompressed size knots
                             print turns0
                             let result0 = take 100 $ solve0 size (Seq.zipWith (-) targetTurns turns0) (zip3 knots (tail $ perms (U.enumFromN 0 size) knots) (repeat Both)) []
                             case result0 of
                               [] -> do putStrLn "No simple solution found"
                               {-
                                        putStr $ unlines $ showKnots size $ snd $ fromLeft (undefined,undefined) (result0 !! 0)
                                        let result1 = do Left (target, knots) <- result0
                                                         solve1 target (zip knots (tail $ perms (U.enumFromN 0 size) knots)) []
                                        case [x | Right x <- result1] of
                                          [] -> putStrLn "Solution not found"
                                          solution:_ -> do putStr $ unlines $ showKnots size solution
                                                           let turns1 = turns (Seq.replicate size 0) (U.enumFromN 0 size) solution
                                                           print turns1
-}
                               solution:_ -> do putStr $ unlines $ showKnots size solution
                                                putStrLn "---"
                                                putStr $ unlines $ showKnotsCompressed size solution
                                                let turns1 = turns (Seq.replicate size 0) (U.enumFromN 0 size) solution
                                                print turns1
            [] -> putStrLn "Usage: solver filename.txt"
