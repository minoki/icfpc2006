{-# LANGUAGE BangPatterns #-}
module Main where
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
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

perms :: Permutation -> Knots -> [Permutation]
perms !p [] = [p]
perms !p (x:xs) = p : perms (p U.// [(x,p U.! (x+1)),(x+1,p U.! x)]) xs

solve0 :: Int -> Seq.Seq Int -> [(Int,Permutation)] -> Knots -> [Knots]
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

solve1 :: Seq.Seq Int -> [(Int,Permutation)] -> Knots -> [Knots]
solve1 !target knots acc
  | all (<= 0) target = if any (< 0) target then
                          error "invalid"
                        else
                          return (reverse acc ++ map fst knots)
  | otherwise = case knots of
                  (!x,!perm):xs -> do let left target acc | x > 0 = do
                                                              let !i0 = perm U.! (x-1)
                                                                  !i1 = perm U.! x
                                                                  !m = min (target `Seq.index` i0) (target `Seq.index` i1)
                                                              t <- [m,m-1..0]
                                                              right (if t == 0 then target else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target) (replicate (2 * t) (x-1) ++ acc)
                                                          | otherwise = right target acc
                                          right target acc | x < U.length perm - 2 = do
                                                               let !i0 = perm U.! (x+1)
                                                                   !i1 = perm U.! (x+2)
                                                                   !m = min (target `Seq.index` i0) (target `Seq.index` i1)
                                                               t <- [m,m-1..0]
                                                               solve1 (if t == 0 then target else Seq.adjust' (subtract t) i0 $ Seq.adjust' (subtract t) i1 target) xs (replicate (2 * t) (x+1) ++ acc)
                                                           | otherwise = solve1 target xs acc
                                      left target (x:acc)
                  [] -> empty

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
                             print turns0
                             case solve0 size (Seq.zipWith (-) targetTurns turns0) (zip knots (tail $ perms (U.enumFromN 0 size) knots)) [] of
                               [] -> putStrLn "Solution not found"
                               solution:_ -> do putStr $ unlines $ showKnots size solution
                                                let turns1 = turns (Seq.replicate size 0) (U.enumFromN 0 size) solution
                                                print turns1
            [] -> putStrLn "Usage: solver filename.txt"
