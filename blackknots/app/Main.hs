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
turns seq perm (x:xs) = let i = perm U.! x
                        in turns (Seq.adjust (+1) i seq) (perm U.// [(x,perm U.! (x+1)),(x+1,i)]) xs

showKnot :: Int -> Int -> String
showKnot width i = List.replicate i '|' ++ "><" ++ List.replicate (width - 2 - i) '|'

showKnots :: Int -> Knots -> [String]
showKnots width knots = map (showKnot width) knots

solve :: Seq.Seq Int -> Permutation -> Maybe Int -> Knots -> Knots -> [Knots]
solve !target !perm !lastI knots acc
  | all (<= 0) target = return (reverse acc ++ knots)
  | otherwise = (do let target' = toList target
                    (i,x,y) <- filter (\(i,x,y) -> x >= 1 && y >= 1) $ zip3 [0..] target' (tail target')
                    guard (case lastI of Just j -> i /= j ; Nothing -> True)
                    let m = min x y
                    t <- [m,m-1..1]
                    solve (Seq.adjust' (subtract t) i $ Seq.adjust' (subtract t) (i + 1) target) perm (Just i) knots (replicate (2 * t) i ++ acc)
                ) <|> (case knots of
                         x : xs -> solve target (perm U.// [(x,perm U.! (x+1)),(x+1,perm U.! x)]) (Just x) xs (x:acc)
                         [] -> empty
                      )

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
                             case solve (Seq.zipWith (-) targetTurns turns0) (U.enumFromN 0 size) Nothing knots [] of
                               [] -> putStrLn "Solution not found"
                               solution:_ -> putStr $ unlines $ showKnots size solution
            [] -> putStrLn "Usage: solver filename.txt"
