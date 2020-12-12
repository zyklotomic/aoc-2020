import Data.List (foldl')
import Data.Foldable
import qualified Data.Sequence as Seq ((|>), drop, fromList)
import qualified Data.Vector as Vector

main :: IO ()
main = interact solP2

solP1 :: String -> String
solP1 input = show ans
  where nums = map read . lines $ input
        ans = findNum nums

solP2 :: String -> String
solP2 input = show ans
  where nums = map read . lines $ input
        target = head $ findNum nums
        ans = findSum target nums

twoSum :: [Int] -> [Int]
twoSum xs = [ x + y | (i,x) <- xs', (j,y) <- xs', i < j]
  where xs' = zip [0..] xs

findNum :: [Int] -> [Int]
findNum ns = snd . foldl' f (s, []) . drop 25 $ ns
  where s = Seq.fromList . take 25 $ ns
        f (m, acc) n = let ts = twoSum . toList $ m
                       in if n `elem` ts
                          then (Seq.drop 1 (m Seq.|> n), acc)
                          else (Seq.drop 1 (m Seq.|> n), acc ++ [n])

findSum :: Int -> [Int] -> [Int]
findSum n ns = map sum' . filter f $ idxs
  where ns' = Vector.fromList ns
        k = Vector.length ns'
        idxs = [(i,j) | i <- [0..k-2], j <- [i+2..k]]
        f (i,j) = (== n) . sum . Vector.drop i . Vector.take j $ ns'
        min' (i,j) = minimum . Vector.drop i . Vector.take j $ ns'
        max' (i,j) = maximum . Vector.drop i . Vector.take j $ ns'
        sum' (i,j) = min' (i,j) + max' (i,j)
