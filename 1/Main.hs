import Data.List (foldl')
import qualified Data.Set

main = do
  xs <- lines <$> readFile "input"
  let xs' = map read xs
  putStrLn . show . get2020Prod $ xs'
  putStrLn . show . get2020Triple $ xs'

get2020Prod :: [Int] -> Maybe Int
get2020Prod xs = f <$> k
  where f x = x * (2020-x)
        (_, k) = foldSet xs

foldSet :: [Int] -> (Data.Set.Set Int, Maybe Int)
foldSet = foldl' f (mempty, Nothing)
  where f (s, k) a = case k of
          Just _ -> (s,k)
          Nothing -> if Data.Set.member a s
                     then (s, Just a)
                     else (Data.Set.insert (2020-a) s, Nothing)

-- edge case check symmetry, check that there are 3! = 6 copies
-- cause, WLOG indicies i,j,k -> i < j < k, such as 1010 + 1010
-- for the case of part1
get2020Triple :: [Int] -> [Int]
get2020Triple xs = [a*b*c | a<-xs, b<-xs, c<-xs, a+b+c==2020]
