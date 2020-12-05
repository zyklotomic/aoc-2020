import Data.List (foldl', sort)

main :: IO ()
main = interact solP2

sol :: String -> String
sol = (++ "\n") . show . maximum . map getSeatID . lines

solP2 :: String -> String
solP2 = (++ "\n") . show . findSeat . map getSeatID . lines

getRow :: String -> Int
getRow = foldl' (\acc x -> acc*2 + f x) 0 . take 7
  where f x = if x == 'F' then 0 else 1

getCol :: String -> Int
getCol = foldl' (\acc x -> acc*2 + f x) 0 . drop 7
  where f x = if x == 'L' then 0 else 1

getSeatID :: String -> Int
getSeatID s = r * 8 + c
  where r = getRow s
        c = getCol s

findSeat :: [Int] -> Maybe Int
findSeat seats = foldl' f Nothing s'
  where s = sort seats
        s' = zip s (drop 1 s)
        f seat (x, y) = case seat of
          Just k -> Just k
          Nothing -> if x+1 == y then Nothing else Just (x+1)
