import Data.List (foldl')

data Slope = Slope { right :: Int, down :: Int }

main :: IO ()
main = interact solP2

solP1 :: String -> String
solP1 inp = (show numTrees) ++ "\n"
  where grid = lines inp
        numTrees = treesEncountered grid

solP2 :: String -> String
solP2 inp = (show numTreesProd) ++ "\n"
  where grid = lines inp
        slopes = [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
        numTreesProd = product . map (treesEncountered' grid) $ slopes

treesEncountered :: [[Char]] -> Int
treesEncountered xs = fst . foldl' f (0, 0) $ xs
  where m = length (xs !! 0)
        f (cnt, ix) row = (cnt + if row !! ix == '#' then 1 else 0,
                           (ix + 3) `mod` m)

treesEncountered' :: [[Char]] -> Slope -> Int
treesEncountered' xs (Slope r d) = fst . foldl' f (0,(0,0)) $ xs
  where m = length (xs !! 0)
        f (cnt, (ix, jx)) row = if jx == 0
                              then (cnt + if row !! ix == '#' then 1 else 0,
                                    ((ix + r) `mod` m, jx + d - 1))
                              else (cnt, (ix, jx-1))
