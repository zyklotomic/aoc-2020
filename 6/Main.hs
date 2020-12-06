import Data.List
import Data.List.Split
import qualified Data.Set as Set


-- Had to modify the input to remove the last newline / eof
-- lazy fix due to my lack of parsing diligence
main :: IO ()
main = interact groupCountPt2

sol :: String -> String
sol input = (++ "\n") . show $ total
  where
    groups = splitGroups input
    counts = map groupCount groups
    total = sum counts

splitGroups :: String -> [String]
splitGroups = splitOn "\n\n"

groupCount :: String -> Int
groupCount = Set.size . Set.fromList . filter (/= '\n')

singleGroupCountP2 :: [String] -> Int
singleGroupCountP2 group = Set.size . foldl' f firstSet $ drop 1 group
  where firstSet = Set.fromList . head $ group
        f set person = set `Set.intersection` (Set.fromList person)

tmp :: String -> String
tmp = show . map (splitOn "\n") . splitGroups

groupCountPt2 :: String -> String
groupCountPt2 input = show . sum $ counts
  where groups = map (splitOn "\n") $ splitGroups input
        counts = map singleGroupCountP2 groups
