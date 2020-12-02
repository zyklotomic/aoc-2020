import Text.Parsec
import Data.Either (partitionEithers)

data Policy = Policy Int Int Char
type Parsec' = Parsec String ()

isValid :: Policy -> String -> Bool
isValid (Policy minC maxC ch) str = minC <= k && k <= maxC
  where k = length . filter (== ch) $ str

isValidPt2 :: Policy -> String -> Bool
isValidPt2 (Policy i j ch) str = ((str !! (i-1) == ch) /= (str !! (j-1) == ch))

parsePolicy :: Parsec' (Policy, String)
parsePolicy = do
  minC <- many1 digit
  _ <- char '-'
  maxC <- many1 digit
  _ <- char ' '
  ch <- anyChar
  _ <- char ':' >> space
  password  <- many1 anyChar
  let p = Policy (read minC) (read maxC) ch
  return (p, password)

numValid :: [(Policy, String)] -> Int
numValid = length . filter (uncurry isValid)

numValidPt2 :: [(Policy, String)] -> Int
numValidPt2 = length . filter (uncurry isValidPt2)

main :: IO ()
main = do
  xs <- lines <$> readFile "input"
  let res = map (runParser parsePolicy () "") xs
  let (lefts, rights) = partitionEithers res
  mapM_ (putStrLn . show) lefts -- error checking
  putStr "Number correct pt1: "
  putStrLn . show . numValid $ rights
  putStr "Number correct pt2: "
  putStrLn . show . numValidPt2 $ rights
