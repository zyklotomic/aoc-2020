import Text.Parsec
import qualified Data.Set as Set
import Data.Either
import qualified Data.Vector as Vector
import Control.Monad
import Data.Maybe

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
type Parsec' = Parsec String ()

main :: IO ()
main = do
  xs <- lines <$> readFile "input"
  let res = map (runParser parseInstruction () "") xs
  let (lefts, rights) = partitionEithers res
  --mapM (putStrLn . show) rights
  mapM_ (putStrLn . show) lefts
  let acc = swapInstructions (Vector.fromList (rights)) mempty 0 0
  putStrLn . show $ acc

parseAcc :: Parsec' Instruction
parseAcc = do
  string "acc "
  pm <- oneOf "+-"
  val <- many1 digit
  let k = if pm == '-' then (*(-1)) else (*1)
  return $ Acc ((k . read) val)

parseJmp :: Parsec' Instruction
parseJmp = do
  string "jmp "
  pm <- oneOf "+-"
  val <- many1 digit
  let k = if pm == '-' then (*(-1)) else (*1)
  return $ Jmp ((k . read) val)

parseNop :: Parsec' Instruction
parseNop = do
  string "nop "
  pm <- oneOf "+-"
  val <- many1 digit
  let k = if pm == '-' then (*(-1)) else (*1)
  return $ Nop ((k . read) val)

parseInstruction :: Parsec' Instruction
parseInstruction = parseAcc <|> parseJmp <|> parseNop

followInstructions :: Vector.Vector Instruction -> Set.Set Int -> Int -> Int -> Maybe Int
followInstructions v s i acc =
  if Set.member i s
  then Nothing -- return acc instead for solution to part 1
  else if length v == i
       then Just acc
       else case v Vector.! i of
              Nop _ -> followInstructions v s' (i+1) acc
              Acc x -> followInstructions v s' (i+1) (acc+x)
              Jmp x -> followInstructions v s' (i+x) acc
  where s' = Set.insert i s

swapInstructions :: Vector.Vector Instruction -> Set.Set Int -> Int -> Int -> Int
swapInstructions v s i acc =
  case v Vector.! i of
    Nop x -> case followInstructions v s' (i+x) acc of
      Just y -> y
      Nothing -> swapInstructions v s' (i+1) acc
    Acc x -> swapInstructions v s' (i+1) (acc+x)
    Jmp x -> case followInstructions v s' (i+1) acc of
      Just y -> y
      Nothing -> swapInstructions v s' (i+x) acc
  where s' = Set.insert i s
