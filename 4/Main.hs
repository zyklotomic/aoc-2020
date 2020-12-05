import Text.Parsec
import Data.Maybe
import Control.Monad
import Data.Either

type Parsec' = Parsec String ()
data Field = BirthYear | IssueYear | ExpYear | Height | HairColor
  | EyeColor | PassportID | CountryID deriving (Eq, Ord, Enum, Bounded, Show)

data Entry = Entry Field String deriving (Show)

data Passport = Passport
  { birthYear :: Maybe String
  , issueYear :: Maybe String
  , expYear :: Maybe String
  , height :: Maybe String
  , hairColor :: Maybe String
  , eyeColor :: Maybe String
  , passportID :: Maybe String
  , countryID :: Maybe String
  } deriving (Show)

-- Maybe I really need to learn how to use lenses
fieldToCode :: Field -> String
fieldToCode field = case field of
  BirthYear -> "byr"
  IssueYear -> "iyr"
  ExpYear -> "eyr"
  Height -> "hgt"
  HairColor -> "hcl"
  EyeColor -> "ecl"
  PassportID -> "pid"
  CountryID -> "cid"

fieldToRecord :: Field -> Passport -> Maybe String
fieldToRecord field = case field of
  BirthYear -> birthYear
  IssueYear -> issueYear
  ExpYear -> expYear
  Height -> height
  HairColor -> hairColor
  EyeColor -> eyeColor
  PassportID -> passportID
  CountryID -> countryID

updateRecord :: Field -> String -> Passport -> Passport
updateRecord fd str p = case fd of
  BirthYear -> p { birthYear = Just str }
  IssueYear -> p { issueYear = Just str }
  ExpYear -> p { expYear = Just str }
  Height -> p { height = Just str }
  HairColor -> p { hairColor = Just str }
  EyeColor -> p { eyeColor = Just str }
  PassportID -> p { passportID = Just str }
  CountryID -> p { countryID = Just str }

parseField :: Parsec' Field
parseField = foldl1 (<|>) $ fmap g [minBound..maxBound]
  where g :: Field -> Parsec' Field
        g fd = try (string (fieldToCode fd)) >> return fd

parseEntry :: Parsec' Entry
parseEntry = do
  fd <- parseField
  char ':'
  str <- manyTill anyChar $ lookAhead (space <|> endOfLine)
  return $ Entry fd str

parsePassport :: Parsec' Passport
parsePassport = do
  entries <- parseEntry `endBy` (space <|> endOfLine)
  return $ foldl f p entries
  where p = Passport { birthYear = Nothing
                     , issueYear = Nothing
                     , expYear = Nothing
                     , height = Nothing
                     , hairColor = Nothing
                     , eyeColor = Nothing
                     , passportID = Nothing
                     , countryID = Nothing
                     }
        f p' (Entry fd str) = let fieldNotPresent = isNothing . fieldToRecord fd $ p'
                              in if fieldNotPresent
                                 then updateRecord fd str p'
                                 else error "Duplicate fields"

parseAllPassports :: Parsec' [Passport]
parseAllPassports = parsePassport `sepBy` endOfLine

isValidPassport :: Passport -> Bool
isValidPassport p = all f rs
  where rs = [ birthYear, issueYear, expYear, height
             , hairColor, eyeColor, passportID
             ]
        f r = isJust . r $ p

-- yes i know parse don't validate but i got lazy
isValidPassportPt2 :: Passport -> Bool
isValidPassportPt2 p = isValidPassport p
                       && validBirth
                       && validIssue
                       && validExpr
                       && validHeight
                       && validHair
                       && validEye
                       && validPassport
  where r f = fromJust . f $ p
        byr = (read . r) birthYear :: Int
        iyr = (read . r) issueYear :: Int
        eyr = (read . r ) expYear :: Int
        validBirth = 1920 <= byr && byr <= 2002
        validIssue = 2010 <= iyr && iyr <= 2020
        validExpr = 2020 <= eyr && eyr <= 2030
        validHeight = let h = r height
                          units = drop ((length h)-2) h
                          val = read . take ((length h)-2) $ h
                      in if units == "cm"
                         then 150 <= val && val <= 193
                         else if units == "in"
                              then 59 <= val && val <= 76
                              else False
        validHair = let h = r hairColor
                    in length h == 7
                       && h !! 0 == '#'
                       && all (\ch -> ch `elem` "0123456789abcdef") (drop 1 h)
        validEye = any (== r eyeColor) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validPassport = let n = r passportID
                        in length n == 9 && all (\ch -> ch `elem` "0123456789") n

main :: IO ()
main = interact sol

sol :: String -> String
sol input = (++"\n") . show $ numValids
  where ps = let e = runParser parseAllPassports () "" input
             in case e of
                  Left _ -> error "oof, invalid input"
                  Right x -> x
        numValids = length . filter isValidPassportPt2 $ ps
