module HW01 where

toDigitsRev :: Integer -> [Integer]
toDigitsRev i
  | i <= 0 = []
  | otherwise = r : toDigitsRev q
  where (q, r) = quotRem i 10

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherR :: [Integer] -> [Integer]
doubleEveryOtherR = reverse . doubleEveryOther . reverse

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev
-- My initial implementation (before linter cleaned it up):
-- sumDigits = sum . foldr (++) [] . map toDigitsRev

-- Validates credit card numbers
validate :: Integer -> Bool
validate = (==0) . flip rem 10 . sumDigits . doubleEveryOther . toDigitsRev
