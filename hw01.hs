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

-- Generates a list of moves
-- Moves from peg a to peg b
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (++) preMoves $ (a, b) : postMoves
  where
    preMoves = hanoi (n - 1) a c b
    postMoves = hanoi (n - 1) c b a
