module Golf where

import Data.List.Split
import Data.List
import qualified Data.Map.Lazy as Map

skips :: [a] -> [[a]]
skips xs = map (\i -> concatMap (drop (i - 1)) (chunksOf i xs)) [1..(length xs)]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_,b,_) -> b) $ filter isMaxima (trips xs)

trips :: [Integer] -> [(Integer, Integer, Integer)]
trips xs = zipWith3 (\a b c -> (a,b,c)) xs (drop 1 xs) (drop 2 xs)

isMaxima :: (Integer, Integer, Integer) -> Bool
isMaxima (a, b, c) = (a < b) && (b > c)

histogram :: [Integer] -> String
histogram = addBottom . genHistogram . countNumbers

countNumbers :: [Integer] -> Map.Map Integer Int
countNumbers = Map.fromList . map (\x -> (head x, length x)) . group . sort

genHistogram :: Map.Map Integer Int -> String
genHistogram m = concatMap (genRow m) rows
  where rows = reverse [1..maxCount]
        maxCount = Map.foldr max 0 m

genRow :: Map.Map Integer Int -> Int -> String
genRow m row = map s [0..9] ++ "\n"
  where s n = if val n >= row then '*' else ' '
        val n = Map.findWithDefault 0 n m

-- countNumbers :: [Integer] -> [Integer]
-- countNumbers = foldr insertNumber $ replicate 10 0
-- countNumbers xs = map (fromIntegral . subtract 1 . length) . group . sort $ [0..9] ++ xs

-- countNumbers' :: [Integer] -> [(Integer, Integer)]
-- countNumbers' = map (\x -> (head x, fromIntegral $ length x)) . group . sort

-- insertNumber :: Integer -> [Integer] -> [Integer]
-- insertNumber _ [] = undefined
-- insertNumber 0 (x:xs) = (x+1):xs
-- insertNumber i (x:xs) = x : insertNumber (i - 1) xs
--
-- genHistogram :: [Integer] -> String
-- genHistogram xs = concatMap (genRow xs) rows
--   where rows = reverse [1..(maximum xs)]
--
-- genRow :: [Integer] -> Integer -> String
-- genRow [] _ = "\n"
-- genRow (x:xs) row = s : genRow xs row
--   where s = if x >= row then '*' else ' '

addBottom :: String -> String
addBottom = (++bottom)
  where bottom = "==========\n0123456789\n"
