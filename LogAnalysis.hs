{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = go . words
  where go ("I":x':xs) = LogMessage Info (read x') (unwords xs)
        go ("W":x':xs) = LogMessage Warning (read x') (unwords xs)
        go ("E":x':xs) = let (y:ys) = xs in
            LogMessage (Error (read x')) (read y) (unwords ys)
        go xs = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m (Node t nm t')
  | ts > ts' = Node t nm $ insert m t'
  | otherwise = Node (insert m t) nm t'
  where ts = timestamp m
        ts' = timestamp nm

timestamp :: LogMessage -> TimeStamp
timestamp (Unknown _) = undefined
timestamp (LogMessage _ ts _) = ts

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t m t') = inOrder t ++ m : inOrder t'

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldr f [] . inOrder . build
  where
    f (LogMessage t _ s) acc = case t of
      (Error sev) -> if sev >= 50 then s:acc else acc
      _ -> acc
    f _ acc = acc
