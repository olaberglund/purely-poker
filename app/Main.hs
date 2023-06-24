{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Function (on)
import Data.List (find, group, groupBy, maximumBy, nub, sort, sortBy, tails, (\\))
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.MultiSet as M hiding (concatMap, filter, map, null, (\\))
import Data.Ord (comparing)
import Debug.Trace (trace)
import System.Random (mkStdGen)
import qualified System.Random.Shuffle as R

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq, Enum)

data Suit = Diamonds | Spades | Clubs | Hearts deriving (Eq, Enum, Ord)

data Card = Card {rank :: Rank, suit :: Suit}

data ByRank

data BySuit

instance Eq Card where
  (==) = (==) `on` rank

instance Ord Card where
  (<=) = (<=) `on` rank

data Flop = Flop Card Card Card deriving (Show)

-- A hand with it's highest rank(s), and kickers where needed
--
data Hand
  = High Rank [Rank]
  | Pair Rank [Rank]
  | TwoPair Rank Rank (Maybe Rank)
  | ThreeKind Rank [Rank]
  | Straight Rank
  | Flush Rank
  | FullHouse Rank Rank
  | FourKind Rank
  | StraightFlush Rank
  deriving (Show)

compareRanks :: Rank -> Rank -> Ordering
compareRanks = compare

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show Ten = "T"
  show r = show $ fromEnum r + 2

instance Show Card where
  show c = show (rank c) <> show (suit c)

instance Show Suit where
  show Diamonds = "d"
  show Spades = "s"
  show Clubs = "c"
  show Hearts = "h"

main :: IO ()
main = putStrLn "Hello, Haskell!"

draw :: [Card] -> (Maybe Card, [Card])
draw [] = (Nothing, [])
draw (c : cs) = (Just c, cs)

deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Diamonds, Spades, Clubs, Hearts]

shuffle :: [Card] -> [Card]
shuffle cs = R.shuffle' cs (length cs) (mkStdGen 0)

occuranceKind :: Int -> [Card] -> [Rank]
occuranceKind n = fmap rank . M.foldOccur (\c oc cs -> if oc >= n then c : cs else cs) [] . M.fromList

kickers :: Int -> [Rank] -> [Card] -> [Rank]
kickers n xcs = take n . sortBy (flip compare) . filter (`notElem` xcs) . fmap rank

identifyHands :: [Card] -> [Hand]
identifyHands cs =
  straightFlushes cs
    <++ catMaybes [fourKind cs]
    <++ fullHouses cs
    <++ flushes cs
    <++ straights cs
    <++ threeKinds cs
    <++ twoPairs cs
    <++ pairs cs
    <++ [high cs]
  where
    [] <++ xs = xs
    xs <++ _ = xs

-- unsafe: assumes non-empty list
high :: [Card] -> Hand
high cs = let highest = rank $ maximum cs in High highest (kickers 4 [highest] cs)

pairs :: [Card] -> [Hand]
pairs cs =
  let ps = occuranceKind 2 cs
   in Pair <$> ps <*> pure (kickers 3 ps cs)

twoPairs :: [Card] -> [Hand]
twoPairs cs = do
  ((Pair r1 _) : rest) <- tails $ pairs cs
  (Pair r2 _) <- rest
  return $ TwoPair r1 r2 (safeHead $ kickers 1 [r1, r2] cs)
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead xs = Just $ head xs

threeKinds :: [Card] -> [Hand]
threeKinds cs =
  let tkr = occuranceKind 3 cs
   in ThreeKind <$> tkr <*> pure (kickers 2 [head tkr] cs)

flushes :: [Card] -> [Hand]
flushes cs = flushes' (sortBy (compare `on` suit) cs) []
  where
    flushes' cs@(c1 : c2 : c3 : c4 : c5 : _) fs
      | all (sameSuit c1) [c2, c3, c4, c5] = flushes' (tail cs) (Flush (rank c5) : fs) -- normal flush
      | otherwise = fs -- not a flush
    flushes' _ fs = fs -- not enough cards
    sameSuit = (==) `on` suit

straights :: [Card] -> [Hand]
straights cs = straights' (nub $ sort cs) []
  where
    straights' :: [Card] -> [Hand] -> [Hand]
    straights' cs'@((Card r1 _) : _ : _ : _ : (Card r5 _) : _) ss
      | nRanksAsc 5 cs' = straights' (tail cs') (Straight r5 : ss) -- normal
      | nRanksAsc 4 cs'
          && r1 == Two
          && Ace `elem` (rank <$> cs) =
          straights' (tail cs') (Straight Five : ss) -- wheel
      | otherwise = straights' (tail cs') ss -- not a straight
    straights' _ ss = ss -- not enough cards
    nRanksAsc n = ascending . fmap rank . take n
    ascending xs = and $ zipWith isSuccessor xs (tail xs)
    isSuccessor = (>=) . succ'
    succ' Ace = Two
    succ' r = succ r

fullHouses :: [Card] -> [Hand]
fullHouses cs = do
  ThreeKind rt _ <- threeKinds cs
  (Pair rp _) <- pairs $ filter ((/= rt) . rank) cs
  return $ FullHouse rt rp

fourKind :: [Card] -> Maybe Hand
fourKind cs =
  let qs = occuranceKind 4 cs
   in case qs of
        [r] -> Just $ FourKind r
        _ -> Nothing

straightFlushes :: [Card] -> [Hand]
straightFlushes cs = map toFlushStraight $ straights $ filter ((== freqSuit) . suit) cs
  where
    (Card _ freqSuit) = head $ maximumBy (comparing length) (group $ sort cs)
    toFlushStraight (Straight r) = StraightFlush r
