{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Function (on)
import Data.List (find, group, groupBy, maximumBy, nub, sort, sortBy, sortOn, tails, (\\))
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.MultiSet as M hiding (concatMap, filter, map, null, (\\))
import Data.Ord (comparing)
import Debug.Trace (trace)
import System.Random (mkStdGen)
import qualified System.Random.Shuffle as R

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq, Enum)

data Suit = Diamonds | Spades | Clubs | Hearts deriving (Eq, Enum, Ord)

instance Eq Card where
  (==) = (==) `on` rank

instance Ord Card where
  (<=) = (<=) `on` rank

data Card = Card {rank :: Rank, suit :: Suit}

data Hand
  = High Rank [Rank]
  | Pair Rank [Rank]
  | TwoPair Rank Rank (Maybe Rank)
  | ThreeKind Rank
  | Straight Rank
  | Flush Rank
  | FullHouse Rank Rank
  | FourKind Rank
  | StraightFlush Rank
  deriving (Show, Eq, Ord)

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
main = do
  d <- R.shuffleM deck
  let ((c11, c12), d1) = deal d
      ((c21, c22), d2) = deal d1
      (f, d3) = flop d2
      (t, d4) = turn d3
      (r, _) = river d4
      board = f ++ t ++ r
      p1 = identifyHands $ c11 : c12 : board
      p2 = identifyHands $ c21 : c22 : board
  print $ "Dealt to player 1: " <> show (c11, c12)
  print $ "Dealt to player 2: " <> show (c21, c22)
  print $ "Board: " <> show board

  case compare p1 p2 of
    LT -> print $ "Player 2 wins with: " <> show p2
    GT -> print $ "Player 1 wins with: " <> show p1
    EQ -> print "Chop"

-- utils

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

keepMostFreqSuit :: [Card] -> [Card]
keepMostFreqSuit cs = filter ((== mostFreqSuit cs) . suit) cs
  where
    mostFreqSuit = suit . head . maximumBy (comparing length) . groupBy sameSuit . sortOn suit

sameSuit :: Card -> Card -> Bool
sameSuit = (==) `on` suit

draw :: [Card] -> (Maybe Card, [Card])
draw [] = (Nothing, [])
draw (c : cs) = (Just c, cs)

deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Diamonds, Spades, Clubs, Hearts]

shuffle :: [Card] -> Int -> [Card]
shuffle cs n = R.shuffle' cs (length cs) (mkStdGen n)

flop, turn, river :: [Card] -> ([Card], [Card])
flop (c1 : c2 : c3 : cs) = ([c1, c2, c3], cs)
flop _ = error "not enough cards"
turn (c1 : cs) = ([c1], cs)
turn _ = error "not enough cards"
river = turn

deal :: [Card] -> ((Card, Card), [Card])
deal (c1 : c2 : cs) = ((c1, c2), cs)
deal _ = error "not enough cards"

occuranceKind :: Int -> [Card] -> [Rank]
occuranceKind n = fmap rank . M.foldOccur (\c oc cs -> if oc >= n then c : cs else cs) [] . M.fromList

kickers :: Int -> [Rank] -> [Card] -> [Rank]
kickers n xcs = take n . sortBy (flip compare) . filter (`notElem` xcs) . fmap rank

-- Hands
-- unsafe: assumes non-empty list
high :: [Card] -> Hand
high cs =
  let highest = rank $ maximum cs
   in High highest (kickers 4 [highest] cs)

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
threeKinds = fmap ThreeKind . occuranceKind 3

flushes :: [Card] -> [Hand]
flushes = flushes' [] . sort . keepMostFreqSuit
  where
    flushes' fs cs@(c1 : c2 : c3 : c4 : c5 : _)
      | all (sameSuit c1) [c2, c3, c4, c5] = flushes' (Flush (rank c5) : fs) (tail cs) -- normal flush
      | otherwise = fs -- not a flush
    flushes' fs _ = fs -- not enough cards

straights :: [Card] -> [Hand]
straights = straights' [] . nub . sort
  where
    straights' :: [Hand] -> [Card] -> [Hand]
    straights' ss cs@((Card r1 _) : _ : _ : _ : (Card r5 _) : _)
      | nRanksAsc 4 cs
          && r1 == Two
          && Ace `elem` (rank <$> cs) =
          straights' (Straight Five : ss) (filter ((/= Ace) . rank) (trace (show cs) cs)) -- wheel
      | nRanksAsc 5 cs = straights' (Straight r5 : ss) (tail cs) -- normal
      | otherwise = straights' ss (tail cs) -- not a straight
    straights' ss _ = ss -- not enough cards
    nRanksAsc n = ascending . fmap rank . take n
    ascending xs = and $ zipWith isSuccessor xs (tail xs)
    isSuccessor = (>=) . succ'
    succ' Ace = Two
    succ' r = succ r

fullHouses :: [Card] -> [Hand]
fullHouses cs = do
  ThreeKind rt <- threeKinds cs
  (Pair rp _) <- pairs $ filter ((/= rt) . rank) cs
  return $ FullHouse rt rp

fourKind :: [Card] -> Maybe Hand
fourKind cs =
  let qs = occuranceKind 4 cs
   in case qs of
        [r] -> Just $ FourKind r
        _ -> Nothing

straightFlushes :: [Card] -> [Hand]
straightFlushes = map toFlushStraight . straights . keepMostFreqSuit
  where
    toFlushStraight (Straight r) = StraightFlush r
