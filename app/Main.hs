{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.DeepSeq (NFData, force)
import Control.Monad (guard)
import Control.Parallel.Strategies (rpar, rseq, runEval, runEvalIO)
import Data.Char (digitToInt)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (find, group, groupBy, maximumBy, nub, sort, sortBy, sortOn, tails, (\\))
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.MultiSet as M hiding (concatMap, filter, map, null, (\\))
import Data.Ord (comparing)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import qualified Math.Combinatorics.Multiset as CM
import System.Environment (getArgs)
import System.Random (mkStdGen)
import qualified System.Random.Shuffle as R

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq, Enum, Generic, NFData)

data Suit = Diamonds | Spades | Clubs | Hearts deriving (Eq, Enum, Ord)

instance Eq Card where
  (==) = (==) `on` rank

instance Ord Card where
  (<=) = (<=) `on` rank

data Card = Card {rank :: Rank, suit :: Suit}

data TwoPairRank = TwoPairRank Rank Rank deriving (Show, Eq, Generic, NFData)

instance Ord TwoPairRank where
  (TwoPairRank r11 r12) <= (TwoPairRank r21 r22) = sort [r11, r12] <= sort [r21, r22]

type Kicker = Rank

data Hand
  = High Rank [Kicker]
  | Pair Rank [Kicker]
  | TwoPair TwoPairRank (Maybe Kicker)
  | ThreeKind Rank [Kicker]
  | Straight Rank
  | Flush [Rank]
  | FullHouse Rank Rank
  | FourKind Rank
  | StraightFlush Rank
  deriving (Show, Eq, Ord, Generic, NFData)

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
  -- n <- getArgs <&> (read . head)
  -- guard (n >= 0 && n <= 5)
  let (cp1, d1) = deal (shuffle deck 0)
      (cp2, d2) = deal d1
      (b, rem) = drawN 4 d1

      -- p1 = runEval $ do
      --   p1' <- rpar (force (possibleHands (c11, c12) b rem))
      --   rseq p1'
      --   return p1'
      -- p2 = runEval $ do
      --   p2' <- rpar (force (possibleHands (c21, c22) b rem))
      --   rseq p2'
      --   return p2'

      p1 = possibleHands cp1 b rem
      p2 = possibleHands cp2 b rem
  let p1Wins = length $ filter id ((>) <$> p1 <*> p2)
      p2Wins = length $ filter id ((<) <$> p1 <*> p2)

  print $ "Player 1 wins " <> show (p1Wins * 100 `div` (p1Wins + p2Wins)) <> "% of the time"
  print $ "Player 2 wins " <> show (p2Wins * 100 `div` (p1Wins + p2Wins)) <> "% of the time"

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

possibleHands :: (Card, Card) -> [Card] -> [Card] -> [Hand]
possibleHands (c1, c2) board rem = combinationsOfLen (5 - length board) rem >>= identifyHands . (cs ++) -- \c -> let hs = identifyHands (cs ++ c) in trace (show hs) hs
  where
    cs = c1 : c2 : board

combinationsOfLen n rem =
  let x = CM.permutations $ CM.fromCounts [(True, n), (False, length rem - n)]
      fn mask = map fst $ filter snd (zip rem mask)
   in map fn x

-- c1 *== c2 = suit c1 == suit c2 && rank c1 == rank c2

keepMostFreqSuit :: [Card] -> [Card]
keepMostFreqSuit cs = filter ((== mostFreqSuit cs) . suit) cs
  where
    mostFreqSuit = suit . head . maximumBy (comparing length) . groupBy sameSuit . sortOn suit

sameSuit :: Card -> Card -> Bool
sameSuit = (==) `on` suit

draw :: [Card] -> (Maybe Card, [Card])
draw [] = (Nothing, [])
draw (c : cs) = (Just c, cs)

drawN :: Int -> [Card] -> ([Card], [Card])
drawN = splitAt

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
  [r1, r2] <- combinationsOfLen 2 (occuranceKind 2 cs)
  return $ TwoPair (TwoPairRank r1 r2) (safeHead $ kickers 1 [r1, r2] cs)
  where
    safeHead [] = Nothing
    safeHead xs = Just $ head xs

threeKinds :: [Card] -> [Hand]
threeKinds cs =
  let ts = occuranceKind 3 cs
   in ThreeKind <$> ts <*> pure (kickers 2 ts cs)

flushes :: [Card] -> [Hand]
flushes = flushes' [] . sortBy (flip compare) . keepMostFreqSuit
  where
    flushes' fs cs@(c1 : c2 : c3 : c4 : c5 : _)
      | all (sameSuit c1) [c2, c3, c4, c5] = flushes' (Flush (rank <$> take 5 cs) : fs) (tail cs) -- normal flush
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
          straights' (Straight Five : ss) (filter ((/= Ace) . rank) cs) -- wheel
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
straightFlushes = map toFlushStraight . straights . keepMostFreqSuit
  where
    toFlushStraight (Straight r) = StraightFlush r

-- https://stackoverflow.com/a/21288092/13131325

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])
