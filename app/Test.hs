module Test where

import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Paths_purelypoker
import Poker

parseHand :: String -> (Maybe Hand, Int)
parseHand = parseHand' . map read . splitOn ","
  where
    parseHand' [sc1, rc1, sc2, rc2, sc3, rc3, sc4, rc4, sc5, rc5, cl] =
      (identifyBestHand [mkCard rc1 sc1, mkCard rc2 sc2, mkCard rc3 sc3, mkCard rc4 sc4, mkCard rc5 sc5], cl)
    parseHand' _ = error "Invalid input"

mkCard :: Int -> Int -> Card
mkCard 1 s = Card Ace (toEnum $ s - 1)
mkCard r s = Card (toEnum $ r - 2) (toEnum $ s - 1)

input :: String -> IO [String]
input fp = lines <$> readFile fp

testBestHand :: Bool -> IO ()
testBestHand useLong = do
  fn <-
    getDataFileName $
      if useLong
        then "data/poker-hands/poker-hand-testing.data"
        else "data/poker-hands/poker-hand-training-true.data"
  ls <- input fn
  let parsed = parseHand <$> ls
      checked = parsed <&> (uncurry (==) . bimap (toInt <$>) pure)
      numbered = zip [1 ..] checked
      firstWrong = find (not . snd) numbered
      noWrong = maybe True snd firstWrong

  putStrLn $ if noWrong then "All tests succeeded" else "Wrong: " <> show (maybe 0 fst firstWrong)

toInt :: Hand -> Int
toInt (High _ _) = 0
toInt (Pair _ _) = 1
toInt (TwoPair _ _) = 2
toInt (ThreeKind _ _) = 3
toInt (Straight _) = 4
toInt (Flush _) = 5
toInt (FullHouse _ _) = 6
toInt (FourKind _) = 7
toInt (StraightFlush Ace) = 9
toInt (StraightFlush _) = 8

card :: String -> Maybe Card
card [r, 'h'] = Just $ card' r Hearts
card [r, 'd'] = Just $ card' r Diamonds
card [r, 's'] = Just $ card' r Spades
card [r, 'c'] = Just $ card' r Clubs
card _ = Nothing

card' = Card . rank'

cards :: String -> [Card]
cards = mapMaybe card . words

rank' 'A' = Ace
rank' 'K' = King
rank' 'Q' = Queen
rank' 'J' = Jack
rank' 'T' = Ten
rank' r = toEnum (digitToInt r - 2)
