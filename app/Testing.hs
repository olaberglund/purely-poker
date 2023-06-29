module Testing where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Main

-- test hands
highHand = identifyHands (cards "2d 8h 9s Jh 3h") == [High Jack [Nine, Eight, Three, Two]]

pairHand = identifyHands (cards "2d 2h 9s Jh 3d") == [Pair Two [Jack, Nine, Three]]

twoPairHand = identifyHands (cards "2d 2h 9s 9h 3h") == [TwoPair (TwoPairRank Two Nine) (Just Three)]

threeKindHand = identifyHands (cards "2d 2h 2s 9h 3h") == [ThreeKind Two [Nine, Three]]

wheelHand = identifyHands (cards "2d 3h 4s 5h Ah") == [Straight Five]

straightHand1 = identifyHands (cards "2d 3h 4s 5h 6h") == [Straight Six]

straightHand2 = identifyHands (cards "2d 3h 4s 5h 6h 7h") == [Straight Seven, Straight Six]

fullHouseHand1 = identifyHands (cards "Ad Ah As Kh Kh") == [FullHouse Ace King]

fullHouseHand2 = identifyHands (cards "Ad Ah As Kh Kc Jc") == [FullHouse Ace King]

fourKindHand = identifyHands (cards "Ad Ah As Ac Kh") == [FourKind Ace]

fourKindHand2 = identifyHands (cards "Ad Ah As Ac Kh Kc Jc") == [FourKind Ace]

straightFlushHand = identifyHands (cards "2d 3d 4d 5d 6d") == [StraightFlush Six]

wheelFlushHand = identifyHands (cards "2d 3d 4d 5d Ad") == [StraightFlush Five]

straightFlushHand2 = identifyHands (cards "2d 3d 4d 5d 6d 7d") == [StraightFlush Seven, StraightFlush Six]

flushHand = identifyHands (cards "2d 8d Jd Qd Ad") == [Flush [Ace, Queen, Jack, Eight, Two]]

-- run all tests and report when something is wrong
testHands :: IO ()
testHands = do
  let tests =
        [ highHand,
          pairHand,
          twoPairHand,
          threeKindHand,
          wheelHand,
          straightHand1,
          straightHand2,
          fullHouseHand1,
          fullHouseHand2,
          fourKindHand,
          fourKindHand2,
          straightFlushHand,
          wheelFlushHand,
          straightFlushHand2,
          flushHand
        ]
  let results = zip [1 ..] tests
  let failures = filter (not . snd) results
  if null failures
    then putStrLn "All tests passed"
    else putStrLn $ "Failed tests: " ++ show (fst <$> failures)

possible1 = possibleHands (tmap card ("Ac", "Ad")) [] [] == [Pair Ace []]

possible2 = possibleHands (tmap card ("Ac", "Ad")) [card "Ah"] [] == [ThreeKind Ace []]

possible3 = possibleHands (tmap card ("Ac", "7d")) [] [] == [High Ace [Seven]]

testPossibleHands :: IO ()
testPossibleHands = do
  let tests =
        [ possible1,
          possible2,
          possible3
        ]
  let results = zip [1 ..] tests
  let failures = filter (not . snd) results
  if null failures
    then putStrLn "All tests passed"
    else putStrLn $ "Failed tests: " ++ show (fst <$> failures)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)

card :: String -> Card
card [r, 'h'] = card' r Hearts
card [r, 'd'] = card' r Diamonds
card [r, 's'] = card' r Spades
card [r, 'c'] = card' r Clubs
card _ = error "invalid card"

card' = Card . rank'

cards :: String -> [Card]
cards = map card . words

rank' 'A' = Ace
rank' 'K' = King
rank' 'Q' = Queen
rank' 'J' = Jack
rank' 'T' = Ten
rank' r = toEnum (digitToInt r - 2)
