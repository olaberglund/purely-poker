module Testing where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Main

-- test hands
highHand = identifyBestHand (cards "2d 8h 9s Jh 3h") == Just (High Jack [Nine, Eight, Three, Two])

pairHand = identifyBestHand (cards "2d 2h 9s Jh 3d") == Just (Pair Two [Jack, Nine, Three])

twoPairHand = identifyBestHand (cards "2d 2h 9s 9h 3h") == Just (TwoPair (TwoPairRank Two Nine) (Just Three))

threeKindHand = identifyBestHand (cards "2d 2h 2s 9h 3h") == Just (ThreeKind Two [Nine, Three])

wheelHand = identifyBestHand (cards "2d 3h 4s 5h Ah") == Just (Straight Five)

straightHand1 = identifyBestHand (cards "2d 3h 4s 5h 6h") == Just (Straight Six)

straightHand2 = identifyBestHand (cards "2d 3h 4s 5h 6h 7h") == Just (Straight Seven)

fullHouseHand1 = identifyBestHand (cards "Ad Ah As Kh Kh") == Just (FullHouse Ace King)

fullHouseHand2 = identifyBestHand (cards "Ad Ah As Kh Kc Jc") == Just (FullHouse Ace King)

fourKindHand = identifyBestHand (cards "Ad Ah As Ac Kh") == Just (FourKind Ace)

fourKindHand2 = identifyBestHand (cards "Ad Ah As Ac Kh Kc Jc") == Just (FourKind Ace)

straightFlushHand = identifyBestHand (cards "2d 3d 4d 5d 6d") == Just (StraightFlush Six)

wheelFlushHand = identifyBestHand (cards "2d 3d 4d 5d Ad") == Just (StraightFlush Five)

straightFlushHand2 = identifyBestHand (cards "2d 3d 4d 5d 6d 7d") == Just (StraightFlush Seven)

flushHand = identifyBestHand (cards "2d 8d Jd Qd Ad") == Just (Flush [Ace, Queen, Jack, Eight, Two])

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
