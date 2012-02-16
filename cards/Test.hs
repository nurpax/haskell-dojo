
import Cards

import System.Exit
import Test.HUnit

-- Test data
h = Card Hearts
s = Card Spades
d = Card Diamonds
c = Card Clubs

royalFlush    = [d 1, d 13, d 12, d 11, d 10]

straightFlush = [c 9, c 8, c 7, c 6, c 5]

fourOfKind    = [s 1, c 3, d 3, s 3, h 3]

fullHouse     = [s 2, d 2, c 2, s 9, d 9]

flushH        = [h 2, h 5, h 6, h 9, h 13]
flushS        = [s 2, s 5, s 6, s 9, s 13]

straight      = [h 4, d 5, s 6, h 7, s 8]
straightD     = [d 1, d 2, s 3, d 4, h 5]
straightHi    = [s 10, d 11, d 12, h 13, s 1]

threeOfKind   = [h 12, c 10, c 13, h 10, s 10]

twoPairs      = [s 14, d 7, s 7, d 4, c 4]

pair          = [h 8, c 12, s 14, c 1, h 1]

highCard      = [h 4, s 5, d 8, d 8, h 14]

expectedHands :: [(Hand, [Card])]
expectedHands =
  [(RoyalFlush, royalFlush),
   (StraightFlush, straightFlush),
   (FourOfKind, fourOfKind),
   (FullHouse, fullHouse),
   (Flush, flushH),
   (Flush, flushS),
   (Straight, straight),
   (Straight, straightD),
   (Straight, straightHi),
   (TwoPairs, twoPairs),
   (Pair, pair)
   ]

pickCases :: (Hand -> Bool) -> [[Card]]
pickCases f =
  map snd . filter (f . fst) $ expectedHands

pickNonMatching :: Hand -> [[Card]]
pickNonMatching hand = pickCases (hand /=)

pickMatching :: Hand -> [[Card]]
pickMatching hand = pickCases (hand ==)


testcase h =
  show h ~:
  do
    assertBool "trueCases"  (all (\c -> checkHand c == h) $ pickMatching h)
    assertBool "falseCases" (all (not . (\c -> checkHand c == h)) $ pickNonMatching h)

tests = test [map testcase [RoyalFlush, StraightFlush, FourOfKind,
                            FullHouse, Flush, Straight, ThreeOfKind,
                            TwoPairs, Pair, HighCard]]

main :: IO ()
main = do
  runTestTT tests >> putStrLn "done."
  putStrLn ""

