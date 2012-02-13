
import Data.Function
import Data.List
import Data.Ord
import System.Exit

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
    deriving (Show, Eq, Ord)

data Hand =
    RoyalFlush
  | StraightFlush
  | FourOfKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfKind
  | TwoPairs
  | Pair
  | HighCard
  deriving (Eq)

sameSuit :: [Card] -> Bool
sameSuit (c:cs) = all (\e -> suit e == suit c) cs
sameSuit _ = False

checkRoyalFlush :: [Card] -> Bool
checkRoyalFlush cards =
  sameSuit cards && (sort . map rank $ cards) == [10..14]

checkStraight :: [Card] -> Bool
checkStraight cards =
  (sort . map (subtract minVal . rank) $ cards) == [0..4]
  where
    minVal = minimum . map rank $ cards

checkFlush :: [Card] -> Bool
checkFlush = sameSuit

checkStraightFlush :: [Card] -> Bool
checkStraightFlush cards =
  checkFlush cards && checkStraight cards

-- Check rank based hands
checkRankPatterns :: [Card] -> Hand
checkRankPatterns cards =
  matchRankGroups (sortBy (comparing length) rankGroups) where
    rankGroups = groupBy ((==) `on` rank) . sortBy (comparing rank) $ cards
    matchRankGroups [[_], [_,_,_,_]] = FourOfKind
    matchRankGroups [[_,_], [_,_,_]] = FullHouse
    matchRankGroups [[_], [_,_], [_,_]] = TwoPairs
    matchRankGroups [[_], [_], [_], [_,_]] = Pair
    matchRankGroups _ = HighCard

checkFourOfKind cards = checkRankPatterns cards == FourOfKind
checkFullHouse cards = checkRankPatterns cards == FullHouse
checkTwoPairs cards = checkRankPatterns cards == TwoPairs
checkPair cards = checkRankPatterns cards == Pair

-- Test data
h = Card Hearts
s = Card Spades
d = Card Diamonds
c = Card Clubs

royalFlush    = [d 14, d 13, d 12, d 11, d 10]

straightFlush = [c 9, c 8, c 7, c 6, c 5]

fourOfKind    = [s 1, c 3, d 3, s 3, h 3]

fullHouse     = [s 2, d 2, c 2, s 9, d 9]

flush         = [h 2, h 5, h 6, h 9, h 13]

straight      = [h 4, d 5, s 6, h 7, s 8]

threeOfKind   = [h 12, c 13, c 10, h 10, s 10]

twoPairs      = [s 14, d 7, s 7, d 4, c 4]

pair          = [h 8, c 12, s 14, c 1, h 1]

highCard      = [h 4, s 5, d 8, d 8, h 14]

-- list of 3-tuples where:
--  1st = card hand check function
--  2nd = hands that should return True
--  3rd = hands that should return False
testCases :: [([Card] -> Bool, [[Card]], [[Card]])]
testCases = [(checkRoyalFlush, [royalFlush], [straightFlush, fourOfKind, fullHouse, flush, straight]),
             (checkStraightFlush, [straightFlush], [fourOfKind, fullHouse, flush, straight]),
             (checkFourOfKind, [fourOfKind], [twoPairs, fullHouse]),
             (checkFullHouse, [fullHouse], [royalFlush, fourOfKind, flush, straightFlush, pair]),
             (checkFlush, [flush, royalFlush, straightFlush], [twoPairs, threeOfKind]),
             (checkStraight, [straight], [twoPairs, fullHouse, fourOfKind, threeOfKind, pair]),
             (checkTwoPairs, [twoPairs], [royalFlush, fourOfKind, fullHouse, flush, straightFlush, pair]),
             (checkPair, [pair], [twoPairs, royalFlush, fourOfKind, fullHouse, flush, straightFlush])
             ]

reportTest :: ([Card] -> Bool) -> [[Card]] -> [[Card]] -> IO ()
reportTest f trueCases falseCases =
  mapM_ (test True) trueCases >> mapM_ (test False) falseCases
    where
      test expected c =
        if f c == expected then putStr "OK "
        else putStrLn "Test failed" >> exitFailure

main :: IO ()
main =
  mapM_ (\(f, trueCases, falseCases) -> reportTest f trueCases falseCases) testCases
