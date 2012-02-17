
module Cards where

import Data.Function
import Data.List
import Data.Ord

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
  deriving (Eq, Show)

sameSuit :: [Card] -> Bool
sameSuit (c:cs) = all (\e -> suit e == suit c) cs
sameSuit _ = False

checkRoyalFlush :: [Card] -> Bool
checkRoyalFlush cards =
  sameSuit cards && (sort $ map rank cards) == 1:[10..13]

checkStraight :: [Card] -> Bool
checkStraight cards =
  sort [rank c - minVal | c <- cards] == [0..4]
  where
    minVal = minimum $ map rank cards

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

checkHand :: [Card] -> Hand
checkHand cards
  | checkRoyalFlush cards = RoyalFlush
  | checkStraightFlush cards = StraightFlush
  | checkFourOfKind cards = FourOfKind
  | checkFullHouse cards = FullHouse
  | checkFlush cards = Flush
  | checkStraight cards = Straight
  | otherwise = checkRankPatterns cards
