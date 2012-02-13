
import Data.List

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, value :: Int } | Joker
    deriving (Show, Eq, Ord)

hearts = Card Hearts
spades = Card Spades
dmnds = Card Diamonds
clubs = Card Clubs

royalFlush = [dmnds 14, dmnds 13, dmnds 12, dmnds 11, dmnds 10]

straightFlush = [clubs 9, clubs 8, clubs 7, clubs 6, clubs 5]

fourOfKind = [spades 1, clubs 3, dmnds 3, spades 3, hearts 3]

fullHouse = [spades 2, dmnds 2, clubs 2, spades 9, dmnds 9]

flush = [hearts 2, hearts 5, hearts 6, hearts 9, hearts 13]

straight = [hearts 4, dmnds 5, spades 6, hearts 7, spades 8]

threeOfKind = [hearts 12, clubs 13, clubs 10, hearts 10, spades 10]

twoPairs = [spades 14, dmnds 7, spades 7, dmnds 4, clubs 4]

pair = [hearts 8, clubs 12, spades 14, clubs 1, hearts 1]

highCard = [hearts 4, spades 5, dmnds 8, dmnds 8, hearts 14]

allSame :: (Eq a, Eq b) => (a -> b) -> [a] -> Bool
allSame f (c:cs) = all (\e -> f c == f e) cs
allSame _ [] = False

checkRoyalFlush cards =
  allSame suit cards && (sort . map value $ cards) == [10..14]

main = do
  putStrLn "foo"
