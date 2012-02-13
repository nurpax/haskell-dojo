
import Data.List
import Control.Monad
import System.Exit

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
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

allSame :: Eq a => (Card -> a) -> [Card] -> Bool
allSame f (c:cs) = all (\e -> f e == f c) cs
allSame _ [] = False

sameSuit :: [Card] -> Bool
sameSuit = allSame suit

checkRoyalFlush :: [Card] -> Bool
checkRoyalFlush cards =
  sameSuit cards && (sort . map rank $ cards) == [10..14]

checkStraightFlush :: [Card] -> Bool
checkStraightFlush cards =
  sameSuit cards && (sort . map (subtract minVal . rank) $ cards) == [0..4]
  where
    minVal = minimum . map rank $ cards

-- list of 3-tuples where:
--  1st = card hand check function
--  2nd = hands that should return True
--  3rd = hands that should return False
testCases :: [([Card] -> Bool, [[Card]], [[Card]])]
testCases = [(checkRoyalFlush, [royalFlush], [straightFlush, fourOfKind, fullHouse, flush, straight]),
             (checkStraightFlush, [straightFlush], [fourOfKind, fullHouse, flush, straight])]

reportTest :: ([Card] -> Bool) -> [[Card]] -> [[Card]] -> IO ()
reportTest f trueCases falseCases =
  mapM_ (test True) trueCases >> mapM_ (test False) falseCases
    where
      test expected c =
        if f c == expected then putStrLn "OK"
        else putStrLn "Test failed" >> exitFailure

main :: IO ()
main =
  mapM_ (\(f, trueCases, falseCases) -> reportTest f trueCases falseCases) testCases
