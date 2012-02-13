
data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, value :: Int } | Joker
    deriving (Show, Eq, Ord)

main = do
  putStrLn "foo"
