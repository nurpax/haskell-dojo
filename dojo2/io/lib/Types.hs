
module Types (
    Card(..)
  , Suit(..)
  , Player(..)
  , decodeCard
  , decodePlayer
  ) where

import           Control.Applicative
import           Text.JSON

data Player = Player { playerName :: String }
    deriving (Show, Eq, Ord)

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
    deriving (Show, Eq, Ord)

toSuit :: String -> Either String Suit
toSuit "hearts"   = Right Hearts
toSuit "spades"   = Right Spades
toSuit "diamonds" = Right Diamonds
toSuit "clubs"    = Right Clubs
toSuit s = Left $ "unknown suit: "++s

fromSuit :: Suit -> String
fromSuit Hearts   = "hearts"
fromSuit Spades   = "spades"
fromSuit Diamonds = "diamonds"
fromSuit Clubs    = "clubs"

instance JSON Card where
  showJSON c =
    makeObj [ ("suit", showJSON . fromSuit . suit $ c)
            , ("rank", showJSON . rank $ c)
            ]

  readJSON object = do
    obj <- readJSON object
    Card <$> (valFromObj "suit" obj >>= toSuitJSON)
         <*>  valFromObj "rank" obj
    where
      toSuitJSON s = either Error Ok (toSuit s)


instance JSON Player where
  showJSON p =
    makeObj [("name", showJSON . playerName $ p)]

  readJSON object = do
    obj <- readJSON object
    Player <$> (valFromObj "name" obj)
    where
      toSuitJSON s = either Error Ok (toSuit s)

decodeCard :: String -> Either String Card
decodeCard = resultToEither . decode

decodePlayer :: String -> Either String Player
decodePlayer = resultToEither . decode
