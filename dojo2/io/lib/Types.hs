
module Types (
    Card(..)
  , Suit(..)
  , ClientReq(..)
  , decodeCard
  , decodeClientReq
  ) where

import           Control.Applicative
import           Text.JSON

data ClientReq = ClientReq {
    crPlayerName :: String
  , crNCards :: Int
  } deriving (Show, Eq, Ord)

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


instance JSON ClientReq where
  showJSON p =
    makeObj [ ("name",   showJSON . crPlayerName $ p)
            , ("nCards", showJSON . crNCards $ p)]

  readJSON object = do
    obj <- readJSON object
    ClientReq <$> valFromObj "name" obj <*> valFromObj "nCards" obj
    where
      toSuitJSON s = either Error Ok (toSuit s)

decodeCard :: String -> Either String Card
decodeCard = resultToEither . decode

decodeClientReq :: String -> Either String ClientReq
decodeClientReq = resultToEither . decode
