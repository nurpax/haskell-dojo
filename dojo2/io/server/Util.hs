
module Util (
    fisherYates
  , note
  , readInt) where

import Data.Map hiding (foldl)
import Data.Maybe (listToMaybe)
import System.Random

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)

note :: String -> Maybe a -> Either String a
note s Nothing  = Left s
note _ (Just a) = Right a

readInt :: String -> Either String Int
readInt s =
  note "failed int decode" $ (fmap fst . listToMaybe . reads $ s)

