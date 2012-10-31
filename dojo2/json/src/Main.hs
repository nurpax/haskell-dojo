
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Types
import qualified YaJSON as Y

testInputs :: [String]
testInputs =
  [ "{ \"suit\": \"hearts\", \"rank\": 1 }"
  , "{ \"suit\": \"spades\", \"rank\": 2 }"
  , "{ \"suit\": \"diamonds\", \"rank\": 3 }"
  , "{ \"suit\": \"clubs\", \"rank\": 4 }"
  ]

-- missing "suit"
testErr1 :: String
testErr1 = "{ \"rank\": 1 }"

-- misspelt "suit" content 'darts'
testErr2 :: String
testErr2 = "{ \"suit\": \"darts\", \"rank\": 1 }"

-- rank content has a string and not an int
testErr3 :: String
testErr3 = "{ \"suit\": \"spades\", \"rank\": \"1\" }"

toSuit :: String -> Either String Suit
toSuit "hearts"   = Right Hearts
toSuit "spades"   = Right Spades
toSuit "diamonds" = Right Diamonds
toSuit "clubs"    = Right Clubs
toSuit s = Left $ "unknown suit: "++s

-- Tries to handle errors with "error" but as you can see, the
-- matching code against the JSON tree is pretty nasty and error
-- handling is unwieldy.
parseCardYaJSON :: String -> Card
parseCardYaJSON s =
  case Y.parseJson s of
    Left err -> error (show err)
    Right r -> jsonToCard r
  where
    jsonToCard (Y.JSONDict d) =
      -- Lookup returns a Maybe type so either "Nothing" or "Just a",
      -- Nothing's indicate a parse error in that the input is
      -- malformed.
      let suit = M.lookup "suit" d
          rank = M.lookup "rank" d in
      case suit of
        Just (Y.JSONString s) ->
          case rank of
            Just (Y.JSONInt r) ->
              case toSuit s of
                -- not even toSuit succeeded!
                Left err -> error err
                Right s' -> Card s' r
            Just _ -> error "rank must be an int"
            Nothing -> error "no rank element in input"
        Just _ -> error "suit must be an int"
        Nothing -> error "no suit element in input"

    jsonToCard _ = error "malformed structure"

------------------------------------------------------------------

-- Use the Either monad to deal with errors using small, composable
-- helper functions.  The try* functions would be part of a re-usable
-- JSON library.

tryTakeElement :: String -> M.Map String Y.JSON -> Either String Y.JSON
tryTakeElement eltName dict =
  maybe
    (Left ("missing elt '"++eltName++"'"))
    Right
    (M.lookup eltName dict)

tryTakeString :: String -> M.Map String Y.JSON -> Either String String
tryTakeString eltName dict = do
  elt <- tryTakeElement eltName dict
  case elt of
    Y.JSONString s -> return s
    _ -> Left "expecting a string JSON type"

tryTakeInt :: String -> M.Map String Y.JSON -> Either String Int
tryTakeInt eltName dict = do
  elt <- tryTakeElement eltName dict
  case elt of
    Y.JSONInt i -> return i
    _ -> Left "expecting an int JSON type"

-- Easier to read parser using above helper functions
parseCardYaJSONImproved :: String -> Either String Card
parseCardYaJSONImproved s =
  case Y.parseJson s of
    Left err -> Left (show err)
    Right r -> jsonToCard r
  where
    jsonToCard (Y.JSONDict d) = do
      suit <- tryTakeString "suit" d >>= toSuit
      rank <- tryTakeInt "rank" d
      return $ Card suit rank


main :: IO ()
main = do
  putStrLn "json outputs with YaJSON"
  mapM_ (print . parseCardYaJSON) testInputs
  putStrLn "json outputs with YaJSON (improved)"
  mapM_ (print . parseCardYaJSONImproved) testInputs
  putStrLn "demonstrate a few error cases"
  print $ parseCardYaJSONImproved testErr1
  print $ parseCardYaJSONImproved testErr2
  print $ parseCardYaJSONImproved testErr3
