
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

toSuit :: String -> Suit
toSuit "hearts"   = Hearts
toSuit "spades"   = Spades
toSuit "diamonds" = Diamonds
toSuit "clubs"    = Clubs

-- Tries to handle errors with "error" but as you can see, the
-- matching code against the JSON tree is pretty nasty and error
-- handling is unwieldy.
parseCardYaJSON :: String -> Card
parseCardYaJSON s =
  case Y.parseJson s of
    Left _ -> error "unhandled"
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
              Card (toSuit s) (fromIntegral r)
            Just _ -> error "rank must be an int"
            Nothing -> error "no rank element in input"
        Just _ -> error "suit must be an int"
        Nothing -> error "no suit element in input"

    jsonToCard _ = error "malformed structure"

main :: IO ()
main = do
  putStrLn "json outputs with YaJSON"
  mapM_ (print . parseCardYaJSON) testInputs
