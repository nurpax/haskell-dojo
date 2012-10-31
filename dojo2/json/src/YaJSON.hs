
module YaJSON (
    JSON(..)
  , parseJson
  ) where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Language
import qualified Data.Map as Map
import           Text.ParserCombinators.Parsec (parse, ParseError)

language = Language.javaStyle
lexer = Token.makeTokenParser language

identifier = Token.identifier lexer
commaSep = Token.commaSep lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
reservedOp = Token.reservedOp lexer
stringLiteral = Token.stringLiteral lexer
float = Token.float lexer
integer = Token.integer lexer

data JSON =
    JSONFloat Double
  | JSONInt Int
  | JSONString String
  | JSONDict (Map.Map String JSON)
  | JSONList [JSON]
    deriving (Eq, Ord, Show)

json =
  fmap JSONFloat (try float) <|>
  fmap (JSONInt . fromIntegral) integer <|>
  fmap JSONString (identifier <|> stringLiteral) <|>
  fmap (JSONDict . Map.fromList) (braces . commaSep $ pair) <|>
  fmap JSONList (brackets . commaSep $ json)
  where
    pair = do
      -- TODO how to handle mismatch better here
      JSONString l <- json
      reservedOp ":"
      r <- json
      return (l, r)

parseJson :: String -> Either ParseError JSON
parseJson = parse json ""
