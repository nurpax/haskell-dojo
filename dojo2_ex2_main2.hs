import           YaJSON
import qualified Data.Map as M

testInput :: String
testInput = "[\"foo\",\"bar\",\"foo\",\"baz\",3,\"foo\"]"

countFoos :: JSON -> Int
countFoos json = 3 -- TODO implement!!

main :: IO ()
main = do
  checkParseOk $ parseJson testInput
  where
    checkParseOk (Left err)   = error (show err)
    checkParseOk (Right json) = print . countFoos $ json
