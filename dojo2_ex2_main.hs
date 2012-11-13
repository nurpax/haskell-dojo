import           YaJSON
import qualified Data.Map as M

testInput :: String
testInput =  "[\"foo\",\"bar\",\"foo\",\"baz\",3,\"foo\"]"

main :: IO ()
main = do
  print . parseJson $ testInput
