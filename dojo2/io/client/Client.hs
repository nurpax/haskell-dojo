
import Network
import Text.JSON
import System.IO

import Types

server = "localhost"
port   = 4242

main :: IO ()
main = do
  putStrLn "IMPLEMENT CONNECTION"
  -- TODO change your player struct
  print $ Player "Jope"
  putStrLn . encode . Player $ "Jope"
