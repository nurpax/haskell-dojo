
import Network
import Text.JSON
import System.IO

import Types

server = "localhost"
port   = 4242

main :: IO ()
main = do
  putStrLn "IMPLEMENT CONNECTION"
  -- TODO change name in your request struct
  print (ClientReq "Jope")
  putStrLn . encode $ ClientReq "Jope"
