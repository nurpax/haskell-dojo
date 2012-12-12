
import Control.Exception (bracket)
import Network
import System.IO
import Text.JSON

import Types

server = "localhost"
port   = 4242

player :: ClientReq
player = ClientReq "Jopex"

main :: IO ()
main =
  bracket (connectTo server (PortNumber (fromIntegral port))) hClose run
  where
    run h = do
      hSetBuffering h NoBuffering
      hPutStrLn h $ encode player
      mapM_ (takeCard h) [1, 2, 3]
 
    takeCard h n = do
      hPutStrLn h (show n)
      card <- fmap decodeCards (hGetLine h)
      print card
      either (error . show)
             (\c -> putStrLn $ "got card: " ++ show c)
             card
