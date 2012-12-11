
import Control.Exception (bracket)
import Network
import System.IO
import Text.JSON

import Types

server = "localhost"
port   = 4242

player :: Player
player = Player "Jopex"

main :: IO ()
main = do
  bracket (connectTo server (PortNumber (fromIntegral port))) hClose run
  where
    run h = do
      hSetBuffering h LineBuffering
      hPutStrLn h $ encode player
      card <- fmap decodeCard (hGetContents h)
      either (\e -> putStrLn $ "error: " ++ show e)
             (\c -> putStrLn $ "got card: " ++ show c)
             card
