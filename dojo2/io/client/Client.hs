
import Control.Exception (bracket)
import Network
import System.IO

server = "localhost"
port   = 4242

main :: IO ()
main = do
  bracket (connectTo server (PortNumber (fromIntegral port))) hClose run
  where
    run h = do
      hSetBuffering h NoBuffering
      t <- hGetContents h
      putStrLn $ "server sez: " ++ t
