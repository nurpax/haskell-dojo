
import Control.Concurrent
import Control.Exception (try)
import Control.Monad (forever)
import Network
import System.IO
import System.Random
import Text.JSON (encode)

import Types
import Util

data ServerState = ServerState {
    ssRandom :: StdGen
  , ssCards  :: [Card]
  }

drawCard :: ServerState -> (Card, ServerState)
drawCard (ServerState rnd (c:cs)) = (c, ServerState rnd cs)
drawCard (ServerState rnd [])     = (head cards, ServerState rnd' (tail cards))
  where
    (cards, rnd') = fisherYates rnd deck

    deck = do
      st <- [Hearts, Spades, Diamonds, Clubs]
      rn <- [1..13]
      return $ Card st rn


respondToClient :: MVar ServerState -> Handle -> Player -> IO ()
respondToClient stateMVar h player = do
  state <- takeMVar stateMVar
  let (card, state') = drawCard state
  putMVar stateMVar state'
  putStrLn $ "player " ++ playerName player ++ " connected"
  hPutStrLn h $ encode card

handleConn :: MVar ServerState -> (Handle, HostName, PortNumber) -> IO ()
handleConn stateMVar (h, hostname, port) = do
  hSetBuffering h LineBuffering
  putStrLn ("Accepted connection from " ++ hostname ++ " port " ++ show port)
  -- Ask the client for its player struct
  line <- try (hGetLine h) :: IO (Either IOError String)
  either (\e -> putStrLn $ "Ignoring error: " ++ show e)
         (respondToClient stateMVar h)
         (decodeLine line)
  hClose h
  where
    decodeLine (Left e) = Left . show $ e
    decodeLine (Right line) = decodePlayer line


main :: IO ()
main = do
  rnd <- getStdGen
  sharedSt <- newMVar $ ServerState rnd []
  sock <- listenOn (PortNumber 4242)
  forever $ accept sock >>= forkIO . handleConn sharedSt
