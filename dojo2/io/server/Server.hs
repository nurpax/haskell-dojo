
import Control.Concurrent
import Control.Monad (forever)
import Network
import System.IO
import System.Random

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


handleConn :: MVar ServerState -> (Handle, HostName, PortNumber) -> IO ()
handleConn stateMVar (h, hostname, port) = do
  state <- takeMVar stateMVar
  let (card, state') = drawCard state
  putMVar stateMVar state'
  hSetBuffering h NoBuffering
  putStrLn ("Accepted connection from " ++ hostname ++ " port " ++ show port)
  hPutStr h $ encodeCard card
  hClose h


main :: IO ()
main = do
  rnd <- getStdGen
  sharedSt <- newMVar $ ServerState rnd []
  sock <- listenOn (PortNumber 4242)
  forever $ accept sock >>= forkIO . handleConn sharedSt
