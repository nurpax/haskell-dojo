
import Control.Concurrent
import Control.Monad (forever)
import Network.Socket
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


handleConn :: MVar ServerState -> (Socket, SockAddr) -> IO ()
handleConn stateMVar (sock, sockAddr) = do
  state <- takeMVar stateMVar
  let (card, state') = drawCard state
  putMVar stateMVar state'
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  putStrLn ("Accepted connection from " ++ show sockAddr)
  hPutStr h ("card is: " ++ show card)
  hClose h


main :: IO ()
main = do
  -- Initialize shared server state
  rnd <- getStdGen
  sharedSt <- newMVar $ ServerState rnd []
  sock <- socket AF_INET Stream 0
  -- make socket immediately reusable - eases debugging.
  setSocketOption sock ReuseAddr 1
  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  -- allow a maximum of 2 outstanding connections (TODO need more??)
  listen sock 2
  forever $ accept sock >>= forkIO . handleConn sharedSt
