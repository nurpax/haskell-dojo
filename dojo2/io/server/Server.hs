{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Exception (try)
import Control.Monad (forever)
import Network
import System.IO
import System.Random
import Text.JSON (encode, decode, resultToEither)

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

drawNCards :: Int -> ServerState -> ([Card], ServerState)
drawNCards nCards st =
  loop st [] 0
  where
    loop st cs n
      | n < nCards =
        let (card, st') = drawCard st in
        loop st' (card:cs) (n+1)
      | otherwise = (cs, st)


respondToClient :: MVar ServerState -> Handle -> ClientReq -> IO ()
respondToClient stateMVar h clientReq = loop
  where
    loop = do
      nCards <- fmap readInt (hGetLine h)
      case nCards of
        Left err ->
          putStrLn $ "failed to decode client nCards response: " ++ err
        Right n ->
          sendCards n >> loop

    sendCards n = do
      state <- takeMVar stateMVar
      let (cards, state') = drawNCards n state
      putMVar stateMVar state'
      putStrLn $ "player " ++ crPlayerName clientReq ++ " connected"
      hPutStrLn h $ encode cards

handleConn :: MVar ServerState -> (Handle, HostName, PortNumber) -> IO ()
handleConn stateMVar (h, hostname, port) = do
  hSetBuffering h LineBuffering
  putStrLn ("Accepted connection from " ++ hostname ++ " port " ++ show port)
  -- Ask the client for its player struct
  line <- try (hGetLine h) :: IO (Either IOError String)
  putStrLn $ "client -> server" ++ show line
  either (\e -> putStrLn $ "Ignoring error: " ++ show e)
         (respondToClient stateMVar h)
         (decodeLine line)
  hClose h
  where
    decodeLine (Left e) = Left . show $ e
    decodeLine (Right line) = decodeClientReq line


main :: IO ()
main = do
  rnd <- getStdGen
  sharedSt <- newMVar $ ServerState rnd []
  sock <- listenOn (PortNumber 4242)
  forever $ accept sock >>= forkIO . handleConn sharedSt
