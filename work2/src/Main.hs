module Main where
import           Control.Concurrent
import           Control.Concurrent.STM.TChan

import qualified Data.Map                     as Map

import UDPBroadcaster
{-import TCPBroadcaster-}

main :: IO ()
main = do
    clientThread <- forkIO clientProcess
    a <- newTChanIO
    serverThread <- forkIO $ serverProcess a
    recieverProcess a Map.empty


