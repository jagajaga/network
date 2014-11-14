module Main where
import           Control.Concurrent
import           Control.Concurrent.STM.TChan

import qualified Data.Map                     as Map
import System.Directory

import UDPBroadcaster
import Utils
{-import TCPBroadcaster-}

main :: IO ()
main = do
    createDirectoryIfMissing True workingDirectory
    clientThread <- forkIO clientProcess
    a <- newTChanIO
    serverThread <- forkIO $ serverProcess a
    recieverProcess a Map.empty


