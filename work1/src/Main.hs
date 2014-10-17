{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Network.Socket hiding (recvFrom, recv)
import Control.Concurrent
import Control.Monad (forever, when, liftM)
import Data.Char (toUpper)
import Data.IORef
import Network.Info
import Control.Applicative ((<$>))
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.IO.Class 

import Data.Time
import Data.Time.Clock.POSIX

broadcastAddress = head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just $ show port)

deriving instance Read IPv4
deriving instance Read MAC

data Message = Message { macAddr :: MAC
                       , surname :: String
                       , ip :: IPv4
                       } deriving (Show, Read)

port = 7777 
surnameCurrent = "Seroka"

childProcess = withSocketsDo $ do
        a <- broadcastAddress
        socket <- socket (addrFamily a) Datagram defaultProtocol
        setSocketOption socket Broadcast 1
        forever $ do
            (ipCurrent, macAddrCurrent) <- liftM (\a -> (\a -> (ipv4 a, mac a)) $ (\[a] -> a) $ filter (\x -> name x == "wlp3s0") a) getNetworkInterfaces
            let msg = Message macAddrCurrent surnameCurrent ipCurrent
            sendAllTo socket (BSL.toStrict $ C.pack $ show msg ) (addrAddress a)
        return()

type Host = SockAddr

initSocket :: IO Socket
initSocket = do
    addr <- head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just $ show port)
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    return sock

parentProcess = withSocketsDo $ do
    s <- initSocket
    hostsRef <- newIORef []
    forever $ do
            (msg, hostAddr) <- recvFrom s 1024
            C.putStrLn (BSL.fromStrict $ msg)
            {-let message = read $ C.unpack $ BSL.fromStrict msg :: Message-}
            {-let hostAddr = ip message-}
            {-hosts <- readIORef hostsRef-}
            {-when (notElem hostAddr hosts) $ modifyIORef hostsRef (hostAddr:)-}
            return ()
    sClose s

main = do
    childTread <- forkIO childProcess
    parentProcess

---TODO drop take ip(4)mac(6)name\0
---3 threads
---async queue
