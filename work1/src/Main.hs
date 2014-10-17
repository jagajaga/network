module Main where
import Network.Socket hiding (recvFrom)
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

broadcastAddress = head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just $ show port)

port = 7777 

childProcess = withSocketsDo $ do
        a <- broadcastAddress
        socket <- socket (addrFamily a) Datagram defaultProtocol
        setSocketOption socket Broadcast 1
        forever $ do
            msg <- liftM (\a -> show $ filter (\x -> name x == "wlp3s0") a) getNetworkInterfaces
            sendAllTo socket (BSL.toStrict $ C.pack msg) (addrAddress a)
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
            putStrLn "LALALAL"
            putStrLn $ (show hostAddr) ++ ": " 
            C.putStrLn (BSL.fromStrict $ msg)
            hosts <- readIORef hostsRef
            return ()
            {-when (notElem hostAddr hosts) $ modifyIORef hostsRef (hostAddr:)-}
            {-sendToAll s (map toUpper msg) $ hosts-}
    sClose s

main = do
    childTread <- forkIO childProcess
    parentProcess

