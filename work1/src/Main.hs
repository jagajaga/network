module Main where
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString            hiding (filter, head)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (toUpper)
import           Data.IORef
import           Network.Info
import           Network.Socket             hiding (recv, recvFrom)
import           Network.Socket.ByteString

import           Data.Time
import           Data.Time.Clock.POSIX

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Word

data Message = Message { ip      :: !IPv4
                       , macAddr :: !MAC
                       , surname :: !ByteString
                       } deriving (Show)

instance Binary IPv4 where
    put (IPv4 b0) = put b0
    get = IPv4 <$> get

instance Binary MAC where
    put (MAC b0 b1 b2 b3 b4 b5) = forM_ [b0, b1, b2, b3, b4, b5] put
    get = MAC <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary Message where
    put (Message a b c) = put a >> put b >> putByteString c >> put '\0'
    get = do
        a <- get
        b <- get
        c <- BSL.toStrict <$> getLazyByteStringNul
        return $ Message a b c

port = 7777
surnameCurrent = BSL.toStrict $ C.pack "Seroka"

broadcastAddress = head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just $ show port)

childProcess = withSocketsDo $ do
        a <- broadcastAddress
        socket <- socket (addrFamily a) Datagram defaultProtocol
        setSocketOption socket Broadcast 1
        forever $ do
            (ipCurrent, macAddrCurrent) <- liftM ((ipv4 &&& mac) . head . filter (\x -> name x == "wlp3s0")) getNetworkInterfaces
            let msg = Message ipCurrent macAddrCurrent surnameCurrent
            sendAllTo socket (BSL.toStrict $ encode msg) (addrAddress a)
        return()

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
    forever $ do
            (msg, hostAddr) <- recvFrom s 1024
            let mmsg@(Message a b c) = decode $ BSL.fromStrict msg
            print mmsg
            return ()
    sClose s

main = do
    childTread <- forkIO childProcess
    parentProcess

---TODO drop take ip(4)mac(6)name\0
---3 threads
---async queue
