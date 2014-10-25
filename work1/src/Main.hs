{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
module Main where
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM

import           Control.Lens
import qualified Data.Foldable                as F
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString              hiding (filter, head)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Char8   as C
import           Data.Char                    (toUpper)

import           Network.Info
import           Network.Socket               hiding (recv, recvFrom)
import           Network.Socket.ByteString

import           Data.Time
import           Data.Time.Clock.POSIX


import           Data.Word
import           System.Console.ANSI          (clearScreen, setCursorPosition)
import Data.Word

data Message = Message { _ip      :: !IPv4
                       , _macAddr :: !MAC
                       , _surname :: !ByteString
                       }
makeLenses ''Message

instance Binary IPv4 where
    put (IPv4 b0) =  put $ byteSwap32 b0
    get = IPv4 <$> get

instance Binary MAC where
    put (MAC b0 b1 b2 b3 b4 b5) = forM_ [b0, b1, b2, b3, b4, b5] put
    get = MAC <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary Message where
    put (Message a b c) = put a >> put b >> putByteString c >> put '\0'
    get = do
        (IPv4 a) <- get
        b <- get
        c <- BSL.toStrict <$> getLazyByteStringNul
        return $ Message (IPv4 $ byteSwap32 a) b c

instance Show Message where
    show a = (show $ _ip a) ++ " | " ++ (show $ _macAddr a) ++ " | " ++ (show $ _surname a)

data RecievedData = RecievedData { _message  :: Message
                                 , _lostPkgs :: Set.Set POSIXTime
                                 }

makeLenses ''RecievedData

instance Show RecievedData where
    show a = (show $ a^.message) ++ " | Lost pkgs: " ++ (show $ (\a -> a - 1) $ Set.size $ a^.lostPkgs)

port = 7777
surnameCurrent = BSL.toStrict $ C.pack "Seroka"

broadcastAddress = head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just $ show port)

clientProcess = withSocketsDo $ do
        a <- broadcastAddress
        socket <- socket (addrFamily a) Datagram defaultProtocol
        setSocketOption socket Broadcast 1
        forever $ do
            (ipCurrent, macAddrCurrent) <- liftM ((ipv4 &&& mac) . head . filter (\x -> (Prelude.take 3 $ name x) == "wlp")) getNetworkInterfaces
            let msg = Message ipCurrent macAddrCurrent surnameCurrent
            sendAllTo socket (BSL.toStrict $ encode msg) (addrAddress a)
            threadDelay (10^6 * 2)
        return()

initSocket :: IO Socket
initSocket = do
    addr <- head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just $ show port)
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    return sock

serverProcess chan = withSocketsDo $ do
    s <- initSocket
    forever $ do
            (msg, hostAddr) <- recvFrom s 1024
            let mmsg = decode $ BSL.fromStrict msg
            atomically $ writeTChan chan mmsg
    sClose s

printAtClearScreen = clearScreen >> setCursorPosition 0 0

recieverProcess = loop where
    loop chan clients = do
        chanState <- atomically $ isEmptyTChan chan
        timeCurrent <- getPOSIXTime
        if chanState
        then
            do
                displayResults clients timeCurrent
        else
            do
                a@(Message mIp _ _) <- atomically $ readTChan chan
                let recievedData = RecievedData a (Set.singleton timeCurrent)
                {-let recievedData = case Map.lookup mIp clients of-}
                     {-Just value -> RecievedData a (value^.lostPkgs) -}
                     {-_ -> RecievedData a (Set.singleton timeCurrent)-}
                let newClients = Map.insert (mIp) recievedData clients
                displayResults newClients timeCurrent
        where
            displayResults clients timeCurrent = do
                printAtClearScreen
                let filteredMap = Map.filter (\value -> (value^.lostPkgs & Set.size) < 10) $ Map.map (`f` timeCurrent) clients
                if Map.null filteredMap then print "No clients :(" else F.traverse_ print filteredMap
                threadDelay (10 ^ 5::Int)
                loop chan filteredMap
            f value timeCurrent = value & lostPkgs .~ (Set.union (value^.lostPkgs) (if addLost then Set.singleton timeCurrent else Set.empty))
                where addLost = (round $ timeCurrent - (value^.lostPkgs & Set.findMax)) > 2

main = do
    clientThread <- forkIO clientProcess
    a <- newTChanIO
    serverThread <- forkIO $ serverProcess a
    recieverProcess a Map.empty
