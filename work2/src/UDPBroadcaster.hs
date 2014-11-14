module UDPBroadcaster where
import           UDPUtils
import           Utils

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM

import           Control.Lens
import qualified Data.Foldable                as F
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set

import           Data.Binary
import qualified Data.ByteString.Lazy         as BSL

import           Data.Time.Clock.POSIX
import           Network.Info
import           Network.Socket               hiding (recv, recvFrom)
import           Network.Socket.ByteString

import System.Directory

broadcastAddress :: IO AddrInfo
broadcastAddress = head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just $ show port)

clientProcess :: IO ()
clientProcess = withSocketsDo $ do -- pass folder as arg
        a <- broadcastAddress
        socket <- socket (addrFamily a) Datagram defaultProtocol
        setSocketOption socket Broadcast 1
        files <- getDirectoryContents workingDirectory
        print files
        forever $ do
            (ipCurrent, macAddrCurrent) <- liftM ((ipv4 &&& mac) . head . filter (\x -> (Prelude.take 3 $ name x) == "wlp")) getNetworkInterfaces
            b <- getPOSIXTime
            let msg = Message ipCurrent (fromIntegral $ (\a -> a - 2) $ length files :: Word) b surnameCurrent
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

serverProcess :: Binary t => TChan t -> IO ()
serverProcess chan = withSocketsDo $ do
    s <- initSocket
    forever $ do
            (msg, hostAddr) <- recvFrom s 1024
            let mmsg = decode $ BSL.fromStrict msg
            {-print mmsg-}
            atomically $ writeTChan chan mmsg
    sClose s

recieverProcess :: TChan Message -> Map.Map IPv4 RecievedData -> IO b
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
                a@(Message mIp _ _ _) <- atomically $ readTChan chan
                let recievedData = RecievedData a (Set.singleton timeCurrent)
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
