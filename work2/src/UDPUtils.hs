{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
module UDPUtils where
import           Control.Lens

import           Control.Applicative
import           Control.Monad
import qualified Data.Set                     as Set

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString            hiding (filter, head)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Time.Clock.POSIX

import           Network.Info
import           System.Console.ANSI          (clearScreen, setCursorPosition)

{-data Message = Message { _ip        :: !IPv4-}
                       {-, _fileCount :: !Word-}
                       {-, _timeStamp :: !POSIXTime-}
                       {-, _surname   :: !ByteString-}
                       {-}-}

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

printAtClearScreen :: IO ()
printAtClearScreen = clearScreen >> setCursorPosition 0 0
