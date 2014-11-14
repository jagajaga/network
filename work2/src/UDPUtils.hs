{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
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
import           Data.Time.Clock

import           Network.Info
import           System.Console.ANSI          (clearScreen, setCursorPosition)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as C

data Message = Message { _ip        :: !IPv4
                       , _fileCount :: !Word
                       , _timeStamp :: !POSIXTime
                       , _surname   :: !ByteString
                       }

makeLenses ''Message

instance Binary IPv4 where
    put (IPv4 b0) =  put $ byteSwap32 b0
    get = IPv4 <$> get

instance Binary MAC where
    put (MAC b0 b1 b2 b3 b4 b5) = forM_ [b0, b1, b2, b3, b4, b5] put
    get = MAC <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary POSIXTime where
    put diff = put $ C.pack (show (nominalDiffToMilli diff)) 
        where nominalDiffToMilli i = round (i * 1000)
    get = do
        a <- get
        return $ realToFrac . (/ (1000 :: Double)) $ fromIntegral $ (read $ C.unpack a :: Integer) 

instance Binary Message where
    put (Message a b c d) = put a >> put b >> put c >> putByteString d >> put '\0'
    get = do
        (IPv4 a) <- get
        b <- get
        c <- get
        d <- BSL.toStrict <$> getLazyByteStringNul
        return $ Message (IPv4 $ byteSwap32 a) b c d

instance Show Message where
    show a = (show $ _ip a) ++ " | " ++ (show $ _fileCount a) ++ " | " ++ (show $ _surname a) ++ " | " ++ (show $ _timeStamp a)

data RecievedData = RecievedData { _message  :: Message
                                 , _lostPkgs :: Set.Set POSIXTime
                                 }

makeLenses ''RecievedData

instance Show RecievedData where
    show a = (show $ a^.message) ++ " | Lost pkgs: " ++ (show $ (\a -> a - 1) $ Set.size $ a^.lostPkgs)

printAtClearScreen :: IO ()
printAtClearScreen = clearScreen >> setCursorPosition 0 0
