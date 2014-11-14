module Utils where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as C

port :: Integer
port = 7777

surnameCurrent :: Data.ByteString.Internal.ByteString
surnameCurrent = BSL.toStrict $ C.pack "Seroka"
