module Utils where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as C

port :: Integer
port = 7777

workingDirectory = "./working_folder"

{-surnameCurrent :: BSL.ByteString-}
surnameCurrent = BSL.toStrict $ C.pack "Seroka"
