{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.NFD
(nfd
,nfc
) where
import Prelude hiding (undefined)
import Data.Text.Normalize
import Data.Text

nfd :: String -> String
nfd = unpack . normalize NFD . pack

nfc :: String -> String
nfc = unpack . normalize NFC . pack
