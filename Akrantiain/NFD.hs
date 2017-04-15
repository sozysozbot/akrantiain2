{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.NFD
(nfd
) where
import Data.Text.Normalize
import Data.Text

nfd :: String -> String
nfd = unpack . normalize NFD . pack
