module Main where

import Control.Monad (forever)
import Conduit
import Data.Text (Text)
import Client

config :: ClientConfig
config = ClientConfig "es-ES_BroadbandModel" 0.1

main :: IO ()
main = runStreamWithSpeech config "access_token" sink

sink :: ConduitT Text Void IO ()
sink = awaitForever $ liftIO . print

-- arecord -f S16_LE -c1 -r 16000 -t raw | stack exec client
