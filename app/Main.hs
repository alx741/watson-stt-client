module Main where

import Conduit
import Control.Monad (forever)
import Data.Text     (Text)
import SttClient

-- arecord -f S16_LE -c1 -r 16000 -t raw | stack exec client

main :: IO ()
main = do
    token <- filter (/= '\n') <$> readFile "access_token"
    runWithSpeech config token client

config :: ClientConfig
config = ClientConfig "es-ES_BroadbandModel" 0.25

client :: Connection -> IO ()
client conn = do
    runConduit $ speechSource conn .| sink

speechSource :: Connection -> ConduitT () Text IO ()
speechSource conn = forever $ yieldM $ receiveTranscripts conn

sink :: ConduitT Text Void IO ()
sink = awaitForever $ liftIO . print
