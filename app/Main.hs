module Main where

import Client
import Conduit
import Data.Text (Text)

config :: ClientConfig
config = ClientConfig "es-ES_BroadbandModel" 0.25

main :: IO ()
main = do
    token <- filter (/= '\n') <$> readFile "access_token"
    runPipelineWithSpeech config token sink

sink :: ConduitT Text Void IO ()
sink = awaitForever $ liftIO . print

-- arecord -f S16_LE -c1 -r 16000 -t raw | stack exec client
