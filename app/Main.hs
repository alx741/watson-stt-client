module Main where

import Client

config :: ClientConfig
config = ClientConfig "es-ES_BroadbandModel" 0.1

main :: IO ()
main = run config "access_token"

-- arecord -f S16_LE -c1 -r 16000 -t raw | stack exec client
