module Main where

import Client

main :: IO ()
main = run "access_token"

-- arecord -f S16_LE -c1 -r 16000 -t raw | stack exec client
