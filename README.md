# watson-stt-client

## As a stand alone executable (mainly for trying it out)

1. Put your `api key` in a `api_key` file and then run the `get_access_token.sh`
script

    $ get_access_token.sh

2. Stream audio data (raw 16bit, 16khz) into the executable

    arecord -f S16_LE -c1 -r 16000 -t raw | stack exec client
    cat someaudiofile.wav | stack exec client

## As a library (intended use)

1. Create a configuration with the name of the model you wish to use and the
seconds of audio data per chunk you want to send (*0.25* seems a sensible
default, play with it)

```haskell
config :: ClientConfig
config = ClientConfig "es-ES_BroadbandModel" 0.25
```

2. Define an application `Connection -> IO ()` using the `receiveTranscripts`
function

`app/Main.hs` provides an example that uses a Conduit pipeline as application:

```haskell
module Main where

import Conduit
import Control.Monad (forever)
import Data.Text     (Text)
import SttClient

main :: IO ()
main = do
    token <- filter (/= '\n') <$> readFile "./access_token"
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
```
