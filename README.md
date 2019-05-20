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

2. Make a `Conduit` pipeline  that does what you actually need

```haskell
type Pipeline = ConduitT Text Void IO ()

-- Just print out the speech
myPipeline :: Pipeline
myPipeline = awaitForever $ liftIO . print
```

3. Run the pipeline with a valid `access token` (which you can get by running
the `get_access_token.sh` script)

```haskell
main :: IO ()
main = do
    token <- filter (/= '\n') <$> readFile "access_token"
    runPipelineWithSpeech config token sink
```
