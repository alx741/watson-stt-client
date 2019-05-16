module Client where

import Data.Text          (Text)
import Network.WebSockets (ClientApp, receiveData, sendClose)
import Wuss (runSecureClient)

host = "stream.watsonplatform.net"
uri accessToken = "/speech-to-text/api/v1/recognize"
    <> "?access_token=" <> accessToken
    <> "&model=es-ES_BroadbandModel"

run :: FilePath -> IO ()
run fp = filter (/= '\n') <$> readFile fp
    >>= (\accessToken -> runSecureClient host 443 (uri accessToken) app)

app :: ClientApp ()
app conn = do
    putStrLn "connected"

    -- Recive answers
    let loop = do
            rawResponse <- receiveData conn :: IO Text
            print rawResponse
            -- let mTranscriptRes = ((decode rawResponse) :: Maybe TranscriptResponse)
            -- let mTranscript = mTranscriptRes >>=  prettyTranscript
            -- case mTranscript of
            --     Just transcript -> putStrLn $ unpack transcript
            --     Nothing -> pure ()
            loop
    loop
    -- sendClose conn (pack "Bye!" :: Text)