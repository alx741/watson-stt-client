module Client where

import Data.Text          (Text)
import Network.WebSockets (ClientApp, receiveData, runClient, sendClose)
import Wuss (runSecureClient)

host = "stream.watsonplatform.net"
uri accessToken = "/speech-to-text/api/v1/recognize"
    <> "?access_token=" <> accessToken
    <> "&model=es-ES_BroadbandModel"

run :: FilePath -> IO ()
run = readFile >>= (\accessToken -> runClient host 80 (uri accessToken) app)

app :: ClientApp ()
app conn = do
    putStrLn "connected"
