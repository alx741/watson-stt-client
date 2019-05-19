{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad        (forever)
import           Data.Aeson           (ToJSON, decode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS (hGet)
import           Data.Text            as T (Text, unpack)

import Network.WebSockets (ClientApp, Connection, receiveData, sendBinaryData,
                           sendClose, sendTextData)
import System.IO          (hFlush, hPrint, stderr, stdin, stdout)
import Wuss               (runSecureClient)

import Types

data ClientConfig = ClientConfig
    { model                  :: String -- ^ e.g. "es-ES_BroadbandModel
    , secondsPerTransmission :: Float -- ^ e.g. 0.1 (a good default)
    } deriving (Show)

host :: String
host = "stream.watsonplatform.net"

uri :: String -> String -> String
uri accessToken m = "/speech-to-text/api/v1/recognize"
    <> "?access_token=" <> accessToken
    <> "&model=" <> m

run :: ClientConfig -> FilePath ->  IO ()
run (ClientConfig m s) token =
    filter (/= '\n') <$> readFile token
    >>= (\accessToken ->
        runSecureClient host 443 (uri accessToken m) (app s))

sendStdinRaw :: Connection -> Int -> IO ()
sendStdinRaw conn bytes = do
    raw <- LBS.hGet stdin bytes
    sendBinaryData conn raw
    sendTextData conn $ encodeToLazyText stopRecognitionReq

app :: Float -> ClientApp ()
app seconds conn = do
    putStrLn "-- Connected"

    -- Send audio data
    startRequest conn
    _ <- forkIO $ forever $ sendStdinRaw conn $ floor $ 16.0 * 16000.0 * seconds

    -- Recive answers
    _ <- forever $ do
            rawResponse <- receiveData conn

            case decode rawResponse :: Maybe RecognitionResults of
                Just result -> do
                    putStr $ unpack $ prettyResult result
                    hFlush stdout
                Nothing -> pure ()

            case decode rawResponse :: Maybe ErrorResponse of
                -- Restart upon error
                Just (ErrorResponse e) -> startRequest conn >> hPrint stderr e
                Nothing -> pure ()

    -- FIXME: When to close the connection?
    sendClose conn ("" :: Text)

startRequest :: Connection -> IO ()
startRequest conn = request startRecognitionReq conn >> threadDelay 50000

stopRequest :: Connection -> IO ()
stopRequest = request stopRecognitionReq

request :: ToJSON a => a -> Connection -> IO ()
request req conn = sendTextData conn $ encodeToLazyText req
