{-# LANGUAGE OverloadedStrings #-}

module SttClient
    ( Pipeline
    , ClientConfig(..)
    , Connection
    , runWithSpeech
    , receiveTranscripts
    ) where

import           Conduit
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad        (forever)
import           Data.Aeson           (ToJSON, decode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS (hGet)
import           Data.Text            as T (Text)
import           Network.WebSockets   (ClientApp, Connection, receiveData,
                                       sendBinaryData, sendClose, sendTextData)
import           System.IO            (hPrint, stderr, stdin)
import           Wuss                 (runSecureClient)

import Types


-- | Conduit pipeline that takes /speech text/ as input
type Pipeline = ConduitT Text Void IO ()

data ClientConfig = ClientConfig
    { model                  :: String -- ^ e.g. "es-ES_BroadbandModel
    , secondsPerTransmission :: Float  -- ^ e.g. 0.1 (a good default)
    } deriving (Show)


runWithSpeech
 :: ClientConfig
 -> String -- ^ IBM Watson access token
 -> (Connection -> IO ()) -- ^ Application
 -> IO ()
runWithSpeech (ClientConfig m s) token client =
    runSecureClient host 443 uri (app s client)
    where
        host = "stream.watsonplatform.net"
        uri = "/speech-to-text/api/v1/recognize"
            <> "?access_token=" <> token
            <> "&model=" <> m

app :: Float -> ClientApp () -> ClientApp ()
app seconds client conn = do
    putStrLn "-- Connected"
    startRequest conn
    _ <- forkIO $ forever $ sendStdinRaw $ floor $ 16.0 * 16000.0 * seconds
    client conn
    sendClose conn ("" :: Text)
    where
        sendStdinRaw :: Int -> IO ()
        sendStdinRaw bytes = do
            raw <- LBS.hGet stdin bytes
            sendBinaryData conn raw
            stopRequest conn

receiveTranscripts :: Connection -> IO Text
receiveTranscripts conn = do
    rawResponse <- receiveData conn
    case decode rawResponse :: Maybe RecognitionResults of
        Just result -> pure $ prettyResult result
        Nothing ->
            case decode rawResponse :: Maybe ErrorResponse of
                -- Restart upon error
                Just (ErrorResponse e) -> do
                    startRequest conn
                    hPrint stderr e
                    receiveTranscripts conn
                Nothing -> receiveTranscripts conn

startRequest :: Connection -> IO ()
startRequest conn = request conn startRecognitionReq >> threadDelay 50000

stopRequest :: Connection -> IO ()
stopRequest conn = request conn stopRecognitionReq

request :: ToJSON a => Connection -> a -> IO ()
request conn = sendTextData conn . encodeToLazyText
