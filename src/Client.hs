{-# LANGUAGE OverloadedStrings #-}

module Client
    ( Pipeline
    , ClientConfig(..)
    , runStreamWithSpeech
    ) where

import           Conduit
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad        (forever)
import           Data.Aeson           (ToJSON, decode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS (hGet)
import           Data.Text            as T (Text)
import           Network.WebSockets   (ClientApp, receiveData, sendBinaryData,
                                       sendClose, sendTextData)
import           System.IO            (hPrint, stderr, stdin)
import           Wuss                 (runSecureClient)

import Types


-- | Conduit pipeline that takes /speech text/ as input
type Pipeline = ConduitT Text Void IO ()

data ClientConfig = ClientConfig
    { model                  :: String -- ^ e.g. "es-ES_BroadbandModel
    , secondsPerTransmission :: Float -- ^ e.g. 0.1 (a good default)
    } deriving (Show)


runStreamWithSpeech
 :: ClientConfig
 -> String -- ^ IBM Watson access token
 -> Pipeline -- ^ Conduit pipeline to run
 -> IO ()
runStreamWithSpeech (ClientConfig m s) token pipeline =
    runSecureClient host 443 uri (app s pipeline)
    where
        host = "stream.watsonplatform.net"
        uri = "/speech-to-text/api/v1/recognize"
            <> "?access_token=" <> token
            <> "&model=" <> m

app :: Float -> Pipeline -> ClientApp ()
app seconds pipeline conn = do
    putStrLn "-- Connected"
    startRequest
    _ <- forkIO $ forever $ sendStdinRaw $ floor $ 16.0 * 16000.0 * seconds
    _ <- runConduit $ speechSource .| pipeline
    sendClose conn ("" :: Text)
    where
        speechSource :: ConduitT () Text IO ()
        speechSource = forever $ yieldM receiveTranscripts

        sendStdinRaw :: Int -> IO ()
        sendStdinRaw bytes = do
            raw <- LBS.hGet stdin bytes
            sendBinaryData conn raw
            stopRequest

        receiveTranscripts :: IO Text
        receiveTranscripts = do
            rawResponse <- receiveData conn
            case decode rawResponse :: Maybe RecognitionResults of
                Just result -> pure $ prettyResult result
                Nothing ->
                    case decode rawResponse :: Maybe ErrorResponse of
                        -- Restart upon error
                        Just (ErrorResponse e) -> do
                            startRequest
                            hPrint stderr e
                            receiveTranscripts
                        Nothing -> receiveTranscripts

        startRequest :: IO ()
        startRequest = request startRecognitionReq >> threadDelay 50000

        stopRequest :: IO ()
        stopRequest = request stopRecognitionReq

        request :: ToJSON a => a -> IO ()
        request = sendTextData conn . encodeToLazyText
