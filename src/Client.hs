{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent   (forkIO)
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

host :: String
host = "stream.watsonplatform.net"

uri :: String -> String
uri accessToken = "/speech-to-text/api/v1/recognize"
    <> "?access_token=" <> accessToken
    <> "&model=es-ES_BroadbandModel"

run :: FilePath -> IO ()
run token = filter (/= '\n') <$> readFile token
    >>= (\accessToken -> runSecureClient host 443 (uri accessToken) app)

sendStdinRaw :: Connection -> Int -> IO ()
sendStdinRaw conn bytes = do
    raw <- LBS.hGet stdin bytes
    sendBinaryData conn raw
    sendTextData conn $ encodeToLazyText stopRecognitionReq

app :: ClientApp ()
app conn = do
    putStrLn "-- Connected"

    -- Send audio data
    startRequest conn
    _ <- forkIO $ forever $ sendStdinRaw conn 25600 -- 0.1 seconds at 256kbps (16bit, 16khz)


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
startRequest = request startRecognitionReq

stopRequest :: Connection -> IO ()
stopRequest = request stopRecognitionReq

request :: ToJSON a => a -> Connection -> IO ()
request req conn = sendTextData conn $ encodeToLazyText req
