{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent   (forkIO)
import           Control.Monad        (forever)
import           Data.Aeson           (decode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS (hGet, readFile)
import           Data.Text            as T (Text, concat, unpack)

import Control.Concurrent (threadDelay)
import Network.WebSockets (ClientApp, Connection, receiveData, sendBinaryData,
                           sendClose, sendTextData)
import System.IO          (stdin)
import Wuss               (runSecureClient)

import Types

host = "stream.watsonplatform.net"
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
    sendTextData conn $ encodeToLazyText startRecognitionReq
    _ <- forkIO $ forever $ sendStdinRaw conn 25600 -- 0.1 seconds at 256kbps (16bit, 16khz)

    -- Recive answers
    let loop = do
            rawResponse <- receiveData conn
            let mResult = decode rawResponse :: Maybe RecognitionResults
            -- print rawResponse
            case mResult of
                Just result -> putStr $ unpack $ prettyResult result
                Nothing -> putStrLn $ "-- " <> show rawResponse
            loop
    loop

    -- FIXME: When to close the connection?
    sendClose conn ("" :: Text)

prettyResult :: RecognitionResults -> Text
prettyResult (RecognitionResults rs) = T.concat $ transcript <$> concatMap alternatives rs
