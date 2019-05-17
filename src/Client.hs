{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent   (forkIO)
import           Data.Aeson           (decode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS (readFile)
import           Data.Text            as T (Text, concat, unpack)
import           Network.WebSockets   (ClientApp, receiveData, sendBinaryData,
                                       sendClose, sendTextData)
import           Wuss                 (runSecureClient)

import Types

host = "stream.watsonplatform.net"
uri accessToken = "/speech-to-text/api/v1/recognize"
    <> "?access_token=" <> accessToken
    <> "&model=es-ES_BroadbandModel"

run :: FilePath -> FilePath -> IO ()
run token wav = filter (/= '\n') <$> readFile token
    >>= (\accessToken -> runSecureClient host 443 (uri accessToken) $ app wav)

app :: FilePath -> ClientApp ()
app wav conn = do
    putStrLn "-- Connected"

    sendTextData conn $ encodeToLazyText startRecognitionReq

    -- Send audio data
    _ <- forkIO $ do
        raw <- LBS.readFile wav
        sendBinaryData conn raw
        sendTextData conn $ encodeToLazyText stopRecognitionReq

    -- Recive answers
    let loop = do
            rawResponse <- receiveData conn
            let mResult = decode rawResponse :: Maybe RecognitionResults
            case mResult of
                Just result -> putStrLn $ unpack $ prettyResult result
                Nothing     -> putStrLn $ "-- " <> show rawResponse
            loop
    loop

    -- FIXME: When to close the connection?
    sendClose conn ("" :: Text)

prettyResult :: RecognitionResults -> Text
prettyResult (RecognitionResults rs) = T.concat $ transcript <$> concatMap alternatives rs
