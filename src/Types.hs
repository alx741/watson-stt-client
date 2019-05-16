{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( startRecognitionReq
    , stopRecognitionReq
    )
    where

import Data.Aeson
import Data.Maybe   (fromMaybe)
import Data.Text    (Text)
import GHC.Generics (Generic)

startRecognitionReq :: RecognitionRequest
startRecognitionReq = RecognitionRequest "start" $ Just "audio/wav"

stopRecognitionReq :: RecognitionRequest
stopRecognitionReq = RecognitionRequest "stop" Nothing

data RecognitionRequest = RecognitionRequest
    { action      :: Text
    , contentType :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON RecognitionRequest where
    toJSON (RecognitionRequest action mContentType) =
        object
            [ "action" .= action
            , "content-type" .= fromMaybe "" mContentType
            ]
