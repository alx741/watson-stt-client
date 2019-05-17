{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe           (fromMaybe)
import Data.Text            (Text)
import GHC.Generics         (Generic)

startRecognitionReq :: RecognitionRequest
startRecognitionReq = RecognitionRequest "start" $ Just "audio/wav"

stopRecognitionReq :: RecognitionRequest
stopRecognitionReq = RecognitionRequest "stop" Nothing

data RecognitionRequest = RecognitionRequest
    { action      :: Text
    , contentType :: Maybe Text
    } deriving (Show, Eq, Generic)

data RecognitionResults = RecognitionResults
    { results :: [RecognitionResult]
    } deriving (Show, Eq, Generic)

data RecognitionResult = RecognitionResult
    { alternatives :: [RecognitionAlternative]
    , final        :: Bool
    } deriving (Show, Eq, Generic)

data RecognitionAlternative = RecognitionAlternative
    { confidence :: Float
    , transcript :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON RecognitionAlternative
instance FromJSON RecognitionResult
instance FromJSON RecognitionResults

instance ToJSON RecognitionRequest where
    toJSON (RecognitionRequest action mContentType) =
        object
            [ "action" .= action
            , "content-type" .= fromMaybe "" mContentType
            ]
