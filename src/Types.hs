{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Maybe           (fromMaybe)
import Data.Text            (Text)
import GHC.Generics         (Generic)

startRecognitionReq :: RecognitionRequest
startRecognitionReq = RecognitionRequest "start" $ Just "audio/l16;rate=16000"

stopRecognitionReq :: RecognitionRequest
stopRecognitionReq = RecognitionRequest "stop" Nothing

data StateResponse = StateResponse
    { state :: Text
    } deriving (Show, Eq, Generic)

data ErrorResponse = ErrorResponse
    { error :: Text
    } deriving (Show, Eq, Generic)

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
instance FromJSON StateResponse
instance FromJSON ErrorResponse

instance ToJSON RecognitionRequest where
    toJSON (RecognitionRequest reqAction mContentType) =
        object
            [ "action" .= reqAction
            , "content-type" .= fromMaybe "" mContentType
            ]
