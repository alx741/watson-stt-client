{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Maybe   (fromMaybe)
import Data.Text    as T (Text, concat)
import GHC.Generics (Generic)

startRecognitionReq :: RecognitionRequest
startRecognitionReq = RecognitionRequest "start" $ Just "audio/l16;rate=16000"

stopRecognitionReq :: RecognitionRequest
stopRecognitionReq = RecognitionRequest "stop" Nothing

prettyResult :: RecognitionResults -> Text
prettyResult (RecognitionResults rs) = T.concat $ transcript <$> concatMap alternatives rs

newtype AccessToken = AccessToken String deriving (Show, Eq, Generic)

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

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \v -> AccessToken
        <$> v .: "access_token"

instance ToJSON RecognitionRequest where
    toJSON (RecognitionRequest reqAction mContentType) =
        object
            [ "action" .= reqAction
            , "content-type" .= fromMaybe "" mContentType
            ]
