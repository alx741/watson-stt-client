{-# LANGUAGE DeriveGeneric #-}

module Types
    ( startRecognitionReq
    )
    where

import GHC.Generics (Generic)
import Data.Aeson

startRecognitionReq :: RecognitionRequest
startRecognitionReq = RecognitionRequest "start" "audio/wav"

data RecognitionRequest = RecognitionRequest
    { action      :: String
    , contentType :: String
    } deriving (Show, Eq, Generic)

instance ToJSON RecognitionRequest
