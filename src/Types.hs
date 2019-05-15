{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson

data RecognitionRequest = RecognitionRequest
    { action      :: RequestAction
    , contentType :: Maybe String
    } deriving (Show, Eq, Generic)

data RequestAction
    = Start
    | Stop
    deriving (Show, Eq, Generic)

instance ToJSON RequestAction
instance ToJSON RecognitionRequest
