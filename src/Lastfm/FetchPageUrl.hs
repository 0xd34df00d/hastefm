{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lastfm.FetchPageUrl where

import qualified Data.Text as T
import qualified Data.Aeson as A
import GHC.Generics

data FetchPageUrl = FetchPageUrl {
                        url :: T.Text,
                        requiresLogin :: Bool
                    } deriving (Eq, Show, Generic, A.ToJSON)
