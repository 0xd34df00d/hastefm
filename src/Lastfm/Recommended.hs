{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lastfm.Recommended
        (
         recommendedArtistsPage
        )
        where

import qualified Data.Text as T
import qualified Data.Aeson as A
import GHC.Generics

import Lastfm.FetchPageUrl

recommendedArtistsPage :: T.Text -> FetchPageUrl
recommendedArtistsPage = const FetchPageUrl { url = "https://www.last.fm/home/artists", requiresLogin = True }

data Recommended = Recommended {
                       name :: T.Text,
                       similarTo :: [T.Text]
                   } deriving (Eq, Show, Generic, A.ToJSON)

newtype RecommendedArtists = RecommendedArtists { recommendedArtists :: [Recommended] } deriving (Eq, Show, Generic, A.ToJSON)
