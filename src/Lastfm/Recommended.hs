{-# LANGUAGE OverloadedStrings #-}

module Lastfm.Recommended
        (
         recommendedArtistsPage
        )
        where

import qualified Data.Text as T

import Lastfm.FetchPageUrl

recommendedArtistsPage :: T.Text -> FetchPageUrl
recommendedArtistsPage = const FetchPageUrl { url = "https://www.last.fm/home/artists", requiresLogin = True }
