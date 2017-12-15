{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE Arrows #-}

module Lastfm.Recommended
        (
         recommendedArtistsPage,
         parseArtistsPage
        )
        where

import qualified Data.Text as T
import qualified Data.Aeson as A
import GHC.Generics
import Text.XML.HXT.Core
import Text.HandsomeSoup

import Lastfm.FetchPageUrl

recommendedArtistsPage :: T.Text -> FetchPageUrl
recommendedArtistsPage = const FetchPageUrl { url = "https://www.last.fm/home/artists", requiresLogin = True }

data Recommended = Recommended {
                       name :: T.Text,
                       similarTo :: [T.Text]
                   } deriving (Eq, Show, Generic, A.ToJSON)

newtype RecommendedArtists = RecommendedArtists { recommendedArtists :: [Recommended] } deriving (Eq, Show, Generic, A.ToJSON)

parseArtistsPage :: T.Text -> RecommendedArtists
parseArtistsPage = RecommendedArtists . runLA recArtistsArrow . T.unpack

recArtistsArrow :: ArrowXml a => a String Recommended
recArtistsArrow = hread >>> css ".recs-feed-item--artist" >>> proc x -> do
    name <- css ".link-block-target" /> getText >>> arr T.pack -< x
    similarTo <- listA $ css ".context >> a" /> getText >>> arr T.pack -< x
    returnA -< Recommended { .. }
