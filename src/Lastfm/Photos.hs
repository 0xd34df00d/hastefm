{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lastfm.Photos
( artistPhotoPage
, parsePage
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Aeson as A
import GHC.Generics
import Data.Maybe
import Text.HTML.DOM
import Text.XML.Cursor
import Text.XML.Selector.TH

newtype ImagesPageUrl = ImagesPageUrl { imagesUrl :: T.Text } deriving (Eq, Show, Generic, A.ToJSON)

artistPhotoPage :: T.Text -> ImagesPageUrl
artistPhotoPage artist = ImagesPageUrl $ "https://www.last.fm/music/" <> artist' <> "/+images"
  where artist' = T.replace " " "+" artist

data ArtistPhoto = ArtistPhoto
  { thumb :: T.Text
  , full :: T.Text
  } deriving (Eq, Show, Generic, A.ToJSON)

data ParsePageResult = ParsePageResult
  { photoList :: [ArtistPhoto]
  , nextPage :: Maybe ImagesPageUrl
  } deriving (Eq, Show, Generic, A.ToJSON)

parsePage :: LBS.ByteString -> ParsePageResult
parsePage page = ParsePageResult { .. }
  where
    root = fromDocument $ parseLBS page
    srcs = mapMaybe (listToMaybe . attribute "src") $ [jq|.image-list-item > img|] `queryT` root
    photoList = photoFromThumb <$> srcs
    nextPage = Nothing

photoFromThumb :: T.Text -> ArtistPhoto
photoFromThumb thumb = ArtistPhoto { .. }
  where full = T.replace "avatar170s" "770x0" thumb
