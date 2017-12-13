{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lastfm.Photos
        (
         artistPhotoPage,
         parsePage
        )
        where

import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS
import qualified Data.Aeson as A
import GHC.Generics
import Data.Monoid
import Data.Maybe(mapMaybe)

newtype ImagesPageUrl = ImagesPageUrl { imagesUrl :: T.Text } deriving (Eq, Show, Generic, A.ToJSON)

artistPhotoPage :: T.Text -> ImagesPageUrl
artistPhotoPage artist = ImagesPageUrl $ "https://www.last.fm/music/" <> artist' <> "/+images"
    where artist' = T.replace " " "+" artist

data ArtistPhoto = ArtistPhoto {
                       thumb :: T.Text,
                       full :: T.Text
                   } deriving (Eq, Show, Generic, A.ToJSON)

data ParsePageResult = ParsePageResult {
                           photoList :: [ArtistPhoto],
                           nextPage :: Maybe ImagesPageUrl
                       } deriving (Eq, Show, Generic, A.ToJSON)

parsePage :: T.Text -> ParsePageResult
parsePage str = ParsePageResult { .. }
    where photoList = mapMaybe getArtistPhoto $ filter isArtistPhoto $ TS.parseTags str
          nextPage = Nothing

isArtistPhoto :: TS.Tag T.Text -> Bool
isArtistPhoto (TS.TagOpen "img" attrs) = ("class", "image-list-image") `elem` attrs
isArtistPhoto _ = False

getArtistPhoto :: TS.Tag T.Text -> Maybe ArtistPhoto
getArtistPhoto (TS.TagOpen _ attrs) | Just src <- lookup "src" attrs = Just $ ArtistPhoto src $ T.replace "avatar170s" "770x0" src
                                    | otherwise = Nothing
getArtistPhoto _ = error "Unexpected element type"

_silenceUnused :: [a]
_silenceUnused = [undefined thumb, undefined full, undefined imagesUrl]
