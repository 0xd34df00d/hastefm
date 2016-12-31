{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lastfm.Photos(
                     artistPhotoPage,
                     parsePage
                    ) where

import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS
import Data.Monoid
import Data.Maybe(mapMaybe)
import Data.List(find)

newtype ImagesPageUrl = ImagesPageUrl T.Text deriving (Eq, Show)

artistPhotoPage :: T.Text -> ImagesPageUrl
artistPhotoPage artist = ImagesPageUrl $ "http://www.last.fm/music/" <> artist' <> "/+images"
    where artist' = T.replace " " "+" artist

data ArtistPhoto = ArtistPhoto {
                       thumb :: T.Text,
                       full :: T.Text
                   } deriving (Eq, Show)

data ParsePageResult = ParsePageResult {
                           photoList :: [ArtistPhoto],
                           nextPage :: Maybe ImagesPageUrl
                       } deriving (Eq, Show)

parsePage :: T.Text -> ParsePageResult
parsePage str = ParsePageResult { .. }
    where photoList = mapMaybe getArtistPhoto $ filter isArtistPhoto $ TS.parseTags str
          nextPage = Nothing

isArtistPhoto :: TS.Tag T.Text -> Bool
isArtistPhoto (TS.TagOpen "img" attrs) | ("class", "image-list-image") `elem` attrs = True
isArtistPhoto _ = False

getArtistPhoto :: TS.Tag T.Text -> Maybe ArtistPhoto
getArtistPhoto (TS.TagOpen _ attrs) | Just (_, src) <- find ((== "src") . fst) attrs = Just $ ArtistPhoto src $ T.replace "avatar170s" "770x0" src
                                    | otherwise = Nothing
