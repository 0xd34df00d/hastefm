{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lastfm.Photos(
                     artistPhotoPage,
                     parsePage
                    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Taggy.Parser as TG
import qualified Text.Taggy.Types as TG
import qualified Data.Aeson as A
import GHC.Generics
import Data.Monoid
import Data.Maybe(mapMaybe)
import Data.List(find)

newtype ImagesPageUrl = ImagesPageUrl { imagesUrl :: T.Text } deriving (Eq, Show, Generic, A.ToJSON)

artistPhotoPage :: T.Text -> ImagesPageUrl
artistPhotoPage artist = ImagesPageUrl $ "http://www.last.fm/music/" <> artist' <> "/+images"
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
    where photoList = mapMaybe getArtistPhoto $ filter isArtistPhoto $ TG.taggyWith False $ TL.fromStrict str
          nextPage = Nothing

isArtistPhoto :: TG.Tag -> Bool
isArtistPhoto (TG.TagOpen "img" attrs _) | TG.Attribute "class" "image-list-image" `elem` attrs = True
isArtistPhoto _ = False

getArtistPhoto :: TG.Tag -> Maybe ArtistPhoto
getArtistPhoto (TG.TagOpen _ attrs _) | Just src <- TG.attrValue <$> find ((== "src") . TG.attrKey) attrs = Just $ ArtistPhoto src $ T.replace "avatar170s" "770x0" src
                                      | otherwise = Nothing
getArtistPhoto _ = error "Unexpected element type"

_silenceUnused :: [a]
_silenceUnused = [undefined thumb, undefined full, undefined imagesUrl]
