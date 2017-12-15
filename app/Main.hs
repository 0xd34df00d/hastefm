{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import qualified Snap.Util.FileUploads as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified System.IO.Streams as IS(read)
import Data.Monoid

import Lastfm.Photos

main :: IO ()
main = do
    cfg <- S.commandLineConfig (S.setPort 12000 S.emptyConfig)
    S.httpServe cfg site

site :: S.Snap ()
site = S.route [("artist/photos/pageurl", photosPageUrlHandler),
                ("artist/photos/parsepage", photosParsePageHandler)]

photosPageUrlHandler :: S.Snap ()
photosPageUrlHandler = do
    artist <- S.getPostParam "artist"
    maybe (S.writeBS "must specify the artist") S.writeLBS $ runOnText artistPhotoPage <$> artist

photosParsePageHandler :: S.Snap ()
photosParsePageHandler = do
    files <- S.handleMultipart S.defaultUploadPolicy $ const reader
    case files of
        [file] -> S.writeLBS $ runOnText parsePage file
        _ -> S.writeBS "must specify the contents"
    where reader is = do
                maybeStr <- IS.read is
                case maybeStr of
                    Nothing -> pure mempty
                    Just str -> (str <>) <$> reader is

runOnText :: A.ToJSON a => (T.Text -> a) -> BS.ByteString -> BSL.ByteString
runOnText f = either (BSL.pack . show) (A.encode . f) . T.decodeUtf8'
