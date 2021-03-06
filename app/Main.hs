{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import qualified Snap.Util.FileUploads as S
import qualified System.IO.Streams as IS(read)
import Control.Arrow
import Control.Monad.Loops

import qualified Lastfm.Photos as LP
import qualified Lastfm.Recommended as LR

main :: IO ()
main = do
  cfg <- S.commandLineConfig (S.setPort 12000 S.emptyConfig)
  S.httpServe cfg site

site :: S.Snap ()
site = S.route [("artist/photos/pageurl", photosPageUrlHandler),
                ("artist/photos/parsepage", parsePageHandler $ pure . LP.parsePage),
                ("recommended/artists/pageurl", recArtistsPageUrlHandler),
                ("recommended/artists/parsepage", parsePageHandler $ (show +++ LR.parseArtistsPage) . T.decodeUtf8' . BSL.toStrict)]

photosPageUrlHandler :: S.Snap ()
photosPageUrlHandler = do
  artist <- S.getPostParam "artist"
  maybe (S.writeBS "must specify the artist") S.writeLBS $ runOnText LP.artistPhotoPage <$> artist

recArtistsPageUrlHandler :: S.Snap ()
recArtistsPageUrlHandler = do
  username <- S.getPostParam "self"
  maybe (S.writeBS "must specify self username") S.writeLBS $ runOnText LR.recommendedArtistsPage <$> username

parsePageHandler :: A.ToJSON a => (BSL.ByteString -> Either String a) -> S.Snap ()
parsePageHandler parser = do
  files <- S.handleMultipart S.defaultUploadPolicy $ const $ unfoldM . IS.read
  case files of
    [file] -> S.writeLBS $ BSL.pack ||| A.encode $ parser $ BSL.fromChunks file
    _ -> S.writeBS "must specify the contents"

runOnText :: A.ToJSON a => (T.Text -> a) -> BS.ByteString -> BSL.ByteString
runOnText f = either (BSL.pack . show) (A.encode . f) . T.decodeUtf8'
