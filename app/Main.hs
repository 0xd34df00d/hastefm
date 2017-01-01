{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import qualified Snap.Http.Server.Config as S
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import Data.Either
import Lastfm.Photos

main :: IO ()
main = S.httpServe (S.setPort 12000 S.defaultConfig) site

site :: S.Snap ()
site = S.route [("photos/pageurl", pageUrlHandler),
                ("photos/parsepage", parsePageHandler)]

pageUrlHandler :: S.Snap ()
pageUrlHandler = do
    artist <- S.getPostParam "artist"
    maybe (S.writeBS "must specify the artist") S.writeLBS $ runOnText artistPhotoPage <$> artist

parsePageHandler :: S.Snap ()
parsePageHandler = undefined

runOnText :: A.ToJSON a => (T.Text -> a) -> BS.ByteString -> BSL.ByteString
runOnText f = either (BSL.pack . show) (A.encode . f) . T.decodeUtf8'
