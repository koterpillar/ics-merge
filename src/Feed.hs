{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Feed where

import           Control.Lens

import           Data.ByteString.Lazy    (ByteString)

import           Data.Text               (Text)
import qualified Data.Text               as Text

import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

import           Event

newtype Feed =
  Feed [Event]

makeFeed :: [Event] -> Feed
makeFeed = Feed

feedContent :: Feed -> ByteString
feedContent (Feed events) =
  LazyText.encodeUtf8 $
  LazyText.fromStrict $
  unlinesR $
  [ "BEGIN:VCALENDAR"
  , "VERSION:2.0"
  , "PRODID:-//koterpillar//ics-merge"
  , "CALSCALE:GREGORIAN"
  ] ++
  map evtText events ++ ["END:VCALENDAR", ""]

renumberFeed :: Text -> Feed -> Feed
renumberFeed uidSuffix (Feed events) =
  Feed $ zipWith renumberEvent [1 ..] events
  where
    renumberEvent :: Integer -> Event -> Event
    renumberEvent i = set evtUID newUid
      where
        newUid = Text.pack (show i) <> "@" <> uidSuffix
