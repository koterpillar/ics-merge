module ICS where

import           Control.Monad

import           Data.ByteString.Lazy    (ByteString)

import           Data.List.Extra

import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

newtype ICS = ICS
  { unICS :: [String]
  }

newtype Feed =
  Feed [String]

icsInnerContent :: ICS -> [String]
icsInnerContent (ICS lns) =
  dropWhile (/= "BEGIN:VEVENT\r") $ dropWhileEnd (/= "END:VEVENT\r") lns

makeFeed :: [ICS] -> Feed
makeFeed events =
  Feed $
  [ "BEGIN:VCALENDAR\r"
  , "VERSION:2.0\r"
  , "PRODID:-//koterpillar//ics-merge\r"
  , "CALSCALE:GREGORIAN\r"
  ] ++
  join (map icsInnerContent events) ++ ["END:VCALENDAR\r"]

readICS :: FilePath -> IO ICS
readICS fileName =
  ICS . filter (not . ("ACKNOWLEDGED:" `isPrefixOf`)) . lines <$>
  readFile fileName

feedContent :: Feed -> ByteString
feedContent (Feed lns) = Text.encodeUtf8 $ Text.unlines $ map Text.pack lns
