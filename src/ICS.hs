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
icsInnerContent (ICS lns) = unwrapLines lns

makeFeed :: [ICS] -> Feed
makeFeed events =
  Feed $
  [ "BEGIN:VCALENDAR\r"
  , "VERSION:2.0\r"
  , "PRODID:-//koterpillar//ics-merge\r"
  , "CALSCALE:GREGORIAN\r"
  ] ++
  join (map icsInnerContent events) ++ ["END:VCALENDAR\r"]

wrapLines :: [String] -> [String]
wrapLines = concatMap wrapLine
  where
    wrapLine ln
      | length ln <= 72 = [ln]
      | otherwise =
        let (ln1, rest) = splitAt 72 ln
            (ln2:lns) = wrapLine rest
         in ln1 : (' ' : ln2) : lns

unwrapLines :: [String] -> [String]
unwrapLines [] = []
unwrapLines [ln] = [ln]
unwrapLines (l1:l2:rest)
  | " " `isPrefixOf` l2 = unwrapLines ((l1 ++ tail l2) : rest)
  | otherwise = l1 : unwrapLines (l2 : rest)

readICS :: FilePath -> IO ICS
readICS fileName =
  ICS .
  wrapLines .
  dropWhile (/= "BEGIN:VEVENT\r") .
  dropWhileEnd (/= "END:VEVENT\r") .
  filter (not . ("ACKNOWLEDGED:" `isPrefixOf`)) . lines <$>
  readFile fileName

feedContent :: Feed -> ByteString
feedContent (Feed lns) = Text.encodeUtf8 $ Text.unlines $ map Text.pack lns
