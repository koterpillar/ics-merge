{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Event where

import           Control.Lens

import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

newtype Event = Event
  { evtContent :: [(Text, Text)]
  } deriving (Show)

evtContentLens :: Iso' Event [(Text, Text)]
evtContentLens = iso evtContent Event

evtAttribute :: Text -> Traversal' Event Text
evtAttribute attr =
  evtContentLens . traverse . filtered (\(k, _) -> k == attr) . _2

evtUID :: Traversal' Event Text
evtUID = evtAttribute "UID"

evtText :: Event -> Text
evtText = unlinesR . wrapLines . map (uncurry mkLine) . evtContent
  where
    mkLine k v = Text.intercalate ":" [k, v]

evtParse :: Text -> [Event]
evtParse =
  map mkEvent .
  drop 1 .
  splitEvents .
  dropWhile (not . isBeginEvent) .
  filter (not . isEndCalendar) .
  map parseLine . filter (not . Text.null) . unwrapLines . linesR
  where
    parseLine ln =
      let (k, v) = Text.breakOn ":" ln
       in if Text.null v
            then error $ "Invalid line " ++ show ln
            else (k, Text.drop 1 v)
    isBeginEvent (k, v) = k == "BEGIN" && v == "VEVENT"
    endEvent = ("END", "VEVENT")
    isEndEvent = (== endEvent)
    isEndCalendar (k, v) = k == "END" && v == "VCALENDAR"
    mkEvent lns
      | isBeginEvent (head lns) && isEndEvent (last lns) = Event lns
      | otherwise = error $ "Wrong event split: " ++ show lns
    splitEvents :: [(Text, Text)] -> [[(Text, Text)]]
    splitEvents [] = []
    splitEvents lns
      | isBeginEvent (head lns) =
        let (ev, rest) = break isEndEvent lns
         in (ev ++ [endEvent]) : splitEvents (tail rest)
      | otherwise = error $ "event start expected: " ++ show lns

wrapLines :: [Text] -> [Text]
wrapLines = concatMap wrapLine
  where
    wrapLine ln
      | Text.length ln <= 72 = [ln]
      | otherwise =
        let (ln1, rest) = Text.splitAt 72 ln
            (ln2:lns) = wrapLine rest
         in ln1 : (" " <> ln2) : lns

unwrapLines :: [Text] -> [Text]
unwrapLines [] = []
unwrapLines [ln] = [ln]
unwrapLines (l1:l2:rest)
  | " " `Text.isPrefixOf` l2 = unwrapLines ((l1 <> Text.drop 1 l2) : rest)
  | otherwise = l1 : unwrapLines (l2 : rest)

unlinesR :: [Text] -> Text
unlinesR = Text.intercalate "\r\n"

linesR :: Text -> [Text]
linesR = Text.splitOn "\r\n"

readEvent :: FilePath -> IO [Event]
readEvent fileName = do
  contents <- Text.readFile fileName
  pure $ evtParse contents
