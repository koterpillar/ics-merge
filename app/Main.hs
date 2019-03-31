{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad

import           System.Directory

import           Web.Scotty

import           Event
import           Feed

eventsDir :: String
eventsDir = "Events"

main :: IO ()
main = do
  eventFiles <-
    (map ((eventsDir ++ "/") ++) <$> getDirectoryContents eventsDir) >>=
    filterM doesFileExist
  events <- traverse readEvent eventFiles
  let feed = renumberFeed "koterpillar.com" $ makeFeed $ join events
  scotty 3000 $
    get "/" $ do
      setHeader "Content-Type" "text/calendar; charset=utf-8"
      raw $ feedContent feed
