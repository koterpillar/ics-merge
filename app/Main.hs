{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad

import           System.Directory

import           Web.Scotty

import           ICS

eventsDir :: String
eventsDir = "recent-events"

main :: IO ()
main = do
  eventFiles <-
    (map ((eventsDir ++ "/") ++) <$> getDirectoryContents eventsDir) >>=
    filterM doesFileExist
  events <- traverse readICS eventFiles
  let feed = makeFeed events
  scotty 3000 $
    get "/" $ do
      setHeader "Content-Type" "text/calendar; charset=utf-8"
      raw $ feedContent feed
