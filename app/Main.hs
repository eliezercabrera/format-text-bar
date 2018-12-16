{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bar
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V
import Dhall hiding (maybe)
import FormatTmuxPane
import Rule
import Segment

constructDhallTemplate :: (Interpret result) => T.Text -> IO result
constructDhallTemplate pathFromRoot =
  let root = "/home/eli/code/cli_utilities/format-tmux-pane/src/templates/"
      path = T.append root pathFromRoot
   in input auto path

constructBar :: TmuxPaneInformation -> IO Bar
constructBar paneInfo = do
  makeBar <- constructDhallTemplate "default-bar.dhall"
  return (makeBar paneInfo)

constructSegment :: TmuxPaneInformation -> T.Text -> IO Segment
constructSegment paneInfo segmentName = do
  makeSegment <- constructDhallTemplate "default-segment.dhall"
  makeOverride <-
    constructDhallTemplate (T.append segmentName "/default") :: IO (Segment -> TmuxPaneInformation -> Segment)
  let content =
        fromMaybe "" (M.lookup segmentName (segmentNameToContent paneInfo))
  return (makeOverride (makeSegment paneInfo segmentName content) paneInfo)

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents
  defaultBar <- constructBar tmuxInformation
  segments <- mapM (constructSegment tmuxInformation) (barSegments defaultBar)
  rules <- constructDhallTemplate "rules.dhall" :: IO [Rule]
  T.putStr (printBar (foldl (flip processRule) segments rules) defaultBar)
