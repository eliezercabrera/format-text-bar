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
import Segment

instance Interpret TmuxPaneInformation

instance Interpret Alignment

instance Interpret Bar

instance Interpret Segment

defaultRoot :: T.Text
defaultRoot = "/home/eli/code/cli_utilities/format-tmux-pane/src/"

replaceTemplate :: T.Text -> (T.Text, T.Text) -> T.Text
replaceTemplate template (key, value) = T.replace trueKey value template
  where
    trueKey = foldl T.append "${" [key, "}"]

constructBar :: TmuxPaneInformation -> IO Bar
constructBar paneInfo = do
  let dhallTemplate = T.concat [defaultRoot, "templates/default-bar.dhall"]
  makeBar <- input auto dhallTemplate :: IO (TmuxPaneInformation -> Bar)
  return (makeBar paneInfo)

constructSegment :: TmuxPaneInformation -> T.Text -> IO Segment
constructSegment paneInfo segmentName = do
  let makeSegmentTemplate =
        T.concat [defaultRoot, "templates/default-segment.dhall"]
  makeSegment <-
    input auto makeSegmentTemplate :: IO (TmuxPaneInformation -> Text -> Segment)
  let overrideDefaultTemplate =
        T.concat [defaultRoot, "templates/", segmentName, "/default"]
  makeOverride <-
    input auto overrideDefaultTemplate :: IO (Segment -> TmuxPaneInformation -> Segment)
  let content =
        fromMaybe "" (M.lookup segmentName (segmentNameToContent paneInfo))
  let defaultSegment = makeSegment paneInfo content
  return (makeOverride defaultSegment paneInfo)

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents
  defaultBar <- constructBar tmuxInformation
  segments <- mapM (constructSegment tmuxInformation) (barSegments defaultBar)
  T.putStr (printBar segments defaultBar)
