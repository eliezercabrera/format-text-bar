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

constructSegment :: TmuxPaneInformation -> T.Text -> IO Segment
constructSegment paneInfo segmentName = do
  let segmentPath =
        foldl T.append defaultRoot ["templates/", segmentName, "/default"]
  input auto $
    fromMaybe segmentPath $ do
      contentOverride <- M.lookup segmentName (segmentNameToContent paneInfo)
      return
        (foldl
           T.append
           segmentPath
           [" // {segmentContent = \"", contentOverride, "\"}"])

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents
  defaultBar <- input auto (T.append defaultRoot "templates/default-bar.dhall")
  segments <- mapM (constructSegment tmuxInformation) (barSegments defaultBar)
  T.putStr (printBar segments defaultBar)
