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
constructBar paneInfo =
  let dhallTemplate =
        "    let makeBar = ${root}templates/default-bar.dhall\
        \ in makeBar ${isActiveText}"
      dhallCommand =
        foldl
          replaceTemplate
          dhallTemplate
          [ ("root", defaultRoot)
          , ("isActiveText", T.pack (show (isPaneActive paneInfo)))
          ]
   in input auto dhallCommand

constructSegment :: TmuxPaneInformation -> T.Text -> IO Segment
constructSegment paneInfo segmentName =
  let dhallTemplate =
        "    let makeSegment = ${root}templates/default-segment.dhall\
        \ in let makeOverride = ${root}templates/${segmentName}/default\
        \ in let defaultSegment = makeSegment ${isActiveText}\
        \ in let overridenSegment = makeOverride defaultSegment ${isActiveText}\
        \ in if ${hasExtraOverride}\
        \ then overridenSegment // {segmentContent = \"${extraOverride}\"}\
        \ else overridenSegment"
      dhallCommand =
        foldl
          replaceTemplate
          dhallTemplate
          [ ("root", defaultRoot)
          , ("segmentName", segmentName)
          , ("isActiveText", T.pack (show (isPaneActive paneInfo)))
          , ( "hasExtraOverride"
            , T.pack
                (show (M.member segmentName (segmentNameToContent paneInfo))))
          , ( "extraOverride"
            , fromMaybe
                ""
                (M.lookup segmentName (segmentNameToContent paneInfo)))
          ]
   in input auto dhallCommand

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents
  defaultBar <- constructBar tmuxInformation
  segments <- mapM (constructSegment tmuxInformation) (barSegments defaultBar)
  T.putStr (printBar segments defaultBar)
