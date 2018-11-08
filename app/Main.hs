{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bar
import qualified Data.Text.IO as T
import Dhall
import FormatTmuxPane
import Segment

instance Interpret TmuxPaneInformation

instance Interpret Alignment

instance Interpret Bar

instance Interpret Segment

what :: Alignment -> Text
what ToTheLeft = "zurdo"
what _ = ""

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents :: IO TmuxPaneInformation
  if (isPaneActive tmuxInformation)
    then T.putStr (paneCurrentPath tmuxInformation)
    else T.putStr (paneCurrentCommand tmuxInformation)
  defaultBar <-
    input
      auto
      "/home/eli/code/cli_utilities/format-tmux-pane/src/templates/default-bar.dhall" :: IO Bar
  case alignment defaultBar of
    ToTheRight -> T.putStr "What not to print"
    _ -> T.putStr "What I want printed"
