{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bar
import qualified Data.Text.IO as T
import Dhall
import FormatTmuxPane
import Segment

instance Interpret TmuxPaneInformation

what :: Alignment -> Text
what ToTheLeft = "zurdo"
what _ = ""

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents :: IO TmuxPaneInformation
  if (isPaneActive tmuxInformation)
    then T.putStr (paneCurrentPath tmuxInformation)
    else T.putStr (paneCurrentCommand tmuxInformation)
