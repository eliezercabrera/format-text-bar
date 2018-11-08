{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Dhall
import FormatTmuxPane

instance Interpret TmuxPaneInformation

main :: IO ()
main = do
  tmuxInformation <- input auto =<< T.getContents :: IO TmuxPaneInformation
  if (isPaneActive tmuxInformation)
    then T.putStr (paneCurrentPath tmuxInformation)
    else T.putStr (paneCurrentCommand tmuxInformation)
