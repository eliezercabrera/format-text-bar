{-# LANGUAGE DeriveGeneric #-}

module FormatTmuxPane
  ( TmuxPaneInformation(..)
  , format
  ) where

import Dhall

data TmuxPaneInformation = TmuxPaneInformation
  { windowWidth :: Natural
  , isPaneActive :: Bool
  , paneWidth :: Natural
  , paneIndex :: Natural
  , paneCurrentCommand :: Text
  , paneCurrentPath :: Text
  , paneTitle :: Text
  } deriving (Generic, Show)

format :: a -> String
format _ = "Hello, World!"
