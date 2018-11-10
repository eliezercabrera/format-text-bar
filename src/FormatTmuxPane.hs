{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FormatTmuxPane
  ( TmuxPaneInformation(..)
  , segmentNameToContent
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
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

segmentNameToContent :: TmuxPaneInformation -> M.Map T.Text T.Text
segmentNameToContent paneInfo =
  M.fromList
    [ ("index", (T.pack . show . paneIndex) paneInfo)
    , ("command", paneCurrentCommand paneInfo)
    , ("path", paneCurrentPath paneInfo)
    , ("title", paneTitle paneInfo)
    ]
