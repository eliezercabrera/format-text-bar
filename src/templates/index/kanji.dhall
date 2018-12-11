  λ(default : ./../Segment.dhall)
→ λ(tmuxInfo : ./../TmuxPaneInformation.dhall)
→     let override =
            { segmentLeftEnd =
                [ if tmuxInfo.isPaneActive then "\u3010" else "\u300C" ] : Optional Text
            , segmentRightEnd =
                [ if tmuxInfo.isPaneActive then "\u3011" else "\u300D" ] : Optional Text
            , segmentPaddingWidth =
                0 : Natural
            , segmentRefineContent =
                [ "kanji" ] : Optional Text
            }
  
  in  default ⫽ override
