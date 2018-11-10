  λ(default : ./../Segment.dhall)
→ λ(isActive : Bool)
→     let override =
            { segmentLeftEnd =
                [ if isActive then "\u3010" else "\u300C" ] : Optional Text
            , segmentRightEnd =
                [ if isActive then "\u3011" else "\u300D" ] : Optional Text
            , segmentPaddingWidth =
                0 : Natural
            , segmentRefineContent =
                [ "kanji" ] : Optional Text
            }
  
  in  default ⫽ override
