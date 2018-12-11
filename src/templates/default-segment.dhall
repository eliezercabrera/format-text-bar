  λ(tmuxInfo : ./TmuxPaneInformation.dhall)
→ λ(content : Text)
→   { segmentLeftEnd =
        [ "" ] : Optional Text
    , segmentRightEnd =
        [ "" ] : Optional Text
    , segmentPaddingWidth =
        1 : Natural
    , segmentContent =
        content
    , segmentRefineContent =
        [] : Optional Text
    }
  : ./Segment.dhall
