  λ(tmuxInfo : ./TmuxPaneInformation.dhall)
→ λ(segmentName : Text)
→ λ(content : Text)
→   { segmentName =
        segmentName : Text
    , segmentLeftEnd =
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
