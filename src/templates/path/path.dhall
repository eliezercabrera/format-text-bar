  λ(default : ./../Segment.dhall)
→ λ(tmuxInfo : ./../TmuxPaneInformation.dhall)
→     let override =
            { segmentRefineContent = [ "shortenPath" ] : Optional Text }
  
  in  default ⫽ override
