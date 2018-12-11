  λ(default : ./../Segment.dhall)
→ λ(tmuxInfo : ./../TmuxPaneInformation.dhall)
→     let override =
            { segmentRefineContent = [ "darkCircled" ] : Optional Text }
  
  in  default ⫽ override
