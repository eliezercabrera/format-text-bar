  λ(default : ./../Segment.dhall)
→ λ(tmuxInfo : ./../TmuxPaneInformation.dhall)
→     let override =
            { segmentRefineContent = [ "lightCircled" ] : Optional Text }
  
  in  default ⫽ override
