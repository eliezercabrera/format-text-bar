  λ(default : ./../Segment.dhall)
→ λ(tmuxInfo : ./../TmuxPaneInformation.dhall)
→     let override = { segmentRefineContent = [ "title" ] : Optional Text }
  
  in  default ⫽ override
