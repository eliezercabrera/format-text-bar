  λ(default : ./../Segment.dhall)
→ λ(tmuxInfo : ./../TmuxPaneInformation.dhall)
→     let override = { segmentRefineContent = [ "command" ] : Optional Text }
  
  in  default ⫽ override
