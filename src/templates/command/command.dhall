  λ(default : ./../Segment.dhall)
→ λ(isActive : Bool)
→     let override = { segmentRefineContent = [ "command" ] : Optional Text }
  
  in  default ⫽ override
