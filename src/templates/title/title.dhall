  λ(default : ./../Segment.dhall)
→ λ(isActive : Bool)
→     let override = { segmentRefineContent = [ "title" ] : Optional Text }
  
  in  default ⫽ override
