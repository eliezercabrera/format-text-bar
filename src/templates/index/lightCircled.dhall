  λ(default : ./../Segment.dhall)
→ λ(isActive : Bool)
→     let override =
            { segmentRefineContent = [ "lightCircled" ] : Optional Text }
  
  in  default ⫽ override
