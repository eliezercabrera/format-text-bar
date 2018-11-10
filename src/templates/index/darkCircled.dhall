  λ(default : ./../Segment.dhall)
→ λ(isActive : Bool)
→     let override =
            { segmentRefineContent = [ "darkCircled" ] : Optional Text }
  
  in  default ⫽ override
