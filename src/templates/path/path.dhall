  λ(default : ./../Segment.dhall)
→ λ(isActive : Bool)
→     let override =
            { segmentRefineContent = [ "shortenPath" ] : Optional Text }
  
  in  default ⫽ override
