    let BarOperation = constructors ./BarOperationType.dhall

in  { DELETE_SEGMENT =
          λ(segmentName : Text)
        →   BarOperation.DeleteSegment { segmentName = segmentName }
          : ./BarOperationType.dhall
    }
