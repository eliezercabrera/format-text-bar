< DeleteSegment :
    { segmentName : Text }
| AddSegment :
    { segmentName : Text }
| ReplaceBar :
    { segmentNames : List Text }
| RefineSegment :
    { segmentName : Text, refineFunction : Text }
>
