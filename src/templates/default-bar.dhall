 {
   barLeftEnd = ["┨"] : Optional Text
  , barRightEnd = ["┠"] : Optional Text
  , separator = ["┃"] : Optional Text
  , alignment = <ToTheLeft = {=} | ToTheRight : {} | Centered : {}> : ./Alignment.dhall
  , barWidth = [] : Optional  Natural
  , segments = ["index", "command", "path", "title"]
  }
