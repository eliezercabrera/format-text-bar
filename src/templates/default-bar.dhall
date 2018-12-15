let Alignment = ./enums/Alignment.dhall
in
  λ(tmuxInfo : ./TmuxPaneInformation.dhall)
→   { barLeftEnd =
        [ "┨" ] : Optional Text
    , barRightEnd =
        [ "┠" ] : Optional Text
    , separator =
        [ "┃" ] : Optional Text
    , alignment =
          Alignment.LEFT : ./enums/AlignmentType.dhall
    , barWidth =
        [] : Optional Natural
    , barSegments =
        [ "index", "command", "path", "title" ]
    }
  : ./Bar.dhall
