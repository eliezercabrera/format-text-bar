    let TextComparator = ./enums/TextComparator.dhall

in  let BarOperation = ./enums/BarOperation.dhall

in  [ { conditions =
          [ [ { conditionSegmentName =
                  "command"
              , operator =
                    TextComparator.EQUALS "fish" : ./enums/TextComparatorType.dhall
              , ignoreCase =
                  True
              }
            ]
          ]
      , effects =
            [ BarOperation.DELETE_SEGMENT "title" : ./enums/BarOperationType.dhall]
      }
    ] :
    List
    ./Rule.dhall
