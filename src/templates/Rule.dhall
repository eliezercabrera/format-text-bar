    let BarOperation = ./enums/BarOperationType.dhall

in  let Condition = ./Condition.dhall

in  { conditions : List (List Condition), effects : List BarOperation }
