    let alignment = constructors ./AlignmentType.dhall

in  { LEFT =
        alignment.ToTheLeft {=}
    , CENTER =
        alignment.Centered {=}
    , RIGHT =
        alignment.ToTheRight {=}
    }
