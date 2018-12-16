    let TextComparator = constructors ./TextComparatorType.dhall

in  { EQUALS =
          λ(needle : Text)
        → TextComparator.Equals { needle = needle } : ./TextComparatorType.dhall
    }
