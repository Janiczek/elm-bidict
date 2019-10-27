# `Janiczek/elm-bidict`

A dictionary data structure with a twist: it allows you to ask for the reverse mapping (from values to keys).

```elm
myMapping : BiDict String Int
myMapping =
    BiDict.empty
        |> BiDict.insert "A" 1
        |> BiDict.insert "B" 2
        |> BiDict.insert "C" 1
        |> BiDict.insert "D" 4

BiDict.getReverse 1 myMapping
--> Set.fromList ["A", "C"]
```

There are both a `Dict`-using and an `assoc-list`-using variant. Thus, one needs `comparable` keys and values and the other can hold any types, but has slightly worse performance characteristics.
