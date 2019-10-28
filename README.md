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

There is also `MultiBiDict`: it not only remembers the reverse mapping, it also **allows you to have multiple values per key:**

```elm
myMapping : MultiBiDict String Int
myMapping =
    MultiBiDict.empty
        |> MultiBiDict.insert "A" 1
        |> MultiBiDict.insert "B" 2
        |> MultiBiDict.insert "C" 3
        |> MultiBiDict.insert "A" 2

MultiBiDict.get "A" myMapping
--> Set.frmoList [1, 2]

MultiBiDict.getReverse 2 myMapping
--> Set.frmoList ["A", "B"]
```

There are both a `Dict`-using and an `assoc-list`-using variant for each of these. Thus, one needs `comparable` keys and values and the other can hold any types, but has slightly worse performance characteristics.
