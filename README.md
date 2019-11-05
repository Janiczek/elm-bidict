# `Janiczek/elm-bidict`

**A dictionary data structure allowing for many-to-one and many-to-many relationships.**

Essentially it allows you to ask for the reverse mapping (from values to keys).

(There are both a `Dict`-using and an `assoc-list`-using variant for each of these. Thus, one needs `comparable` keys and values and the other can hold any types, but has slightly worse performance characteristics.)

### Many to one - BiDict

```elm
manyToOne : BiDict String Int
manyToOne =
    BiDict.empty
        |> BiDict.insert "A" 1
        |> BiDict.insert "B" 2
        |> BiDict.insert "C" 1
        |> BiDict.insert "D" 4

BiDict.get "A" manyToOne
--> Just 1 -- just like Dict!

BiDict.getReverse 1 manyToOne
--> Set.fromList ["A", "C"]
```

### Many to many - MultiBiDict

There is also `MultiBiDict`: it not only remembers the reverse mapping, it also **allows you to have multiple values per key:**

```elm
manyToMany : MultiBiDict String Int
manyToMany =
    MultiBiDict.empty
        |> MultiBiDict.insert "A" 1
        |> MultiBiDict.insert "B" 2
        |> MultiBiDict.insert "C" 3
        |> MultiBiDict.insert "A" 2

MultiBiDict.get "A" manyToMany
--> Set.fromList [1, 2]

MultiBiDict.getReverse 2 manyToMany
--> Set.fromList ["A", "B"]
```
