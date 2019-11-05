# `Janiczek/elm-bidict`

**A dictionary data structure allowing for many-to-one, one-to-many and many-to-many relationships.**

These, along with the ordinary Dict, give you all the relations you might need:

* one-to-one: `Dict`
* many-to-one: `BiDict`
* one-to-many: `MultiDict`
* many-to-many: `MultiBiDict`

The `many-to-*` variants allow you to ask for the reverse mapping (from values to keys) - see the `getReverse` functions.

There are both a `Dict`-using and an `assoc-list`-using variant for each of these. Thus, one needs `comparable` keys and values and the other can hold any types, but has slightly worse performance characteristics.

### Many to one - BiDict

```elm
manyToOne : BiDict String Int
manyToOne =
    BiDict.empty
        |> BiDict.insert "A" 1
        |> BiDict.insert "B" 2
        |> BiDict.insert "C" 1
        |> BiDict.insert "D" 4

BiDict.getReverse 1 manyToOne
--> Set.fromList ["A", "C"]
```

### One to many - MultiDict

```elm
oneToMany : MultiDict String Int
oneToMany =
    MultiDict.empty
        |> MultiDict.insert "A" 1
        |> MultiDict.insert "B" 2
        |> MultiDict.insert "A" 3
        |> MultiDict.insert "D" 4

MultiDict.get "A" oneToMany
--> Set.fromList [1, 3]
```

### Many to many - MultiBiDict

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
