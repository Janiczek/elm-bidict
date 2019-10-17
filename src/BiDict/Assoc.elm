module BiDict.Assoc exposing
    ( BiDict
    , getReverse, sizeReverse, uniqueValues, toReverseList, fromReverseList
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A bidirectional dictionary mapping unique keys to values, and that maintains
a mapping from the values back to keys.

Uses [`assoc-list`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest/) and [`assoc-set`](https://package.elm-lang.org/packages/erlandsona/assoc-set/latest/) under the hood to
get rid of the `comparable` constraint on keys that's usually associated with Dicts and Sets.


# Dictionaries

@docs BiDict


# Differences from Dict

@docs getReverse, sizeReverse, uniqueValues, toReverseList, fromReverseList


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)



-- TODO think about all the possible handy `reverse` functions
{-

   A -> 1    1 -> [A, C]
   B -> 2    2 -> [B]
   C -> 1


-}


{-| TODO
-}
type BiDict a b
    = BiDict
        { forward : Dict a b
        , reverse : Dict b (Set a)
        }


{-| TODO
-}
empty : BiDict a b
empty =
    BiDict
        { forward = Dict.empty
        , reverse = Dict.empty
        }


{-| TODO
-}
singleton : a -> b -> BiDict a b
singleton from to =
    BiDict
        { forward = Dict.singleton from to
        , reverse = Dict.singleton to (Set.singleton from)
        }


{-| TODO
-}
insert : a -> b -> BiDict a b -> BiDict a b
insert from to (BiDict d) =
    BiDict
        { d
            | forward = Dict.insert from to d.forward
            , reverse = Dict.update to (Maybe.withDefault Set.empty >> Set.insert from >> Just) d.reverse
        }


{-| TODO
-}
update : a -> (Maybe b -> Maybe b) -> BiDict a b -> BiDict a b
update from fn (BiDict d) =
    BiDict
        { d
            | forward = Dict.update from fn d.forward
            , reverse = Debug.todo "update.reverse"
        }


{-| TODO
-}
remove : a -> BiDict a b -> BiDict a b
remove from (BiDict d) =
    BiDict
        { d
            | forward = Dict.remove from d.forward
            , reverse = Dict.map (\_ set -> Set.remove from set) d.reverse
        }


{-| TODO
-}
isEmpty : BiDict a b -> Bool
isEmpty (BiDict d) =
    Dict.isEmpty d.forward


{-| TODO
-}
member : a -> BiDict a b -> Bool
member from (BiDict d) =
    Dict.member from d.forward


{-| TODO
-}
get : a -> BiDict a b -> Maybe b
get from (BiDict d) =
    Dict.get from d.forward


{-| TODO
-}
getReverse : b -> BiDict a b -> Set a
getReverse to (BiDict d) =
    Dict.get to d.reverse
        |> Maybe.withDefault Set.empty


{-| TODO
-}
size : BiDict a b -> Int
size (BiDict d) =
    Dict.size d.forward


{-| TODO
-}
sizeReverse : BiDict a b -> Int
sizeReverse (BiDict d) =
    Dict.size d.reverse


{-| TODO
-}
keys : BiDict a b -> List a
keys (BiDict d) =
    Dict.keys d.forward


{-| TODO
-}
values : BiDict a b -> List b
values (BiDict d) =
    Dict.values d.forward


{-| TODO
-}
uniqueValues : BiDict a b -> List b
uniqueValues (BiDict d) =
    Dict.keys d.reverse


{-| TODO
-}
toList : BiDict a b -> List ( a, b )
toList (BiDict d) =
    Dict.toList d.forward


{-| TODO
-}
toReverseList : BiDict a b -> List ( b, Set a )
toReverseList (BiDict d) =
    Dict.toList d.reverse


{-| TODO
-}
fromList : List ( a, b ) -> BiDict a b
fromList list =
    BiDict
        { forward = Dict.fromList list
        , reverse = Debug.todo "fromList.reverse"
        }


{-| TODO
-}
fromReverseList : List ( b, Set a ) -> BiDict a b
fromReverseList list =
    BiDict
        { forward = Debug.todo "fromReverseList.forward"
        , reverse = Dict.fromList list
        }


{-| TODO
-}
map : (a -> b1 -> b2) -> BiDict a b1 -> BiDict a b2
map fn (BiDict d) =
    BiDict
        { forward = Dict.map fn d.forward
        , reverse = Debug.todo "map.reverse"
        }


{-| TODO
-}
foldl : (a -> b -> acc -> acc) -> acc -> BiDict a b -> acc
foldl fn zero (BiDict d) =
    -- TODO anything about the reverse?
    Dict.foldl fn zero d.forward


{-| TODO
-}
foldr : (a -> b -> acc -> acc) -> acc -> BiDict a b -> acc
foldr fn zero (BiDict d) =
    -- TODO anything about the reverse?
    Dict.foldr fn zero d.forward


{-| TODO
-}
filter : (a -> b -> Bool) -> BiDict a b -> BiDict a b
filter fn (BiDict d) =
    BiDict
        { d
            | forward = Dict.filter fn d.forward
            , reverse = Debug.todo "filter.reverse"
        }


{-| TODO
-}
partition : (a -> b -> Bool) -> BiDict a b -> ( BiDict a b, BiDict a b )
partition fn (BiDict d) =
    let
        ( forwardTrue, forwardFalse ) =
            Dict.partition fn d.forward

        ( reverseTrue, reverseFalse ) =
            Debug.todo "partition.reverse"
    in
    ( BiDict
        { d
            | forward = forwardTrue
            , reverse = reverseTrue
        }
    , BiDict
        { d
            | forward = forwardFalse
            , reverse = reverseFalse
        }
    )


{-| TODO
-}
union : BiDict a b -> BiDict a b -> BiDict a b
union (BiDict left) (BiDict right) =
    BiDict
        { forward = Dict.union left.forward right.forward
        , reverse = Debug.todo "union.reverse"
        }


{-| TODO
-}
intersect : BiDict a b -> BiDict a b -> BiDict a b
intersect (BiDict left) (BiDict right) =
    BiDict
        { forward = Dict.intersect left.forward right.forward
        , reverse = Debug.todo "intersect.reverse"
        }


{-| TODO
-}
diff : BiDict a b -> BiDict a b -> BiDict a b
diff (BiDict left) (BiDict right) =
    BiDict
        { forward = Dict.diff left.forward right.forward
        , reverse = Debug.todo "diff.reverse"
        }


{-| TODO
-}
merge :
    (a -> b1 -> acc -> acc)
    -> (a -> b1 -> b2 -> acc -> acc)
    -> (a -> b2 -> acc -> acc)
    -> BiDict a b1
    -> BiDict a b2
    -> acc
    -> acc
merge fnLeft fnBoth fnRight left right zero =
    Debug.todo "merge"
