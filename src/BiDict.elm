module BiDict exposing
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

import Dict exposing (Dict)
import Set exposing (Set)


{-| TODO
-}
type BiDict comparable1 comparable2
    = BiDict
        { forward : Dict comparable1 comparable2
        , reverse : Dict comparable2 (Set comparable1)
        }


{-| TODO
-}
empty : BiDict comparable1 comparable2
empty =
    BiDict
        { forward = Dict.empty
        , reverse = Dict.empty
        }


{-| TODO
-}
singleton : comparable1 -> comparable2 -> BiDict comparable1 comparable2
singleton from to =
    BiDict
        { forward = Dict.singleton from to
        , reverse = Dict.singleton to (Set.singleton from)
        }


{-| TODO
-}
insert : comparable1 -> comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
insert from to (BiDict d) =
    BiDict
        { d
            | forward = Dict.insert from to d.forward
            , reverse = Dict.update to (Maybe.withDefault Set.empty >> Set.insert from >> Just) d.reverse
        }


{-| TODO
-}
update : comparable1 -> (Maybe comparable2 -> Maybe comparable2) -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
update from fn (BiDict d) =
    BiDict
        { d
            | forward = Dict.update from fn d.forward
            , reverse = Debug.todo "update.reverse"
        }


{-| TODO
-}
remove : comparable1 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
remove from (BiDict d) =
    BiDict
        { d
            | forward = Dict.remove from d.forward
            , reverse = Dict.map (\_ set -> Set.remove from set) d.reverse
        }


{-| TODO
-}
isEmpty : BiDict comparable1 comparable2 -> Bool
isEmpty (BiDict d) =
    Dict.isEmpty d.forward


{-| TODO
-}
member : comparable1 -> BiDict comparable1 comparable2 -> Bool
member from (BiDict d) =
    Dict.member from d.forward


{-| TODO
-}
get : comparable1 -> BiDict comparable1 comparable2 -> Maybe comparable2
get from (BiDict d) =
    Dict.get from d.forward


{-| TODO
-}
getReverse : comparable2 -> BiDict comparable1 comparable2 -> Set comparable1
getReverse to (BiDict d) =
    Dict.get to d.reverse
        |> Maybe.withDefault Set.empty


{-| TODO
-}
size : BiDict comparable1 comparable2 -> Int
size (BiDict d) =
    Dict.size d.forward


{-| TODO
-}
sizeReverse : BiDict comparable1 comparable2 -> Int
sizeReverse (BiDict d) =
    Dict.size d.reverse


{-| TODO
-}
keys : BiDict comparable1 comparable2 -> List comparable1
keys (BiDict d) =
    Dict.keys d.forward


{-| TODO
-}
values : BiDict comparable1 comparable2 -> List comparable2
values (BiDict d) =
    Dict.values d.forward


{-| TODO
-}
uniqueValues : BiDict comparable1 comparable2 -> List comparable2
uniqueValues (BiDict d) =
    Dict.keys d.reverse


{-| TODO
-}
toList : BiDict comparable1 comparable2 -> List ( comparable1, comparable2 )
toList (BiDict d) =
    Dict.toList d.forward


{-| TODO
-}
toReverseList : BiDict comparable1 comparable2 -> List ( comparable2, Set comparable1 )
toReverseList (BiDict d) =
    Dict.toList d.reverse


{-| TODO
-}
fromList : List ( comparable1, comparable2 ) -> BiDict comparable1 comparable2
fromList list =
    BiDict
        { forward = Dict.fromList list
        , reverse = Debug.todo "fromList.reverse"
        }


{-| TODO
-}
fromReverseList : List ( comparable2, Set comparable1 ) -> BiDict comparable1 comparable2
fromReverseList list =
    BiDict
        { forward = Debug.todo "fromReverseList.forward"
        , reverse = Dict.fromList list
        }


{-| TODO
-}
map : (comparable1 -> comparable21 -> comparable22) -> BiDict comparable1 comparable21 -> BiDict comparable1 comparable22
map fn (BiDict d) =
    BiDict
        { forward = Dict.map fn d.forward
        , reverse = Debug.todo "map.reverse"
        }


{-| TODO
-}
foldl : (comparable1 -> comparable2 -> acc -> acc) -> acc -> BiDict comparable1 comparable2 -> acc
foldl fn zero (BiDict d) =
    -- TODO anything about the reverse?
    Dict.foldl fn zero d.forward


{-| TODO
-}
foldr : (comparable1 -> comparable2 -> acc -> acc) -> acc -> BiDict comparable1 comparable2 -> acc
foldr fn zero (BiDict d) =
    -- TODO anything about the reverse?
    Dict.foldr fn zero d.forward


{-| TODO
-}
filter : (comparable1 -> comparable2 -> Bool) -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
filter fn (BiDict d) =
    BiDict
        { d
            | forward = Dict.filter fn d.forward
            , reverse = Debug.todo "filter.reverse"
        }


{-| TODO
-}
partition : (comparable1 -> comparable2 -> Bool) -> BiDict comparable1 comparable2 -> ( BiDict comparable1 comparable2, BiDict comparable1 comparable2 )
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
union : BiDict comparable1 comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
union (BiDict left) (BiDict right) =
    BiDict
        { forward = Dict.union left.forward right.forward
        , reverse = Debug.todo "union.reverse"
        }


{-| TODO
-}
intersect : BiDict comparable1 comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
intersect (BiDict left) (BiDict right) =
    BiDict
        { forward = Dict.intersect left.forward right.forward
        , reverse = Debug.todo "intersect.reverse"
        }


{-| TODO
-}
diff : BiDict comparable1 comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
diff (BiDict left) (BiDict right) =
    BiDict
        { forward = Dict.diff left.forward right.forward
        , reverse = Debug.todo "diff.reverse"
        }


{-| TODO
-}
merge :
    (comparable1 -> comparable21 -> acc -> acc)
    -> (comparable1 -> comparable21 -> comparable22 -> acc -> acc)
    -> (comparable1 -> comparable22 -> acc -> acc)
    -> BiDict comparable1 comparable21
    -> BiDict comparable1 comparable22
    -> acc
    -> acc
merge fnLeft fnBoth fnRight left right zero =
    Debug.todo "merge"
