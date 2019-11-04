module MultiBiDict exposing
    ( MultiBiDict
    , toDict, fromDict, getReverse, uniqueValues, uniqueValuesCount, toReverseList
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| comparable1 bidirectional dictionary mapping unique keys to **multiple** values, which
**maintains comparable1 mapping from the values back to keys.**

Example usage:

    myMapping : MultiBiDict String Int
    myMapping =
        MultiBiDict.empty
            |> MultiBiDict.insert "A" 1
            |> MultiBiDict.insert "B" 2
            |> MultiBiDict.insert "C" 3
            |> MultiBiDict.insert "A" 2

    MultiBiDict.get "A" myMapping
    --> Set.fromList [1, 2]

    MultiBiDict.getReverse 2 myMapping
    --> Set.fromList ["A", "B"]


# Dictionaries

@docs MultiBiDict


# Differences from Dict

@docs toDict, fromDict, getReverse, uniqueValues, uniqueValuesCount, toReverseList


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
import Dict.Extra
import Set exposing (Set)


{-| comparable1 dictionary that holds multiple values per key and allows asking for the
mappings in reverse direction.

Think about it as

    type alias BiDict comparable1 comparable2 =
        { forward : Dict comparable1 comparable2 -- just comparable1 normal Dict!
        , reverse : Dict comparable2 (Set comparable1) -- the reverse mappings!
        }

-}
type MultiBiDict comparable1 comparable2
    = MultiBiDict
        { forward : Dict comparable1 (Set comparable2)
        , reverse : Dict comparable2 (Set comparable1)
        }


{-| Create an empty dictionary.
-}
empty : MultiBiDict comparable1 comparable2
empty =
    MultiBiDict
        { forward = Dict.empty
        , reverse = Dict.empty
        }


{-| Create comparable1 dictionary with one key-value pair.
-}
singleton : comparable1 -> comparable2 -> MultiBiDict comparable1 comparable2
singleton from to =
    MultiBiDict
        { forward = Dict.singleton from (Set.singleton to)
        , reverse = Dict.singleton to (Set.singleton from)
        }


{-| Insert comparable1 key-value pair into comparable1 dictionary. Replaces value when there is
a collision.
-}
insert : comparable1 -> comparable2 -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
insert from to (MultiBiDict d) =
    Dict.update
        from
        (\maybeSet ->
            case maybeSet of
                Nothing ->
                    Just (Set.singleton to)

                Just set ->
                    Just (Set.insert to set)
        )
        d.forward
        |> fromDict


{-| Update the value of comparable1 dictionary for comparable1 specific key with comparable1 given function.
-}
update : comparable1 -> (Set comparable2 -> Set comparable2) -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
update from fn (MultiBiDict d) =
    Dict.update from (Maybe.map fn) d.forward
        |> fromDict


{-| In our model, (Just Set.empty) has the same meaning as Nothing.
Make it be Nothing!
-}
normalizeSet : Set comparable1 -> Maybe (Set comparable1)
normalizeSet set =
    if Set.isEmpty set then
        Nothing

    else
        Just set


{-| Remove comparable1 key-value pair from comparable1 dictionary. If the key is not found,
no changes are made.
-}
remove : comparable1 -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
remove from (MultiBiDict d) =
    MultiBiDict
        { d
            | forward = Dict.remove from d.forward
            , reverse = Dict.Extra.filterMap (\_ set -> Set.remove from set |> normalizeSet) d.reverse
        }


{-| Determine if comparable1 dictionary is empty.

    isEmpty empty == True

-}
isEmpty : MultiBiDict comparable1 comparable2 -> Bool
isEmpty (MultiBiDict d) =
    Dict.isEmpty d.forward


{-| Determine if comparable1 key is in comparable1 dictionary.
-}
member : comparable1 -> MultiBiDict comparable1 comparable2 -> Bool
member from (MultiBiDict d) =
    Dict.member from d.forward


{-| Get the value associated with comparable1 key. If the key is not found, return
`Nothing`. This is useful when you are not sure if comparable1 key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable1 -> MultiBiDict comparable1 comparable2 -> Set comparable2
get from (MultiBiDict d) =
    Dict.get from d.forward
        |> Maybe.withDefault Set.empty


{-| Get the keys associated with comparable1 value. If the value is not found,
return an empty set.
-}
getReverse : comparable2 -> MultiBiDict comparable1 comparable2 -> Set comparable1
getReverse to (MultiBiDict d) =
    Dict.get to d.reverse
        |> Maybe.withDefault Set.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : MultiBiDict comparable1 comparable2 -> Int
size (MultiBiDict d) =
    Dict.foldl (\_ set acc -> Set.size set + acc) 0 d.forward


{-| Get all of the keys in comparable1 dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : MultiBiDict comparable1 comparable2 -> List comparable1
keys (MultiBiDict d) =
    Dict.keys d.forward


{-| Get all of the values in comparable1 dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : MultiBiDict comparable1 comparable2 -> List comparable2
values (MultiBiDict d) =
    Dict.values d.forward
        |> List.concatMap Set.toList


{-| Get comparable1 list of unique values in the dictionary.
-}
uniqueValues : MultiBiDict comparable1 comparable2 -> List comparable2
uniqueValues (MultiBiDict d) =
    Dict.keys d.reverse


{-| Get comparable1 count of unique values in the dictionary.
-}
uniqueValuesCount : MultiBiDict comparable1 comparable2 -> Int
uniqueValuesCount (MultiBiDict d) =
    Dict.size d.reverse


{-| Convert comparable1 dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : MultiBiDict comparable1 comparable2 -> List ( comparable1, Set comparable2 )
toList (MultiBiDict d) =
    Dict.toList d.forward


{-| Convert comparable1 dictionary into comparable1 reverse association list of value-keys pairs.
-}
toReverseList : MultiBiDict comparable1 comparable2 -> List ( comparable2, Set comparable1 )
toReverseList (MultiBiDict d) =
    Dict.toList d.reverse


{-| Convert an association list into comparable1 dictionary.
-}
fromList : List ( comparable1, Set comparable2 ) -> MultiBiDict comparable1 comparable2
fromList list =
    Dict.fromList list
        |> fromDict


{-| Apply comparable1 function to all values in comparable1 dictionary.
-}
map : (comparable1 -> comparable21 -> comparable22) -> MultiBiDict comparable1 comparable21 -> MultiBiDict comparable1 comparable22
map fn (MultiBiDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    Dict.map (\key set -> Set.map (fn key) set) d.forward
        |> fromDict


{-| Convert MultiBiDict into comparable1 Dict. (Throw away the reverse mapping.)
-}
toDict : MultiBiDict comparable1 comparable2 -> Dict comparable1 (Set comparable2)
toDict (MultiBiDict d) =
    d.forward


{-| Convert Dict into comparable1 MultiBiDict. (Compute the reverse mapping.)
-}
fromDict : Dict comparable1 (Set comparable2) -> MultiBiDict comparable1 comparable2
fromDict forward =
    MultiBiDict
        { forward = forward
        , reverse =
            Dict.foldl
                (\key set acc ->
                    Set.foldl
                        (\value acc_ ->
                            Dict.update
                                value
                                (\maybeSet ->
                                    case maybeSet of
                                        Nothing ->
                                            Just (Set.singleton key)

                                        Just set_ ->
                                            Just (Set.insert key set_)
                                )
                                acc_
                        )
                        acc
                        set
                )
                Dict.empty
                forward
        }


{-| Fold over the key-value pairs in comparable1 dictionary from lowest key to highest key.


    getAges users =
        Dict.foldl addAge [] users

    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (comparable1 -> Set comparable2 -> acc -> acc) -> acc -> MultiBiDict comparable1 comparable2 -> acc
foldl fn zero (MultiBiDict d) =
    Dict.foldl fn zero d.forward


{-| Fold over the key-value pairs in comparable1 dictionary from highest key to lowest key.


    getAges users =
        Dict.foldr addAge [] users

    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (comparable1 -> Set comparable2 -> acc -> acc) -> acc -> MultiBiDict comparable1 comparable2 -> acc
foldr fn zero (MultiBiDict d) =
    Dict.foldr fn zero d.forward


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (comparable1 -> Set comparable2 -> Bool) -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
filter fn (MultiBiDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    Dict.filter fn d.forward
        |> fromDict


{-| Partition comparable1 dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable1 -> Set comparable2 -> Bool) -> MultiBiDict comparable1 comparable2 -> ( MultiBiDict comparable1 comparable2, MultiBiDict comparable1 comparable2 )
partition fn (MultiBiDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    let
        ( forwardTrue, forwardFalse ) =
            Dict.partition fn d.forward
    in
    ( fromDict forwardTrue
    , fromDict forwardFalse
    )


{-| Combine two dictionaries. If there is comparable1 collision, preference is given
to the first dictionary.
-}
union : MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
union (MultiBiDict left) (MultiBiDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    Dict.union left.forward right.forward
        |> fromDict


{-| Keep comparable1 key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
intersect (MultiBiDict left) (MultiBiDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    Dict.intersect left.forward right.forward
        |> fromDict


{-| Keep comparable1 key-value pair when its key does not appear in the second dictionary.
-}
diff : MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
diff (MultiBiDict left) (MultiBiDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    Dict.diff left.forward right.forward
        |> fromDict


{-| The most general way of combining two dictionaries. You provide three
accumulators for when comparable1 given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (comparable1 -> Set comparable21 -> acc -> acc)
    -> (comparable1 -> Set comparable21 -> Set comparable22 -> acc -> acc)
    -> (comparable1 -> Set comparable22 -> acc -> acc)
    -> MultiBiDict comparable1 comparable21
    -> MultiBiDict comparable1 comparable22
    -> acc
    -> acc
merge fnLeft fnBoth fnRight (MultiBiDict left) (MultiBiDict right) zero =
    Dict.merge fnLeft fnBoth fnRight left.forward right.forward zero
