module BiDict exposing (BiDict)

import Dict exposing (Dict)
import Set exposing (Set)


type BiDict comparable1 comparable2
    = BiDict
        { forward : Dict comparable1 comparable2
        , reverse : Dict comparable2 (Set comparable1)
        }


empty : BiDict comparable1 comparable2
empty =
    BiDict
        { forward = Dict.empty
        , reverse = Dict.empty
        }


insert : comparable1 -> comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
insert from to (BiDict d) =
    BiDict
        { d
            | forward = Dict.insert from to d.forward
            , reverse = Dict.update to (Maybe.withDefault Set.empty >> Set.insert from >> Just) d.reverse
        }


remove : comparable1 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
remove from (BiDict d) =
    BiDict
        { d
            | forward = Dict.remove from d.forward
            , reverse = Dict.map (\_ set -> Set.remove from set) d.reverse
        }


toList : BiDict comparable1 comparable2 -> List ( comparable1, comparable2 )
toList (BiDict d) =
    Dict.toList d.forward


toreverseList : BiDict comparable1 comparable2 -> List ( comparable2, Set comparable1 )
toreverseList (BiDict d) =
    d.reverse
        |> Dict.filter (\_ v -> not (Set.isEmpty v))
        |> Dict.toList


keys : BiDict comparable1 comparable2 -> List comparable1
keys (BiDict d) =
    Dict.keys d.forward


values : BiDict comparable1 comparable2 -> List comparable2
values (BiDict d) =
    Dict.values d.forward
