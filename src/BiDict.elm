module BiDict exposing (BiDict)

import Dict exposing (Dict)
import Set exposing (Set)


type BiDict comparable1 comparable2
    = BiDict
        { dict : Dict comparable1 comparable2
        , inverse : Dict comparable2 (Set comparable1)
        }


empty : BiDict comparable1 comparable2
empty =
    BiDict
        { dict = Dict.empty
        , inverse = Dict.empty
        }


insert : comparable1 -> comparable2 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
insert from to (BiDict d) =
    BiDict
        { d
            | dict = Dict.insert from to d.dict
            , inverse = Dict.update to (Maybe.withDefault Set.empty >> Set.insert from >> Just) d.inverse
        }


remove : comparable1 -> BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
remove from (BiDict d) =
    BiDict
        { d
            | dict = Dict.remove from d.dict
            , inverse = Dict.map (Set.remove from) d.inverse
        }


toList : BiDict comparable1 comparable2 -> List ( comparable1, comparable2 )
toList (BiDict d) =
    Dict.toList d.dict


toInverseList : BiDict comparable1 comparable2 -> List ( comparable2, Set comparable1 )
toInverseList (BiDict d) =
    d.inverse
        |> Dict.filter (\_ v -> not (Set.isEmpty v))
        |> Dict.toList


keys : BiDict comparable1 comparable2 -> List comparable1
keys (BiDict d) =
    Dict.keys d.dict


values : BiDict comparable1 comparable2 -> List comparable2
values (BiDict d) =
    Dict.values d.dict
