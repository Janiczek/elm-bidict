module Main exposing (BiDict, toDict)

import AssocList as Dict exposing (Dict)


type BiDict a
    = BiDict


toDict : BiDict a -> Dict a b
toDict BiDict =
    Dict.empty


fn value =
    let
        initialDict : Dict a b
        initialDict =
            toDict value

        finalDict : Dict a b
        finalDict =
            List.foldl
                (\dict msgFn -> msgFn dict)
                initialDict
                []
    in
    ()
