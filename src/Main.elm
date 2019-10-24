module Main exposing (BiDict, toDict)

import Dict exposing (Dict)


type BiDict a
    = BiDict


{-| remove this annotation and it starts working
-}
toDict : BiDict a -> Dict a b
toDict BiDict =
    Dict.empty


fn value =
    let
        -- remove this type annotation and it starts working
        initialDict : Dict a b
        initialDict =
            toDict value
    in
    ()
