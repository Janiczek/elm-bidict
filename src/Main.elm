module Main exposing (Foo)

import Dict exposing (Dict)


{-| remove this type parameter and it starts working
-}
type Foo a
    = Foo


{-| remove this annotation and it starts working
-}
toDict : Foo a -> Dict a b
toDict Foo =
    Dict.empty


fn value =
    let
        -- remove this type annotation and it starts working
        dict : Dict a b
        dict =
            toDict value
    in
    ()
