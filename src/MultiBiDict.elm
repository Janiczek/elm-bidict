module MultiBiDict exposing (MultiBiDict)

import Dict exposing (Dict)
import Set exposing (Set)


type MultiBiDict comparable1 comparable2
    = MultiBiDict
        { dict : Dict comparable1 (Set comparable2)
        , inverse : Dict comparable2 (Set comparable1)
        }
