module MultiBiDict exposing (MultiBiDict)

import Dict exposing (Dict)
import Set exposing (Set)


type MultiBiDict comparable1 comparable2
    = MultiBiDict
        { forward : Dict comparable1 (Set comparable2)
        , reverse : Dict comparable2 (Set comparable1)
        }
