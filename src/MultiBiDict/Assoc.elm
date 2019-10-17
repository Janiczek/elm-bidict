module MultiBiDict.Assoc exposing (MultiBiDict)

import AssocList exposing (Dict)
import AssocSet exposing (Set)


type MultiBiDict a b
    = MultiBiDict
        { forward : Dict a (Set b)
        , reverse : Dict b (Set a)
        }
