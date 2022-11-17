module Helpers exposing
    ( nonemptyAssocSetFuzzer
    , nonemptySetFuzzer
    )

import AssocSet
import Fuzz exposing (Fuzzer)
import Set exposing (Set)


nonemptySetFuzzer : Fuzzer comparable -> Fuzzer (Set comparable)
nonemptySetFuzzer fuzzer =
    Fuzz.map2 (::) fuzzer (Fuzz.list fuzzer)
        |> Fuzz.map Set.fromList


nonemptyAssocSetFuzzer : Fuzzer comparable -> Fuzzer (AssocSet.Set comparable)
nonemptyAssocSetFuzzer fuzzer =
    Fuzz.map2 (::) fuzzer (Fuzz.list fuzzer)
        |> Fuzz.map AssocSet.fromList
