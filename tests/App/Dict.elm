module App.Dict exposing (Model, Msg(..), app, update)

import ArchitectureTest
import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Helpers exposing (..)
import Set exposing (Set)


type alias Model =
    Dict String Int


type Msg
    = Insert String Int
    | UpdateAdd String Int
    | Remove String
    | MapAdd Int
    | FilterLessThan Int
    | Union Model
    | Intersect Model
    | Diff Model


app : ArchitectureTest.TestedApp Model Msg
app =
    { model = ArchitectureTest.FuzzedModel initModelFuzzer
    , update = ArchitectureTest.UpdateWithoutCmds update
    , msgFuzzer = msgFuzzer
    , msgToString = Debug.toString
    , modelToString = Debug.toString
    }


update : Msg -> Model -> Model
update msg dict =
    case msg of
        Insert k v ->
            Dict.insert k v dict

        UpdateAdd k v ->
            Dict.update k (Maybe.map ((+) v)) dict

        Remove k ->
            Dict.remove k dict

        MapAdd n ->
            Dict.map (\_ v -> v + n) dict

        FilterLessThan n ->
            Dict.filter (\_ v -> v < n) dict

        Union dict2 ->
            Dict.union dict dict2

        Intersect dict2 ->
            Dict.intersect dict dict2

        Diff dict2 ->
            Dict.diff dict dict2


msgFuzzers =
    { insert = Fuzz.map2 Insert keyFuzzer valueFuzzer
    , updateAdd = Fuzz.map2 UpdateAdd keyFuzzer valueFuzzer
    , remove = Fuzz.map Remove keyFuzzer
    , mapAdd = Fuzz.map MapAdd valueFuzzer
    , filterLessThan = Fuzz.map FilterLessThan valueFuzzer
    , union = Fuzz.map Union initModelFuzzer
    , intersect = Fuzz.map Intersect initModelFuzzer
    , diff = Fuzz.map Diff initModelFuzzer
    }


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.oneOf
        [ msgFuzzers.insert
        , msgFuzzers.updateAdd
        , msgFuzzers.remove
        , msgFuzzers.mapAdd
        , msgFuzzers.filterLessThan
        , msgFuzzers.union
        , msgFuzzers.intersect
        , msgFuzzers.diff
        ]


initModelFuzzer : Fuzzer Model
initModelFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Dict.empty
        , Fuzz.constant fullDict
        , Fuzz.constant oneWayDict
        , Fuzz.constant otherWayDict
        , Fuzz.map2
            (\keys values ->
                let
                    uniqueKeys =
                        keys
                            |> Set.fromList
                            |> Set.toList
                in
                Dict.fromList
                    (List.map2 Tuple.pair
                        uniqueKeys
                        values
                    )
            )
            (Fuzz.list keyFuzzer)
            (Fuzz.list valueFuzzer)
        ]


valueFuzzer : Fuzzer Int
valueFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant 1
        , Fuzz.constant 2
        , Fuzz.constant 3
        , Fuzz.constant 4
        , Fuzz.int
        ]


keyFuzzer : Fuzzer String
keyFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "foo"
        , Fuzz.constant "bar"
        , Fuzz.constant "baz"
        , Fuzz.constant "quux"
        , Fuzz.string
        ]


fullDict : Dict String Int
fullDict =
    Dict.empty
        |> Dict.insert "A" 1
        |> Dict.insert "B" 2
        |> Dict.insert "A" 3
        |> Dict.insert "D" 2


oneWayDict : Dict String Int
oneWayDict =
    Dict.empty
        |> Dict.insert "A" 1
        |> Dict.insert "B" 2
        |> Dict.insert "A" 3
        |> Dict.insert "D" 4


otherWayDict : Dict String Int
otherWayDict =
    Dict.empty
        |> Dict.insert "A" 1
        |> Dict.insert "B" 2
        |> Dict.insert "C" 3
        |> Dict.insert "D" 2
