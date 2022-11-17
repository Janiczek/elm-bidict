module App.MultiDict exposing (Model, Msg(..), app, msgFuzzers, update)

import ArchitectureTest
import Fuzz exposing (Fuzzer)
import Helpers exposing (..)
import MultiDict exposing (MultiDict)
import Set exposing (Set)


type alias Model =
    MultiDict String Int


type Msg
    = Insert String Int
    | UpdateAdd String Int
    | Remove String Int
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
            MultiDict.insert k v dict

        UpdateAdd k v ->
            MultiDict.update k (Set.map ((+) v)) dict

        Remove k v ->
            MultiDict.remove k v dict

        MapAdd n ->
            MultiDict.map (\_ v -> v + n) dict

        FilterLessThan n ->
            MultiDict.filter (\_ v -> v < n) dict

        Union dict2 ->
            MultiDict.union dict dict2

        Intersect dict2 ->
            MultiDict.intersect dict dict2

        Diff dict2 ->
            MultiDict.diff dict dict2


msgFuzzers =
    { insert = Fuzz.map2 Insert keyFuzzer valueFuzzer
    , updateAdd = Fuzz.map2 UpdateAdd keyFuzzer valueFuzzer
    , remove = Fuzz.map2 Remove keyFuzzer valueFuzzer
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
        [ Fuzz.constant MultiDict.empty
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
                MultiDict.fromList
                    (List.map2 Tuple.pair
                        uniqueKeys
                        values
                    )
            )
            (Fuzz.list keyFuzzer)
            (Fuzz.list (nonemptySetFuzzer valueFuzzer))
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


fullDict : MultiDict String Int
fullDict =
    MultiDict.empty
        |> MultiDict.insert "A" 1
        |> MultiDict.insert "B" 2
        |> MultiDict.insert "A" 3
        |> MultiDict.insert "D" 2


oneWayDict : MultiDict String Int
oneWayDict =
    MultiDict.empty
        |> MultiDict.insert "A" 1
        |> MultiDict.insert "B" 2
        |> MultiDict.insert "A" 3
        |> MultiDict.insert "D" 4


otherWayDict : MultiDict String Int
otherWayDict =
    MultiDict.empty
        |> MultiDict.insert "A" 1
        |> MultiDict.insert "B" 2
        |> MultiDict.insert "C" 3
        |> MultiDict.insert "D" 2
