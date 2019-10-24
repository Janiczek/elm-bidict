module BiDict.AssocTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList as Dict exposing (Dict)
import AssocSet as Set
import BiDict.Assoc as BiDict exposing (BiDict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)



-- EXAMPLES


bijectiveBidict : BiDict String Int
bijectiveBidict =
    BiDict.empty
        |> BiDict.insert "A" 1
        |> BiDict.insert "B" 2
        |> BiDict.insert "C" 3
        |> BiDict.insert "D" 4


nonInjectiveBidict : BiDict String Int
nonInjectiveBidict =
    BiDict.empty
        |> BiDict.insert "A" 1
        |> BiDict.insert "B" 2
        |> BiDict.insert "C" 1
        |> BiDict.insert "D" 4



-- ARCHITECTURE TEST STUFF


type alias Model =
    BiDict String Int


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
    , modelToString = modelToString
    }


update : Msg -> Model -> Model
update msg bidict =
    case msg of
        Insert k v ->
            BiDict.insert k v bidict

        UpdateAdd k v ->
            BiDict.update k (Maybe.map ((+) v)) bidict

        Remove k ->
            BiDict.remove k bidict

        MapAdd n ->
            BiDict.map (\_ v -> v + n) bidict

        FilterLessThan n ->
            BiDict.filter (\_ v -> v < n) bidict

        Union bidict2 ->
            BiDict.union bidict bidict2

        Intersect bidict2 ->
            BiDict.intersect bidict bidict2

        Diff bidict2 ->
            BiDict.diff bidict bidict2


msgToDictMsg : Msg -> Dict a b -> Dict a b
msgToDictMsg msg dict =
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

        Union bidict2 ->
            Dict.union dict (BiDict.toDict bidict2)

        Intersect bidict2 ->
            Dict.intersect dict (BiDict.toDict bidict2)

        Diff bidict2 ->
            Dict.diff dict (BiDict.toDict bidict2)


modelToString : Model -> String
modelToString bidict =
    let
        forward =
            BiDict.toList bidict

        reverse =
            BiDict.toReverseList bidict
    in
    [ "BiDict"
    , ""
    , "      forward ="
    , forward
        |> List.map (\( from, to ) -> "        " ++ Debug.toString from ++ " -> " ++ Debug.toString to)
        |> String.join "\n"
    , ""
    , "      reverse ="
    , reverse
        |> List.map (\( to, froms ) -> "        " ++ Debug.toString to ++ " <- " ++ Debug.toString (Set.toList froms))
        |> String.join "\n"
    ]
        |> String.join "\n"


initModelFuzzer : Fuzzer Model
initModelFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant BiDict.empty
        , Fuzz.constant bijectiveBidict
        , Fuzz.constant nonInjectiveBidict
        , Fuzz.map2
            (\keys values ->
                let
                    uniqueKeys =
                        keys
                            |> Set.fromList
                            |> Set.toList
                in
                BiDict.fromList
                    (List.map2 Tuple.pair
                        uniqueKeys
                        values
                    )
            )
            (Fuzz.list keyFuzzer)
            (Fuzz.list valueFuzzer)
        ]


{-| Start with initModelFuzzer, add random Msgs, return the final model
-}
bidictFuzzer : Fuzzer (BiDict String Int)
bidictFuzzer =
    ArchitectureTest.modelFuzzer app


keyFuzzer : Fuzzer String
keyFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "foo"
        , Fuzz.constant "bar"
        , Fuzz.constant "baz"
        , Fuzz.constant "quux"
        , Fuzz.string
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



-- TESTS


expectEqualToDict : Dict a b -> BiDict a b -> Expectation
expectEqualToDict dict bidict =
    BiDict.toList bidict
        |> Expect.equalLists (Dict.toList dict)


suite : Test
suite =
    describe "BiDict.Assoc"
        [ describe "invariants" <|
            [ invariantTest "reverse dict reflects forward dict properly" app <|
                \_ _ finalBidict ->
                    BiDict.size finalBidict
                        |> Expect.equal
                            (finalBidict
                                |> BiDict.toReverseList
                                |> List.foldl (\( _, set ) acc -> Set.size set + acc) 0
                            )
            , invariantTest "no empty sets" app <|
                \_ _ finalBidict ->
                    BiDict.toReverseList finalBidict
                        |> List.all (\( _, set ) -> not (Set.isEmpty set))
                        |> Expect.true ""
            , invariantTest "behaves like dict" app <|
                \initialBidict msgs finalBidict ->
                    let
                        initialDict : Dict a b
                        initialDict =
                            BiDict.toDict initialBidict

                        dictMsgFns : List (Dict a b -> Dict a b)
                        dictMsgFns =
                            List.map msgToDictMsg msgs

                        finalDict : Dict a b
                        finalDict =
                            List.foldl
                                (\dict msgFn -> msgFn dict)
                                initialDict
                                dictMsgFns
                    in
                    1
            ]
        , describe "empty"
            [ test "has size 0" <|
                \() ->
                    BiDict.size BiDict.empty
                        |> Expect.equal 0
            ]

        -- TODO "getReverse"
        -- TODO "sizeReverse"
        -- TODO "uniqueValues"
        -- TODO "toReverseList"
        ]
