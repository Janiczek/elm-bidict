module MultiBiDict.AssocTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import MultiBiDict.Assoc as MultiBiDict exposing (MultiBiDict)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)



-- EXAMPLES


fullDict : MultiBiDict String Int
fullDict =
    MultiBiDict.empty
        |> MultiBiDict.insert "A" 1
        |> MultiBiDict.insert "B" 2
        |> MultiBiDict.insert "A" 3
        |> MultiBiDict.insert "D" 2


oneWayDict : MultiBiDict String Int
oneWayDict =
    MultiBiDict.empty
        |> MultiBiDict.insert "A" 1
        |> MultiBiDict.insert "B" 2
        |> MultiBiDict.insert "A" 3
        |> MultiBiDict.insert "D" 4


otherWayDict : MultiBiDict String Int
otherWayDict =
    MultiBiDict.empty
        |> MultiBiDict.insert "A" 1
        |> MultiBiDict.insert "B" 2
        |> MultiBiDict.insert "C" 3
        |> MultiBiDict.insert "D" 2



-- ARCHITECTURE TEST STUFF


type alias Model =
    MultiBiDict String Int


type Msg
    = Insert String Int
    | UpdateAdd String Int
    | RemoveAll String
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
    , modelToString = modelToString
    }


update : Msg -> Model -> Model
update msg dict =
    case msg of
        Insert k v ->
            MultiBiDict.insert k v dict

        UpdateAdd k v ->
            MultiBiDict.update k (Set.map ((+) v)) dict

        Remove k v ->
            MultiBiDict.remove k v dict

        RemoveAll k ->
            MultiBiDict.removeAll k dict

        MapAdd n ->
            MultiBiDict.map (\_ v -> v + n) dict

        FilterLessThan n ->
            MultiBiDict.filter (\_ v -> v < n) dict

        Union dict2 ->
            MultiBiDict.union dict dict2

        Intersect dict2 ->
            MultiBiDict.intersect dict dict2

        Diff dict2 ->
            MultiBiDict.diff dict dict2


msgToDictMsg : Msg -> Dict String (Set Int) -> Dict String (Set Int)
msgToDictMsg msg dict =
    case msg of
        Insert k v ->
            Dict.update k
                (\maybeVal ->
                    case maybeVal of
                        Nothing ->
                            Just <| Set.singleton v

                        Just set ->
                            Just <| Set.insert v set
                )
                dict

        UpdateAdd k v ->
            Dict.update k (Maybe.map (Set.map ((+) v))) dict

        RemoveAll k ->
            Dict.remove k dict

        Remove k _ ->
            Dict.remove k dict

        MapAdd n ->
            Dict.map (\_ set -> Set.map (\v -> v + n) set) dict

        FilterLessThan n ->
            Dict.filter (\_ set -> not <| Set.isEmpty <| Set.filter (\v -> v < n) set) dict

        Union dict2 ->
            Dict.union dict (MultiBiDict.toDict dict2)

        Intersect dict2 ->
            Dict.intersect dict (MultiBiDict.toDict dict2)

        Diff dict2 ->
            Dict.diff dict (MultiBiDict.toDict dict2)


modelToString : Model -> String
modelToString multibidict =
    let
        forward =
            MultiBiDict.toList multibidict

        reverse =
            MultiBiDict.toReverseList multibidict
    in
    [ "MultiBiDict"
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
        [ Fuzz.constant MultiBiDict.empty
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
                MultiBiDict.fromList
                    (List.map2 Tuple.pair
                        uniqueKeys
                        values
                    )
            )
            (Fuzz.list keyFuzzer)
            (Fuzz.list (nonemptySetFuzzer valueFuzzer))
        ]


nonemptySetFuzzer : Fuzzer comparable -> Fuzzer (Set comparable)
nonemptySetFuzzer fuzzer =
    Fuzz.map2 (::) fuzzer (Fuzz.list fuzzer)
        |> Fuzz.map Set.fromList


{-| Start with initModelFuzzer, add random Msgs, return the final model
-}
multibidictFuzzer : Fuzzer (MultiBiDict String Int)
multibidictFuzzer =
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
    , remove = Fuzz.map2 Remove keyFuzzer valueFuzzer
    , removeAll = Fuzz.map RemoveAll keyFuzzer
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
        , msgFuzzers.removeAll
        , msgFuzzers.mapAdd
        , msgFuzzers.filterLessThan
        , msgFuzzers.union
        , msgFuzzers.intersect
        , msgFuzzers.diff
        ]



-- TESTS


expectEqualToDict : Dict a (Set b) -> MultiBiDict a b -> Expectation
expectEqualToDict dict multibidict =
    MultiBiDict.toList multibidict
        |> Expect.equalLists (Dict.toList dict)


suite : Test
suite =
    describe "MultiBiDict.Assoc"
        [ describe "invariants" <|
            [ invariantTest "reverse dict reflects forward dict properly" app <|
                \_ _ finalMultibidict ->
                    MultiBiDict.size finalMultibidict
                        |> Expect.equal
                            (finalMultibidict
                                |> MultiBiDict.toReverseList
                                |> List.foldl (\( _, set ) acc -> Set.size set + acc) 0
                            )
            , invariantTest "no empty sets" app <|
                \_ _ finalMultibidict ->
                    MultiBiDict.toList finalMultibidict
                        |> List.all (\( _, set ) -> not (Set.isEmpty set))
                        |> Expect.equal True
            , invariantTest "no reverrse empty sets" app <|
                \_ _ finalMultibidict ->
                    MultiBiDict.toReverseList finalMultibidict
                        |> List.all (\( _, set ) -> not (Set.isEmpty set))
                        |> Expect.equal True
            ]
        , describe "toDict"
            [ invariantTest "have same toLists" app <|
                \_ _ finalMultibidict ->
                    MultiBiDict.toList finalMultibidict
                        |> Expect.equalLists (Dict.toList (MultiBiDict.toDict finalMultibidict))
            ]
        , describe "getReverse"
            [ test "empty" <|
                \() ->
                    MultiBiDict.getReverse 1 MultiBiDict.empty
                        |> Expect.equal Set.empty
            , test "fullDict" <|
                \() ->
                    MultiBiDict.getReverse 2 fullDict
                        |> Expect.equal (Set.fromList [ "D", "B" ])
            , test "oneWayDict" <|
                \() ->
                    MultiBiDict.getReverse 1 oneWayDict
                        |> Expect.equal (Set.singleton "A")
            , test "otherWayDict" <|
                \() ->
                    MultiBiDict.getReverse 2 otherWayDict
                        |> Expect.equal (Set.fromList [ "D", "B" ])
            ]
        , describe "uniqueValues"
            [ test "empty" <|
                \() ->
                    MultiBiDict.uniqueValues MultiBiDict.empty
                        |> Expect.equal []
            , test "fullDict" <|
                \() ->
                    MultiBiDict.uniqueValues fullDict
                        |> List.sort
                        |> Expect.equal [ 1, 2, 3 ]
            , test "oneWayDict" <|
                \() ->
                    MultiBiDict.uniqueValues oneWayDict
                        |> List.sort
                        |> Expect.equal [ 1, 2, 3, 4 ]
            , test "otherWayDict" <|
                \() ->
                    MultiBiDict.uniqueValues otherWayDict
                        |> List.sort
                        |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "uniqueValuesCount"
            [ invariantTest "is count of uniqueValues" app <|
                \_ _ finalMultibidict ->
                    MultiBiDict.uniqueValuesCount finalMultibidict
                        |> Expect.equal (List.length (MultiBiDict.uniqueValues finalMultibidict))
            ]
        , describe "toReverseList"
            [ test "empty" <|
                \() ->
                    MultiBiDict.toReverseList MultiBiDict.empty
                        |> Expect.equal []
            , test "fullDict" <|
                \() ->
                    MultiBiDict.toReverseList fullDict
                        |> List.sortBy Tuple.first
                        |> Expect.equal
                            [ ( 1, Set.singleton "A" )
                            , ( 2, Set.fromList [ "D", "B" ] )
                            , ( 3, Set.singleton "A" )
                            ]
            , test "oneWayDict" <|
                \() ->
                    MultiBiDict.toReverseList oneWayDict
                        |> List.sortBy Tuple.first
                        |> Expect.equal
                            [ ( 1, Set.singleton "A" )
                            , ( 2, Set.singleton "B" )
                            , ( 3, Set.singleton "A" )
                            , ( 4, Set.singleton "D" )
                            ]
            , test "otherWayDict" <|
                \() ->
                    MultiBiDict.toReverseList otherWayDict
                        |> List.sortBy Tuple.first
                        |> Expect.equal
                            [ ( 1, Set.singleton "A" )
                            , ( 2, Set.fromList [ "D", "B" ] )
                            , ( 3, Set.singleton "C" )
                            ]
            ]
        ]
