module MultiDict.AssocTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Helpers exposing (..)
import MultiDict.Assoc as MultiDict exposing (MultiDict)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)



-- EXAMPLES


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



-- ARCHITECTURE TEST STUFF


type alias Model =
    MultiDict String Int


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
    , modelToString = MultiDict.toDict >> Debug.toString
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

        RemoveAll k ->
            MultiDict.removeAll k dict

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
            Dict.union dict (MultiDict.toDict dict2)

        Intersect dict2 ->
            Dict.intersect dict (MultiDict.toDict dict2)

        Diff dict2 ->
            Dict.diff dict (MultiDict.toDict dict2)


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
            (Fuzz.list (nonemptyAssocSetFuzzer valueFuzzer))
        ]


{-| Start with initModelFuzzer, add random Msgs, return the final model
-}
multiDictFuzzer : Fuzzer (MultiDict String Int)
multiDictFuzzer =
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


expectEqualToDict : Dict a (Set b) -> MultiDict a b -> Expectation
expectEqualToDict dict multiDict =
    MultiDict.toList multiDict
        |> Expect.equalLists (Dict.toList dict)


suite : Test
suite =
    describe "MultiDict.Assoc"
        [ describe "invariants" <|
            [ invariantTest "no empty sets" app <|
                \_ _ finalMultiDict ->
                    MultiDict.toList finalMultiDict
                        |> List.all (\( _, set ) -> not (Set.isEmpty set))
                        |> Expect.equal True
            ]
        , describe "toDict"
            [ invariantTest "have same toLists" app <|
                \_ _ finalMultiDict ->
                    MultiDict.toList finalMultiDict
                        |> Expect.equalLists (Dict.toList (MultiDict.toDict finalMultiDict))
            ]
        , test "fromFlatList doc example" <|
            \() ->
                MultiDict.fromFlatList
                    [ ( "foo", 1 )
                    , ( "bar", 2 )
                    , ( "foo", 3 )
                    ]
                    |> Expect.equal
                        (MultiDict.fromList
                            [ ( "foo", Set.fromList [ 1, 3 ] )
                            , ( "bar", Set.fromList [ 2 ] )
                            ]
                        )
        ]
