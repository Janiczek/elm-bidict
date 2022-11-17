module MultiDictTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList
import AssocSet
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import MultiDict exposing (MultiDict)
import MultiDict.Assoc
import MultiDict.AssocTest exposing (Msg(..))
import Set
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)


assocMultiDictToMultiDict : MultiDict.Assoc.MultiDict comparable1 comparable2 -> MultiDict comparable1 comparable2
assocMultiDictToMultiDict assocMultiDict =
    assocMultiDict
        |> MultiDict.Assoc.toDict
        |> AssocList.toList
        |> List.map (Tuple.mapSecond (AssocSet.toList >> Set.fromList))
        |> Dict.fromList
        |> MultiDict.fromDict


msgToDictMsg : Msg -> MultiDict String Int -> MultiDict String Int
msgToDictMsg msg multiDict =
    case msg of
        Insert k v ->
            MultiDict.insert k v multiDict

        UpdateAdd k v ->
            MultiDict.update k (Set.map ((+) v)) multiDict

        Remove k v ->
            MultiDict.remove k v multiDict

        RemoveAll k ->
            MultiDict.removeAll k multiDict

        MapAdd n ->
            MultiDict.map (\_ v -> v + n) multiDict

        FilterLessThan n ->
            MultiDict.filter (\_ v -> v < n) multiDict

        Union assocMultidict2 ->
            MultiDict.union multiDict (assocMultiDictToMultiDict assocMultidict2)

        Intersect assocMultidict2 ->
            MultiDict.intersect multiDict (assocMultiDictToMultiDict assocMultidict2)

        Diff assocMultidict2 ->
            MultiDict.diff multiDict (assocMultiDictToMultiDict assocMultidict2)


expectEqualToMultiDict : MultiDict comparable1 comparable2 -> MultiDict.Assoc.MultiDict comparable1 comparable2 -> Expectation
expectEqualToMultiDict multiDict assocMultidict =
    MultiDict.toList multiDict
        |> List.map (Tuple.mapSecond (Set.toList >> List.sort))
        |> List.sort
        |> Expect.equalLists
            (MultiDict.Assoc.toList assocMultidict
                |> List.map (Tuple.mapSecond (AssocSet.toList >> List.sort))
                |> List.sort
            )


suite : Test
suite =
    describe "MultiDict"
        [ invariantTest "behaves likes the Assoc variant" MultiDict.AssocTest.app <|
            \initialAssocMultidict msgs finalAssocMultidict ->
                let
                    initialMultidict =
                        assocMultiDictToMultiDict initialAssocMultidict

                    dictMsgFns =
                        List.map msgToDictMsg msgs

                    finalMultidict =
                        List.foldl
                            (\msgFn dict -> msgFn dict)
                            initialMultidict
                            dictMsgFns
                in
                finalAssocMultidict
                    |> expectEqualToMultiDict finalMultidict
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
        , test "empty isEmpty" <|
            \() ->
                MultiDict.empty
                    |> MultiDict.isEmpty
                    |> Expect.equal True
        , test "singleton is not empty" <|
            \() ->
                MultiDict.singleton 1 2
                    |> MultiDict.isEmpty
                    |> Expect.equal False
        ]
