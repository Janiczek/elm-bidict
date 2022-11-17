module MultiDictTest exposing (..)

import App.Dict
import App.MultiDict
import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList
import AssocSet
import Dict exposing (Dict)
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


toFlattenedList : MultiDict String Int -> List ( String, Int )
toFlattenedList dict =
    dict
        |> MultiDict.toList
        |> List.concatMap
            (\( k, set ) ->
                set
                    |> Set.toList
                    |> List.map (\v -> ( k, v ))
            )


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


expectEqualToAssocMultiDict : MultiDict comparable1 comparable2 -> MultiDict.Assoc.MultiDict comparable1 comparable2 -> Expectation
expectEqualToAssocMultiDict multiDict assocMultidict =
    MultiDict.toList multiDict
        |> List.map (Tuple.mapSecond (Set.toList >> List.sort))
        |> List.sort
        |> Expect.equalLists
            (MultiDict.Assoc.toList assocMultidict
                |> List.map (Tuple.mapSecond (AssocSet.toList >> List.sort))
                |> List.sort
            )


dictToMultiDict : Dict String Int -> MultiDict String Int
dictToMultiDict dict =
    dict
        |> Dict.toList
        |> List.map (Tuple.mapSecond Set.singleton)
        |> MultiDict.fromList


runDictMsgOnMultiDict : App.Dict.Msg -> MultiDict String Int -> MultiDict String Int
runDictMsgOnMultiDict msg multiDict =
    let
        withoutCollision k d =
            if MultiDict.member k d then
                MultiDict.removeAll k d

            else
                d
    in
    case msg of
        App.Dict.Insert k v ->
            MultiDict.insert k v (withoutCollision k multiDict)

        App.Dict.UpdateAdd k v ->
            MultiDict.update k (Set.map ((+) v)) multiDict

        App.Dict.Remove k ->
            MultiDict.removeAll k multiDict

        App.Dict.MapAdd n ->
            MultiDict.map (\_ v -> v + n) multiDict

        App.Dict.FilterLessThan n ->
            MultiDict.filter (\_ v -> v < n) multiDict

        App.Dict.Union dict2 ->
            MultiDict.union multiDict (dictToMultiDict dict2)

        App.Dict.Intersect dict2 ->
            MultiDict.intersect multiDict (dictToMultiDict dict2)

        App.Dict.Diff dict2 ->
            MultiDict.diff multiDict (dictToMultiDict dict2)


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
                    |> expectEqualToAssocMultiDict finalMultidict
        , invariantTest "behaves like a normal Dict if we don't add multiple items to a key"
            App.Dict.app
          <|
            \initialDict msgs finalDict ->
                let
                    initialMultidict : MultiDict String Int
                    initialMultidict =
                        dictToMultiDict initialDict

                    finalMultidict : MultiDict String Int
                    finalMultidict =
                        List.foldl
                            runDictMsgOnMultiDict
                            initialMultidict
                            msgs

                    multiDictList : List ( String, Int )
                    multiDictList =
                        toFlattenedList finalMultidict

                    dictList : List ( String, Int )
                    dictList =
                        Dict.toList finalDict
                in
                multiDictList
                    |> Expect.equalLists dictList
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
        , invariantTest "MultiDict.size == List.length of flattened dict" App.MultiDict.app <|
            \_ _ multiDict ->
                MultiDict.size multiDict
                    |> Expect.equal
                        (multiDict
                            |> toFlattenedList
                            |> List.length
                        )
        ]
