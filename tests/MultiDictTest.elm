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


assocBiDictToBiDict : MultiDict.Assoc.MultiDict comparable1 comparable2 -> MultiDict comparable1 comparable2
assocBiDictToBiDict assocBiDict =
    assocBiDict
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
            MultiDict.filter (\_ set -> not <| Set.isEmpty <| Set.filter (\v -> v < n) set) multiDict

        Union assocBidict2 ->
            MultiDict.union multiDict (assocBiDictToBiDict assocBidict2)

        Intersect assocBidict2 ->
            MultiDict.intersect multiDict (assocBiDictToBiDict assocBidict2)

        Diff assocBidict2 ->
            MultiDict.diff multiDict (assocBiDictToBiDict assocBidict2)


expectEqualToBidict : MultiDict comparable1 comparable2 -> MultiDict.Assoc.MultiDict comparable1 comparable2 -> Expectation
expectEqualToBidict multiDict assocBidict =
    MultiDict.toList multiDict
        |> List.map (Tuple.mapSecond (Set.toList >> List.sort))
        |> List.sort
        |> Expect.equalLists
            (MultiDict.Assoc.toList assocBidict
                |> List.map (Tuple.mapSecond (AssocSet.toList >> List.sort))
                |> List.sort
            )


suite : Test
suite =
    describe "MultiDict"
        [ invariantTest "behaves likes the Assoc variant" MultiDict.AssocTest.app <|
            \initialAssocBidict msgs finalAssocBidict ->
                let
                    initialBidict =
                        assocBiDictToBiDict initialAssocBidict

                    dictMsgFns =
                        List.map msgToDictMsg msgs

                    finalBidict =
                        List.foldl
                            (\msgFn dict -> msgFn dict)
                            initialBidict
                            dictMsgFns
                in
                finalAssocBidict
                    |> expectEqualToBidict finalBidict
        ]
