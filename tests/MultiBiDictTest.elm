module MultiBiDictTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList
import AssocSet
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import MultiBiDict exposing (MultiBiDict)
import MultiBiDict.Assoc
import MultiBiDict.AssocTest exposing (Msg(..))
import Set
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)


assocBiDictToBiDict : MultiBiDict.Assoc.MultiBiDict comparable1 comparable2 -> MultiBiDict comparable1 comparable2
assocBiDictToBiDict assocBiDict =
    assocBiDict
        |> MultiBiDict.Assoc.toDict
        |> AssocList.toList
        |> List.map (Tuple.mapSecond (AssocSet.toList >> Set.fromList))
        |> Dict.fromList
        |> MultiBiDict.fromDict


msgToDictMsg : Msg -> MultiBiDict String Int -> MultiBiDict String Int
msgToDictMsg msg multiBiDict =
    case msg of
        Insert k v ->
            MultiBiDict.insert k v multiBiDict

        UpdateAdd k v ->
            MultiBiDict.update k (Set.map ((+) v)) multiBiDict

        Remove k ->
            MultiBiDict.remove k multiBiDict

        MapAdd n ->
            MultiBiDict.map (\_ v -> v + n) multiBiDict

        FilterLessThan n ->
            MultiBiDict.filter (\_ set -> not <| Set.isEmpty <| Set.filter (\v -> v < n) set) multiBiDict

        Union assocBidict2 ->
            MultiBiDict.union multiBiDict (assocBiDictToBiDict assocBidict2)

        Intersect assocBidict2 ->
            MultiBiDict.intersect multiBiDict (assocBiDictToBiDict assocBidict2)

        Diff assocBidict2 ->
            MultiBiDict.diff multiBiDict (assocBiDictToBiDict assocBidict2)


expectEqualToBidict : MultiBiDict comparable1 comparable2 -> MultiBiDict.Assoc.MultiBiDict comparable1 comparable2 -> Expectation
expectEqualToBidict multiBiDict assocBidict =
    MultiBiDict.toList multiBiDict
        |> List.map (Tuple.mapSecond (Set.toList >> List.sort))
        |> List.sort
        |> Expect.equalLists
            (MultiBiDict.Assoc.toList assocBidict
                |> List.map (Tuple.mapSecond (AssocSet.toList >> List.sort))
                |> List.sort
            )


suite : Test
suite =
    describe "MultiBiDict"
        [ invariantTest "behaves likes the Assoc variant" MultiBiDict.AssocTest.app <|
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
