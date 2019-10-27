module BiDictTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import AssocList
import BiDict exposing (BiDict)
import BiDict.Assoc
import BiDict.AssocTest exposing (Msg(..))
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)


assocBiDictToBiDict : BiDict.Assoc.BiDict comparable1 comparable2 -> BiDict comparable1 comparable2
assocBiDictToBiDict assocBiDict =
    assocBiDict
        |> BiDict.Assoc.toDict
        |> AssocList.toList
        |> Dict.fromList
        |> BiDict.fromDict


msgToDictMsg : Msg -> BiDict String Int -> BiDict String Int
msgToDictMsg msg bidict =
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

        Union assocBidict2 ->
            BiDict.union bidict (assocBiDictToBiDict assocBidict2)

        Intersect assocBidict2 ->
            BiDict.intersect bidict (assocBiDictToBiDict assocBidict2)

        Diff assocBidict2 ->
            BiDict.diff bidict (assocBiDictToBiDict assocBidict2)


expectEqualToBidict : BiDict comparable1 comparable2 -> BiDict.Assoc.BiDict comparable1 comparable2 -> Expectation
expectEqualToBidict bidict assocBidict =
    List.sort (BiDict.toList bidict)
        |> Expect.equalLists (List.sort (BiDict.Assoc.toList assocBidict))


suite : Test
suite =
    describe "BiDict"
        [ invariantTest "behaves likes the Assoc variant" BiDict.AssocTest.app <|
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
