module Tests exposing (..)

import Test exposing (..)
import Expect
import PrimMaze exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


all : Test
all =
    describe "Maze Generation with Prim's Algorithm"
        [ test_findNeighbors
        , test_openWall
        , test_updateOpenings
        , test_exportJsonOutput
        ]


test_findNeighbors : Test
test_findNeighbors =
    describe "findNeighbors"
        [ test "gives the 4 neighbours around the give square" <|
            \() ->
                let
                    expected =
                        Set.fromList [ ( 1, 1 ), ( 3, 1 ), ( 2, 0 ), ( 2, 2 ) ]
                in
                    (findNeighbors 4 ( 2, 1 )) |> Expect.equal expected
        , test "excludes neighbours out of maze bounds" <|
            \() ->
                let
                    expected =
                        Set.fromList [ ( 1, 0 ), ( 0, 1 ) ]
                in
                    (findNeighbors 4 ( 0, 0 )) |> Expect.equal expected
        , test "excludes neighbours out of maze bounds" <|
            \() ->
                let
                    expected =
                        Set.fromList [ ( 2, 3 ), ( 3, 2 ) ]
                in
                    (findNeighbors 4 ( 3, 3 )) |> Expect.equal expected
        ]


test_openWall : Test
test_openWall =
    describe "openWall"
        [ test "returns W[est] when target is on the left" <|
            \() ->
                Expect.equal [ "W" ] (openWall ( 1, 1 ) ( 0, 1 ) Dict.empty)
        , test "returns E[ast] when target is on the right" <|
            \() ->
                Expect.equal [ "E" ] (openWall ( 1, 1 ) ( 2, 1 ) Dict.empty)
        , test "returns N[orth] when target is above" <|
            \() ->
                Expect.equal [ "N" ] (openWall ( 1, 1 ) ( 1, 0 ) Dict.empty)
        , test "returns S[outh] when target is below" <|
            \() ->
                Expect.equal [ "S" ] (openWall ( 1, 1 ) ( 1, 2 ) Dict.empty)
        , test "adds the new opening to the existing list" <|
            \() ->
                let
                    source =
                        ( 1, 1 )

                    openings =
                        Dict.singleton source [ "W" ]
                in
                    Expect.equal [ "S", "W" ] (openWall source ( 1, 2 ) openings)
        ]


test_updateOpenings : Test
test_updateOpenings =
    describe "updateOpenings"
        [ test "opens walls on both sides" <|
            \() ->
                let
                    source =
                        ( 1, 1 )

                    target =
                        ( 2, 1 )

                    openings =
                        Dict.empty

                    expectedOpenings =
                        Dict.fromList [ ( source, [ "E" ] ), ( target, [ "W" ] ) ]
                in
                    Expect.equal expectedOpenings (updateOpenings source target openings)
        , test "augments the existing openings" <|
            \() ->
                let
                    source =
                        ( 1, 1 )

                    target =
                        ( 2, 1 )

                    openings =
                        Dict.fromList [ ( source, [ "N" ] ), ( ( 1, 0 ), [ "S" ] ) ]

                    expectedOpenings =
                        Dict.fromList [ ( source, [ "E", "N" ] ), ( target, [ "W" ] ), ( ( 1, 0 ), [ "S" ] ) ]
                in
                    Expect.equal expectedOpenings (updateOpenings source target openings)
        ]


test_exportJsonOutput : Test
test_exportJsonOutput =
    describe "exportJsonOutput"
        [ test "exports maze data structure as Json" <|
            \() ->
                let
                    expectedOutput =
                        "{\"0,0\":[\"E\",\"S\"],\"0,1\":[\"N\",\"E\"],\"1,0\":[\"W\"],\"1,1\":[\"W\"]}"

                    mazeOpenings =
                        Dict.fromList
                            [ ( ( 0, 0 ), [ "E", "S" ] )
                            , ( ( 1, 0 ), [ "W" ] )
                            , ( ( 0, 1 ), [ "N", "E" ] )
                            , ( ( 1, 1 ), [ "W" ] )
                            ]
                in
                    Expect.equal expectedOutput (exportJsonOutput mazeOpenings)
        ]
