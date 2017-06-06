module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import PrimMaze exposing (..)
import Set exposing (Set)


all : Test
all =
    describe "Maze Generation with Prim's Algorithm"
        [ describe "squareIdString"
            [ test "transforms square coordinates into an id string" <|
                \() ->
                    Expect.equal "3,7" (squareIdString ( 3, 7 ))
            , fuzz2 int int "concatenates both coordinates to create the id" <|
                \x y ->
                    (squareIdString ( x, y )) |> Expect.equal ((toString x) ++ "," ++ (toString y))
            ]
        , describe "findNeighbors"
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
        ]
