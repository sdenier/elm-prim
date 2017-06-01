module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Random
import Random.Set as Set
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { size : Int
    , toVisit : Set Square
    , visited : DictWalls
    , output : String
    }


type alias DictWalls =
    Dict Square Walls


type alias Walls =
    List String


init : ( Model, Cmd Msg )
init =
    ( { size = 3, toVisit = Set.empty, visited = Dict.empty, output = "" }, Cmd.none )


type alias Square =
    ( Int, Int )


runPrim : Int -> Cmd Msg
runPrim mazeSize =
    let
        upperBound =
            mazeSize - 1

        squareGen =
            Random.pair (Random.int 0 upperBound) (Random.int 0 upperBound)
    in
        Random.generate PrimInit squareGen


findNeighbors : Int -> Square -> List Square
findNeighbors maxSize square =
    let
        ( x, y ) =
            square

        neighbors =
            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
    in
        List.filter (\( x, y ) -> x >= 0 && x < maxSize && y >= 0 && y < maxSize) neighbors


openWall : Square -> Square -> DictWalls -> Walls
openWall source target dictWalls =
    let
        sourceWalls =
            Dict.get source dictWalls |> Maybe.withDefault []

        ( sourceX, sourceY ) =
            source

        ( targetX, targetY ) =
            target
    in
        if sourceX > targetX then
            "W" :: sourceWalls
        else if sourceX < targetX then
            "E" :: sourceWalls
        else if sourceY > targetY then
            "N" :: sourceWalls
        else
            "S" :: sourceWalls


type Msg
    = ChangeSize String
    | GenMaze
    | PrimInit Square
    | PrimNextVisit (Maybe Square)
    | PrimOpenWall Square (Maybe Square)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSize size ->
            let
                newSize =
                    Result.withDefault model.size (String.toInt size)
            in
                ( { model | size = newSize }, Cmd.none )

        PrimInit square ->
            let
                _ =
                    Debug.log "PrimInit" (toString square)

                visited =
                    Dict.singleton square []

                toVisit =
                    Set.fromList (findNeighbors model.size square)

                newModel =
                    { model | toVisit = toVisit, visited = visited, output = (toString square) }

                nextStep =
                    Random.generate PrimNextVisit (Set.sample toVisit)
            in
                ( newModel, nextStep )

        PrimNextVisit (Just square) ->
            let
                _ =
                    Debug.log "PrimNextVisit" (toString square ++ " " ++ (toString visitedNeighbors) ++ " " ++ (toString toVisitNeighbors))

                visited =
                    Dict.insert square [] model.visited

                neighbors =
                    Set.fromList (findNeighbors model.size square)

                ( visitedNeighbors, toVisitNeighbors ) =
                    Set.partition (\s -> Dict.member s model.visited) neighbors

                toVisit =
                    Set.union (Set.remove square model.toVisit) toVisitNeighbors

                newModel =
                    { model | toVisit = toVisit, visited = visited, output = (toString square) }

                nextStep =
                    Random.generate (PrimOpenWall square) (Set.sample visitedNeighbors)
            in
                ( newModel, nextStep )

        PrimNextVisit Nothing ->
            let
                _ =
                    Debug.log "Next" "Done"
            in
                ( model, Cmd.none )

        PrimOpenWall sourceSquare (Just targetSquare) ->
            let
                _ =
                    Debug.log "PrimOpenWall" (toString sourceSquare ++ " -> " ++ toString targetSquare)

                sourceWalls =
                    openWall sourceSquare targetSquare model.visited

                targetWalls =
                    openWall targetSquare sourceSquare model.visited

                dictUpdate =
                    Dict.fromList [ ( sourceSquare, sourceWalls ), ( targetSquare, targetWalls ) ]

                visited =
                    Dict.union dictUpdate model.visited

                newModel =
                    { model | visited = visited }

                nextStep =
                    Random.generate PrimNextVisit (Set.sample model.toVisit)

                _ =
                    Debug.log "PrimOpenWall" visited
            in
                ( newModel, nextStep )

        PrimOpenWall _ Nothing ->
            let
                _ =
                    Debug.log "PrimOpenWall" "Something is wrong"
            in
                ( model, Cmd.none )

        GenMaze ->
            ( model, runPrim model.size )


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Maze Generator by Prim's Algorithm" ]
        , div []
            [ Html.label [] [ text "Maze Size:" ]
            , input [ onInput ChangeSize, value (toString model.size) ] []
            , Html.span [] [ text " " ]
            , button [ onClick GenMaze ] [ text "Run" ]
            ]
        , Html.hr [] []
        , div []
            [ textarea [ style [ ( "width", "500px" ), ( "height", "300px" ) ] ]
                [ text model.output
                ]
            ]
        ]
