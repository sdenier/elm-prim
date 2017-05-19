module Main exposing (..)

import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Random


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
    , output : String
    }


init : ( Model, Cmd Msg )
init =
    ( { size = 3, output = "" }, Cmd.none )


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
        Random.generate FirstSquare squareGen


type Msg
    = ChangeSize String
    | FirstSquare Square
    | GenMaze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSize size ->
            let
                newSize =
                    Result.withDefault model.size (String.toInt size)
            in
                ( { model | size = newSize }, Cmd.none )

        FirstSquare s ->
            let
                _ =
                    Debug.log ("toto" ++ toString s)
            in
                ( { model | output = (toString s) }, Cmd.none )

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
