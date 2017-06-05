module Main exposing (..)

import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import PrimMaze exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


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
