module PrimMaze exposing (..)

import Dict exposing (Dict)
import Json.Encode as Json
import Random
import Random.Set as Set
import Set exposing (Set)


type alias Square =
    ( Int, Int )


type alias Openings =
    List String


type alias DictOpenings =
    Dict Square Openings


type alias Event =
    String


type alias Model =
    { size : Int
    , toVisit : Set Square
    , visited : DictOpenings
    , output : String
    , history : List Event
    }


init : ( Model, Cmd Msg )
init =
    ( { size = 3, toVisit = Set.empty, visited = Dict.empty, output = "", history = [] }, Cmd.none )


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

        GenMaze ->
            ( model, runPrim model.size )

        PrimInit square ->
            let
                history =
                    [ eventInit square model ]

                visited =
                    Dict.singleton square []

                toVisit =
                    findNeighbors model.size square

                newModel =
                    { model | toVisit = toVisit, visited = visited, history = history }

                nextStep =
                    Random.generate PrimNextVisit (Set.sample toVisit)
            in
                ( newModel, nextStep )

        PrimNextVisit (Just square) ->
            let
                history =
                    List.append model.history [ eventNextVisit square model ]

                visited =
                    Dict.insert square [] model.visited

                neighbors =
                    findNeighbors model.size square

                ( visitedNeighbors, toVisitNeighbors ) =
                    Set.partition (\s -> Dict.member s model.visited) neighbors

                toVisit =
                    Set.union (Set.remove square model.toVisit) toVisitNeighbors

                newModel =
                    { model | toVisit = toVisit, visited = visited, history = history }

                nextStep =
                    Random.generate (PrimOpenWall square) (Set.sample visitedNeighbors)
            in
                ( newModel, nextStep )

        PrimNextVisit Nothing ->
            let
                history =
                    List.append model.history [ "Done" ]

                output =
                    exportJsonOutput model.visited
            in
                ( { model | output = output, history = history }, Cmd.none )

        PrimOpenWall sourceSquare (Just targetSquare) ->
            let
                history =
                    List.append model.history [ eventOpenWall sourceSquare targetSquare ]

                visited =
                    updateOpenings sourceSquare targetSquare model.visited

                nextStep =
                    Random.generate PrimNextVisit (Set.sample model.toVisit)
            in
                ( { model | visited = visited, history = history }, nextStep )

        PrimOpenWall _ Nothing ->
            let
                _ =
                    Debug.log "PrimOpenWall" "Something is wrong"
            in
                ( model, Cmd.none )


runPrim : Int -> Cmd Msg
runPrim mazeSize =
    let
        upperBound =
            mazeSize - 1

        squareGen =
            Random.pair (Random.int 0 upperBound) (Random.int 0 upperBound)
    in
        Random.generate PrimInit squareGen


findNeighbors : Int -> Square -> Set Square
findNeighbors maxSize square =
    let
        ( x, y ) =
            square

        neighbors =
            Set.fromList [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
    in
        Set.filter (\( x, y ) -> x >= 0 && x < maxSize && y >= 0 && y < maxSize) neighbors


updateOpenings : Square -> Square -> DictOpenings -> DictOpenings
updateOpenings source target openings =
    let
        sourceOpenings =
            openWall source target openings

        targetOpenings =
            openWall target source openings

        dictUpdate =
            Dict.fromList [ ( source, sourceOpenings ), ( target, targetOpenings ) ]

        visited =
            Dict.union dictUpdate openings
    in
        visited


openWall : Square -> Square -> DictOpenings -> Openings
openWall source target dictOpenings =
    let
        sourceOpenings =
            Dict.get source dictOpenings |> Maybe.withDefault []

        ( sourceX, sourceY ) =
            source

        ( targetX, targetY ) =
            target
    in
        if sourceX > targetX then
            "W" :: sourceOpenings
        else if sourceX < targetX then
            "E" :: sourceOpenings
        else if sourceY > targetY then
            "N" :: sourceOpenings
        else
            "S" :: sourceOpenings


exportJsonOutput : DictOpenings -> String
exportJsonOutput visited =
    let
        squareIdString =
            \( x, y ) -> (toString x) ++ "," ++ (toString y)

        mapOpeningsToJsonList =
            \openings -> List.map (\opening -> Json.string opening) openings |> Json.list
    in
        Dict.toList visited
            |> List.map (\( sq, openings ) -> ( squareIdString sq, mapOpeningsToJsonList openings ))
            |> Json.object
            |> Json.encode 0



-- Functions for logging Prim's Algorithm steps (events)


eventInit : Square -> Model -> String
eventInit square model =
    let
        totalSquare =
            model.size * model.size
    in
        "InitWith " ++ toString square ++ " - total square = " ++ (toString totalSquare)


eventNextVisit : Square -> Model -> String
eventNextVisit square model =
    let
        toGo =
            Set.size model.toVisit

        done =
            Dict.size model.visited

        totalSquare =
            model.size * model.size

        toDo =
            totalSquare - toGo - done
    in
        "NextVisit: "
            ++ toString square
            ++ " - to do = "
            ++ (toString toDo)
            ++ ", to go = "
            ++ (toString toGo)
            ++ ", done = "
            ++ (toString done)


eventOpenWall : Square -> Square -> String
eventOpenWall sourceSquare targetSquare =
    "OpenWall " ++ toString sourceSquare ++ " -> " ++ toString targetSquare
