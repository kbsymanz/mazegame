module Maze exposing (initialWalls, update)

import Dict
import Html exposing (Html, div, text)
import Html.App as App
import Svg as S
import Svg.Attributes as S


-- LOCAL IMPORTS

import MazeView as MV
import Model exposing (Model, Cell)
import Msg exposing (Msg(..))


-- MODEL


{-| The number of blocks the maze is horizontally and
    vertically. Blocks are blockSize pixels in size each.
-}
gameWindowSize : Int
gameWindowSize =
    40


{-| The number of blocks the user can currently see horizontally
    and vertically. Blocks are blockSize pixels in size each.
-}
displayWindowSize : Int
displayWindowSize =
    20


{-| The number of pixels that make up the width and height of
    each block.
-}
blockSize : Int
blockSize =
    10


initialModel : Model
initialModel =
    { cells = initialWalls gameWindowSize
    , blockSize = blockSize
    , gameWindowSize = gameWindowSize
    , displayWindowSize = displayWindowSize
    , center = Cell 10 10 True
    , exit = Nothing
    , isFinished = False
    , allowToggleCells = False
    }


initialWalls : Int -> Dict.Dict (List Int) Cell
initialWalls windowSize =
    let
        -- top, left, right, bottom as a List
        outsideWall =
            List.map (\x -> ( [ x, 1 ], Cell x 1 True )) [1..windowSize]
                ++ List.map (\x -> ( [ 1, x ], Cell 1 x True )) [1..windowSize]
                ++ List.map (\x -> ( [ windowSize, x ], Cell windowSize x True )) [1..windowSize]
                ++ List.map (\x -> ( [ x, windowSize ], Cell x windowSize True )) [1..windowSize]

        -- All cells not on the edge.
        inner =
            List.map (\x -> List.map (\y -> ( [ x, y ], Cell x y False )) [2..(windowSize - 1)]) [2..(windowSize - 1)]
                |> List.concat
    in
        Dict.fromList (outsideWall ++ inner)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move cell ->
            model ! []

        Click x y ->
            -- Toggle whether the chosen cell is a wall or not.
            let
                cell =
                    Dict.get [ x, y ] model.cells

                newCells =
                    case cell of
                        Nothing ->
                            model.cells

                        Just c ->
                            if model.allowToggleCells then
                                Dict.insert [ x, y ] { c | isWall = not c.isWall } model.cells
                            else
                                model.cells
            in
                ( { model | cells = newCells }, Cmd.none )

        ToggleAllowToggle ->
            ( { model | allowToggleCells = not model.allowToggleCells }, Cmd.none )

        DisplayWindowSize size ->
            ( { model | displayWindowSize = size }, Cmd.none )



-- MAIN


subscriptions =
    always Sub.none


main : Program Never
main =
    App.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = MV.view
        , subscriptions = subscriptions
        }
