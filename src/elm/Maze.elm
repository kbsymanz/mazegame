module Maze exposing (initialWalls, update)

import Dict
import Html exposing (Html, div, text)
import Html.App as App
import Svg as S
import Svg.Attributes as S


-- LOCAL IMPORTS

import View as V
import Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Cell
        , defaultMaze
        , emptyMazes
        , getCurrentMaze
        , updateCurrentMaze
        )
import Msg exposing (Msg(..))
import Util as U


-- MODEL


{-| The number of blocks the maze is horizontally and
    vertically. Blocks are blockSize pixels in size each.
-}
gameWindowSize : Int
gameWindowSize =
    20


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
    40


initialModel : Model
initialModel =
    { mazes =
        Just (defaultMaze blockSize gameWindowSize displayWindowSize)
            |> updateCurrentMaze emptyMazes
    , mazeMode = Editing
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
            {- TODO: actually write code here. -}
            model ! []

        Click x y ->
            -- Toggle whether the chosen cell is a wall or not.
            let
                -- Get the current maze from the List.
                currentMaze' =
                    getCurrentMaze model.mazes

                -- Get the cell from the Maze.
                cell =
                    U.getCell currentMaze' [ x, y ]

                -- Create a new "toggled" version of the cell.
                newCell =
                    U.toggleCellWall currentMaze' cell

                -- Get a version of the maze with the new cell in it.
                newMaze =
                    U.updateMazeWithCell newCell [ x, y ] currentMaze'

                -- Get an updated version of all the mazes with the new maze as current.
                mazes' =
                    updateCurrentMaze model.mazes newMaze
            in
                { model | mazes = mazes' } ! []

        PlayMode mode ->
            { model | mazeMode = mode } ! []

        DisplayWindowSize size ->
            let
                -- Get the current maze from the List.
                currentMaze' =
                    getCurrentMaze model.mazes

                -- Make a version of the maze with the adjusted value.
                newMaze =
                    case currentMaze' of
                        Just m ->
                            Just { m | displayWindowSize = size }

                        Nothing ->
                            currentMaze'

                -- Get an updated version of all the mazes with the new maze as current.
                mazes' =
                    updateCurrentMaze model.mazes newMaze
            in
                { model | mazes = mazes' } ! []



-- MAIN


subscriptions =
    always Sub.none


main : Program Never
main =
    App.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = V.view
        , subscriptions = subscriptions
        }