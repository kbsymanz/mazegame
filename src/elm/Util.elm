module Util exposing (..)

import Dict


-- LOCAL IMPORTS

import Model exposing (Model, Maze, Mode(..), Cell)
import Msg exposing (Msg(..))


getCell : Maybe Maze -> List Int -> Maybe Cell
getCell maze key =
    case maze of
        Just m ->
            Dict.get key m.cells

        Nothing ->
            Nothing


toggleCellWall : Maybe Maze -> Maybe Cell -> Maybe Cell
toggleCellWall maze cell =
    case ( maze, cell ) of
        ( Just m, Just c ) ->
            Just { c | isWall = not c.isWall }

        _ ->
            cell


updateMazeWithCell : Maybe Cell -> List Int -> Maybe Maze -> Maybe Maze
updateMazeWithCell cell key maze =
    case ( cell, maze ) of
        ( Just c, Just m ) ->
            Just { m | cells = (Dict.insert key c m.cells) }

        _ ->
            Nothing
