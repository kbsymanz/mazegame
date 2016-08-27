module MazeView exposing (..)

import Dict
import Html exposing (Html, div, text)
import Svg as S
import Svg.Attributes as S
import Svg.Events as S


-- LOCAL IMPORTS

import Model exposing (Model, Cell)
import Msg exposing (..)


intToPx : Int -> String
intToPx int =
    int |> toString |> flip (++) "px"


view : Model -> Html Msg
view model =
    let
        wh =
            model.gameWindowSize * model.blockSize
    in
        S.svg
            [ S.width (intToPx wh)
            , S.height (intToPx wh)
            ]
            [ background model
            ]


background : Model -> Html Msg
background model =
    let
        wh =
            model.gameWindowSize * model.blockSize

        list =
            [ S.rect
                [ S.width (intToPx wh)
                , S.height (intToPx wh)
                , S.fill "lightgrey"
                ]
                []
            ]
                ++ (drawCells model)
    in
        S.g [] list


drawCells : Model -> List (S.Svg Msg)
drawCells model =
    let
        -- Convert from Dict to List, taking the values only, which are the cells.
        cells =
            List.map snd (Dict.toList model.cells)
    in
        List.map (\cell -> drawCell cell.x cell.y cell.isWall model.blockSize) cells


drawCell : Int -> Int -> Bool -> Int -> S.Svg Msg
drawCell x y isWall blockSize =
    let
        -- Translate for blockSize and 0 based Svg system.
        xs =
            (x - 1) * blockSize

        ys =
            (y - 1) * blockSize

        ( fillColor, strokeColor ) =
            if isWall then
                ( "black", "black" )
            else
                ( "lightgrey", "grey" )
    in
        S.rect
            [ S.width <| intToPx blockSize
            , S.height <| intToPx blockSize
            , S.stroke strokeColor
            , S.fill fillColor
            , S.x (intToPx xs)
            , S.y (intToPx ys)
            , S.onClick (Click x y)
            ]
            []
