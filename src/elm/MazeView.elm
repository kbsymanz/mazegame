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


fourIntToString : Int -> Int -> Int -> Int -> String
fourIntToString a b c d =
    ((toString a) ++ " " ++ (toString b) ++ " " ++ (toString c) ++ " " ++ (toString d))


view : Model -> Html Msg
view model =
    let
        bs =
            model.blockSize

        wh =
            model.gameWindowSize * bs

        vbx =
            max 0 ((model.center.x * bs) - ((model.displayWindowSize * bs) // 2))

        vby =
            max 0 ((model.center.y * bs) - ((model.displayWindowSize * bs) // 2))

        displayWH =
            model.displayWindowSize * bs

        _ =
            Debug.log "view: vbx/vby/displayWH" ((toString vbx) ++ "/" ++ (toString vby) ++ "/" ++ (toString displayWH))
    in
        S.svg
            [ S.width (intToPx wh)
            , S.height (intToPx wh)
            , S.viewBox <| (fourIntToString vbx vby displayWH displayWH)
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
        -- NOTE: temporarily adding center to list in order to see where it is.
        cells =
            (List.map snd (Dict.toList model.cells)) ++ [ model.center ]
    in
        List.map (\cell -> drawCell cell.x cell.y cell.isWall model.allowToggleCells model.blockSize) cells


drawCell : Int -> Int -> Bool -> Bool -> Int -> S.Svg Msg
drawCell x y isWall drawGridLines blockSize =
    let
        -- Translate for blockSize and 0 based Svg system.
        xs =
            (x - 1) * blockSize

        ys =
            (y - 1) * blockSize

        ( fillColor, strokeColor ) =
            if isWall then
                ( "black", "black" )
            else if drawGridLines then
                ( "lightgrey", "grey" )
            else
                ( "lightgrey", "lightgrey" )

        cell =
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

        contents =
            if drawGridLines then
                [ cell ]
                    ++ [ S.text'
                            [ S.x (toString (xs + (blockSize // 5)))
                            , S.y (toString (ys + (blockSize // 2)))
                            , S.fontSize (toString (blockSize // 5))
                            , S.fontFamily "Verdana"
                            , S.fontWeight "800"
                            , S.onClick (Click x y)
                            , S.fill
                                (if isWall then
                                    "white"
                                 else
                                    "black"
                                )
                            ]
                            [ S.text ((toString x) ++ "," ++ (toString y)) ]
                       ]
            else
                [ cell ]
    in
        S.g []
            contents
