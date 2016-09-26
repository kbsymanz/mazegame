module View exposing (..)

import Dict
import Html exposing (Html, div, text)
import Svg as S
import Svg.Attributes as S
import Svg.Events as S


-- LOCAL IMPORTS

import Model exposing (Model, Maze, Cell, Mode(..), getCurrentMaze)
import Msg exposing (..)


--import Util as U


intToPx : Int -> String
intToPx int =
    int |> toString |> flip (++) "px"


fourIntToString : Int -> Int -> Int -> Int -> String
fourIntToString a b c d =
    ((toString a) ++ " " ++ (toString b) ++ " " ++ (toString c) ++ " " ++ (toString d))


view : Model -> Html Msg
view model =
    let
        maze =
            getCurrentMaze model.mazes

        ( bs, wh, displayWindowSize, x, y ) =
            case maze of
                Just m ->
                    ( m.blockSize
                    , m.gameWindowSize * m.blockSize
                    , m.displayWindowSize
                    , fst m.center
                    , snd m.center
                    )

                Nothing ->
                    ( 0, 0, 0, 0, 0 )

        vbx =
            max 0 ((x * bs) - ((displayWindowSize * bs) // 2))

        vby =
            max 0 ((y * bs) - ((displayWindowSize * bs) // 2))

        displayWH =
            displayWindowSize * bs

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
        currentMaze =
            getCurrentMaze model.mazes

        wh =
            case currentMaze of
                Just m ->
                    m.gameWindowSize * m.blockSize

                Nothing ->
                    0

        list =
            [ S.rect
                [ S.width (intToPx wh)
                , S.height (intToPx wh)
                , S.fill "lightgrey"
                ]
                []
            ]
                ++ (drawCells currentMaze model.mazeMode)
    in
        S.g [] list


drawCells : Maybe Maze -> Mode -> List (S.Svg Msg)
drawCells maze mode =
    let
        ( cells, blockSize, centerX, centerY ) =
            case maze of
                Just m ->
                    ( (List.map snd (Dict.toList m.cells))
                    , m.blockSize
                    , fst m.center
                    , snd m.center
                    )

                Nothing ->
                    ( []
                    , 0
                    , 0
                    , 0
                    )
    in
        List.map
            (\cell ->
                drawCell cell.x
                    cell.y
                    cell.isWall
                    (centerX == cell.x && centerY == cell.y)
                    mode
                    blockSize
            )
            cells


drawCell : Int -> Int -> Bool -> Bool -> Mode -> Int -> S.Svg Msg
drawCell x y isWall isCenter mode blockSize =
    let
        -- Translate for blockSize and 0 based Svg system.
        xs =
            (x - 1) * blockSize

        ys =
            (y - 1) * blockSize

        -- TODO: this is not flexible enough.
        ( fillColor, strokeColor ) =
            if isWall then
                ( "black", "black" )
            else if isCenter then
                ( "white", "grey" )
            else if mode == Editing then
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
            if mode == Editing then
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
