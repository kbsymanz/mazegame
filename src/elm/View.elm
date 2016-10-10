module View exposing (..)

import Dict
import Html exposing (Html, div, p, text)
import Html.Attributes as Html exposing (class, style)
import Html.Events as Html
import List
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.List as MList
import Material.Options as Options
import Material.Typography as Typo
import Svg as S
import Svg.Attributes as S
import Svg.Events as S


-- LOCAL IMPORTS

import Model
    exposing
        ( Model
        , Maze
        , Cell
        , Mode(..)
        , getCurrentMaze
        , mazesAsList
        )
import Msg exposing (..)


--import Util as U


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


intToPx : Int -> String
intToPx int =
    int |> toString |> flip (++) "px"


fourIntToString : Int -> Int -> Int -> Int -> String
fourIntToString a b c d =
    ((toString a) ++ " " ++ (toString b) ++ " " ++ (toString c) ++ " " ++ (toString d))


type alias Mdl =
    Material.Model


newMazeBtn =
    [ 0, 0 ]


previousMazeBtn =
    [ 0, 1 ]


nextMazeBtn =
    [ 0, 2 ]


view : Model -> Html Msg
view model =
    let
        numMazes =
            Model.mazesAsList model.mazes
                |> fst
                |> List.length

        _ =
            Debug.log "view: count mazes" numMazes
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = headerSmall "Maze Deathtrap" model
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewMain model ]
            }


viewMain : Model -> Html Msg
viewMain model =
    let
        view =
            case model.mazeMode of
                Viewing ->
                    viewViewing model

                Editing ->
                    viewViewing model

                Playing ->
                    viewViewing model
    in
        view


viewViewing : Model -> Html Msg
viewViewing model =
    let
        ( mazesList, currMazeIdx ) =
            mazesAsList model.mazes

        mazes =
            List.indexedMap (\idx maze -> mazeToCell model.mdl idx maze (idx == currMazeIdx)) mazesList
    in
        grid
            [ Color.background <| Color.accentContrast
            ]
            [ cell
                -- Maze on the left.
                [ size Desktop 6
                , size Tablet 8
                , size Phone 4
                ]
                <| [ viewMaze model ]
            , cell
                -- List of Mazes on the right.
                [ size Desktop 6
                , size Tablet 8
                , size Phone 4
                ]
                [ grid
                    [ Color.background <| Color.accent
                    ]
                    -- Title of the list of mazes.
                    [ cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.display1
                            ]
                            [ text "Mazes" ]
                        ]
                    , cell
                        -- List of mazes.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ grid [] mazes ]
                    , cell
                        -- New maze button.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            newMazeBtn
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| NewMaze
                            ]
                            [ text "New Maze" ]
                        ]
                    ]
                ]
            ]


mazeToCell : Material.Model -> Int -> Maze -> Bool -> Grid.Cell Msg
mazeToCell mdl idx maze isCurrentMaze =
    let
        makeMsg val =
            (toString val) ++ " x " ++ (toString val) ++ " blocks"

        gws =
            makeMsg maze.gameWindowSize

        vps =
            makeMsg maze.displayWindowSize

        backgroundColor =
            if isCurrentMaze then
                Color.primaryContrast
            else
                Color.accentContrast

        textColor =
            if isCurrentMaze then
                Color.primary
            else
                Color.accent
    in
        cell
            [ size Desktop 12
            , size Tablet 8
            , size Phone 4
            ]
            [ Card.view
                [ Options.css "width" "100%"
                , Color.background backgroundColor
                , Color.text textColor
                , Card.border
                , Options.attribute <| Html.onClick <| GoToMaze idx
                ]
                [ Card.title []
                    [ Card.head
                        [ Color.background backgroundColor
                        , Color.text textColor
                        ]
                        [ text ("Title: " ++ maze.title ++ " " ++ (toString idx)) ]
                    ]
                , Card.text [ Color.text textColor ]
                    [ text ("Size: " ++ gws ++ ", Viewport: " ++ vps) ]
                , Card.text []
                    [ Button.render Mdl
                        [ 1, idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.onClick <| PlayMode Playing
                        ]
                        [ text "Play" ]
                    , Button.render Mdl
                        [ 2, idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.onClick <| PlayMode Editing
                        ]
                        [ text "Edit" ]
                    ]
                ]
            ]


headerSmall : String -> Model -> List (Html a)
headerSmall title model =
    let
        contents =
            [ Layout.row []
                [ Layout.title []
                    [ Options.styled p [ Typo.headline ] [ text title ]
                    ]
                ]
            ]
    in
        contents


viewMaze : Model -> Html Msg
viewMaze model =
    let
        maze =
            getCurrentMaze model.mazes

        -- TODO: wh to be determined by available screen width and playmode.
        wh =
            case model.mazeMode of
                Viewing ->
                    300

                Editing ->
                    500

                Playing ->
                    400

        ( bs, displayWindowSize, x, y ) =
            case maze of
                Just m ->
                    ( m.blockSize
                    , m.displayWindowSize
                    , fst m.center
                    , snd m.center
                    )

                Nothing ->
                    ( 0, 0, 0, 0 )

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
