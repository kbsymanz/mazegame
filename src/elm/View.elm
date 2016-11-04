module View exposing (..)

import Array
import Html exposing (Html, div, p, text)
import Html.Events as Html
import List
import List.Zipper as Zipper exposing (Zipper)
import Matrix
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Svg as S
import Svg.Attributes as S


-- LOCAL IMPORTS

import MazeGenerate as MG
import Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Difficulty(..)
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


{-| MDL contexts.
-}
viewViewingContext : Int
viewViewingContext =
    0


viewEditingContext : Int
viewEditingContext =
    1


viewPlayingContext : Int
viewPlayingContext =
    2


mazeMetaInfoContext : Int
mazeMetaInfoContext =
    3


view : Model -> Html Msg
view model =
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
                    viewEditing model

                Playing ->
                    viewPlaying model
    in
        view


viewPlaying : Model -> Html Msg
viewPlaying model =
    let
        ( mazesList, currMazeIdx ) =
            ( Zipper.toList model.mazes
            , Zipper.before model.mazes
                |> List.length
            )
    in
        grid
            [ Color.background <| Color.accentContrast
            ]
            [ cell
                -- Maze on the left.
                [ size Desktop 10
                , size Tablet 7
                , size Phone 4
                ]
                <| [ viewMaze model ]
            , cell
                -- Done Playing button.
                [ size Desktop 2
                , size Tablet 1
                , size Phone 4
                ]
                [ Button.render Mdl
                    [ viewPlayingContext, 0 ]
                    model.mdl
                    [ Button.ripple
                    , Button.colored
                    , Button.onClick <| PlayMode Viewing
                    ]
                    [ text "Done Playing" ]
                ]
            ]


viewEditing : Model -> Html Msg
viewEditing model =
    let
        currentMaze =
            Zipper.current model.mazes
    in
        grid
            [ Color.background <| Color.accentContrast
            ]
            [ cell
                -- Maze on the left.
                [ size Desktop 8
                , size Tablet 6
                , size Phone 4
                ]
                <| [ viewMaze model ]
            , cell
                -- Maze meta data on the right.
                [ size Desktop 4
                , size Tablet 2
                , size Phone 4
                ]
                [ grid
                    [ Color.background <| Color.accent
                    ]
                    -- Title of the maze.
                    [ cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Textfield.render Mdl
                            [ viewEditingContext, 1 ]
                            model.mdl
                            [ Textfield.label "Title"
                            , Textfield.floatingLabel
                            , Textfield.value currentMaze.title
                            , Textfield.onInput SetTitle
                            ]
                        ]
                    , cell
                        -- The size of the maze to generate.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Toggles.radio Mdl
                            [ viewEditingContext, 10 ]
                            model.mdl
                            [ Toggles.value <| model.mazeSizePending <= 10
                            , Toggles.group "mazeSizePending"
                            , Toggles.ripple
                            , Toggles.onClick <| MazeSizePending 10
                            , Options.css "padding-right" "10px"
                            ]
                            [ text "10 x 10" ]
                        , Toggles.radio Mdl
                            [ viewEditingContext, 11 ]
                            model.mdl
                            [ Toggles.value <| model.mazeSizePending > 10 && model.mazeSizePending <= 20
                            , Toggles.group "mazeSizePending"
                            , Toggles.ripple
                            , Toggles.onClick <| MazeSizePending 20
                            , Options.css "padding-right" "10px"
                            ]
                            [ text "20 x 20" ]
                        , Toggles.radio Mdl
                            [ viewEditingContext, 12 ]
                            model.mdl
                            [ Toggles.value <| model.mazeSizePending > 20
                            , Toggles.group "mazeSizePending"
                            , Toggles.ripple
                            , Toggles.onClick <| MazeSizePending 40
                            , Options.css "padding-right" "10px"
                            ]
                            [ text "40 x 40" ]
                        ]
                    , cell
                        -- Generate maze button and percent complete.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            [ viewEditingContext, 0 ]
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| MazeGenerate (MG.BinaryTreeInit currentMaze.mazeSize currentMaze.id)
                            ]
                            [ text "Generate the maze" ]
                        , Chip.span []
                            [ Chip.content []
                                [ text <| (toString currentMaze.percComplete) ++ "%" ]
                            ]
                        ]
                    , cell
                        -- Done editing button.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            [ viewEditingContext, 1 ]
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| PlayMode Viewing
                            ]
                            [ text "Done Editing" ]
                        ]
                    ]
                ]
            ]


viewViewing : Model -> Html Msg
viewViewing model =
    let
        ( mazesList, currMazeIdx ) =
            ( Zipper.toList model.mazes
            , Zipper.before model.mazes
                |> List.length
            )

        mazes =
            List.indexedMap (\idx maze -> mazeMetaInfo model.mdl idx maze (idx == currMazeIdx) model.mazeDifficulty) mazesList
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
                            [ viewViewingContext, 0 ]
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


mazeMetaInfo : Material.Model -> Int -> Maze -> Bool -> Difficulty -> Grid.Cell Msg
mazeMetaInfo mdl idx maze isCurrentMaze mazeDifficulty =
    let
        makeMsg val =
            (toString val) ++ " x " ++ (toString val) ++ " blocks"

        gws =
            makeMsg maze.mazeSize

        vps =
            -- TODO: fix hard-code.
            makeMsg 60

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
                        [ text ("Title: " ++ maze.title) ]
                    ]
                , Card.text [ Color.text textColor ]
                    [ text ("Size: " ++ gws ++ ", Viewport: " ++ vps) ]
                , Card.text [ Color.text textColor ]
                    [ Toggles.radio Mdl
                        [ mazeMetaInfoContext, 10, idx ]
                        mdl
                        [ Toggles.value <| mazeDifficulty == Easy
                        , Toggles.group "PlayMode"
                        , Toggles.ripple
                        , Toggles.onClick <| MazeDifficulty Easy
                        , Options.css "padding-right" "10px"
                        ]
                        [ text "Easy" ]
                    , Toggles.radio Mdl
                        [ mazeMetaInfoContext, 11, idx ]
                        mdl
                        [ Toggles.value <| mazeDifficulty == Medium
                        , Toggles.group "PlayMode"
                        , Toggles.ripple
                        , Toggles.onClick <| MazeDifficulty Medium
                        , Options.css "padding-right" "10px"
                        ]
                        [ text "Medium" ]
                    , Toggles.radio Mdl
                        [ mazeMetaInfoContext, 12, idx ]
                        mdl
                        [ Toggles.value <| mazeDifficulty == Hard
                        , Toggles.group "PlayMode"
                        , Toggles.ripple
                        , Toggles.onClick <| MazeDifficulty Hard
                        , Options.css "padding-right" "10px"
                        ]
                        [ text "Hard" ]
                    ]
                , Card.text []
                    [ Button.render Mdl
                        [ mazeMetaInfoContext, 1, idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.onClick <| PlayMode Playing
                        ]
                        [ text "Play" ]
                    , Button.render Mdl
                        [ mazeMetaInfoContext, 2, idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.onClick <| PlayMode Editing
                        ]
                        [ text "Edit" ]
                    , Button.render Mdl
                        [ mazeMetaInfoContext, 3, idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Options.css "float" "right"
                        , Button.onClick <| DeleteMaze idx
                        ]
                        [ text "Delete" ]
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
            Zipper.current model.mazes

        -- TODO: widthHeight and blockSize to be determined by available
        -- screen width and playmode instead of arbitrary like this.
        ( widthHeight, blockSize ) =
            case model.mazeMode of
                Viewing ->
                    ( 300, 300 // maze.mazeSize )

                Editing ->
                    ( 600, 600 // maze.mazeSize )

                Playing ->
                    ( 800, 800 // maze.mazeSize )

        ( viewportSize, x, y ) =
            ( case ( model.mazeMode, model.mazeDifficulty ) of
                ( Playing, Hard ) ->
                    round <| (toFloat maze.mazeSize) / 4

                ( Playing, Medium ) ->
                    round <| (toFloat maze.mazeSize) / 2

                ( Playing, Easy ) ->
                    maze.mazeSize

                _ ->
                    maze.mazeSize
            , fst maze.center
            , snd maze.center
            )

        vbx =
            max 0 ((x * blockSize) - ((viewportSize * blockSize) // 2))

        vby =
            max 0 ((y * blockSize) - ((viewportSize * blockSize) // 2))

        displayWH =
            viewportSize * blockSize
    in
        S.svg
            [ S.width (intToPx widthHeight)
            , S.height (intToPx widthHeight)
            , S.viewBox <| (fourIntToString vbx vby displayWH displayWH)
            ]
            [ background model blockSize
            ]


background : Model -> Int -> Html Msg
background model blockSize =
    let
        currentMaze =
            Zipper.current model.mazes

        -- TODO: 10 is a hard-coded blockSize. Set according to available space.
        wh =
            currentMaze.mazeSize * blockSize

        list =
            [ S.rect
                [ S.width (intToPx wh)
                , S.height (intToPx wh)
                , S.fill "white"
                ]
                []
            ]
                ++ (drawCells currentMaze model.mazeMode blockSize)
    in
        S.g [] list


drawCells : Maze -> Mode -> Int -> List (S.Svg Msg)
drawCells maze mode blockSize =
    let
        ( cells, centerX, centerY ) =
            ( Matrix.toIndexedArray maze.cells
                |> Array.toList
            , fst maze.center
            , snd maze.center
            )
    in
        List.map
            (\c ->
                let
                    col =
                        fst c
                            |> fst

                    row =
                        fst c
                            |> snd

                    cell =
                        snd c
                in
                    drawCell col
                        row
                        cell.northLink
                        cell.eastLink
                        cell.southLink
                        cell.westLink
                        (centerX == col && centerY == row)
                        mode
                        blockSize
            )
            cells


drawCell : Int -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Mode -> Int -> S.Svg Msg
drawCell col row north east south west isCenter mode blockSize =
    let
        -- Translate for blockSize and 0 based Svg system.
        xs =
            col * blockSize

        ys =
            row * blockSize

        fillColor =
            "white"

        strokeColor =
            "black"

        {- : Generate lines but only if needed. -}
        topLine =
            if north then
                []
            else
                [ S.line
                    [ S.x1 (intToPx xs)
                    , S.y1 (intToPx ys)
                    , S.x2 (intToPx (xs + blockSize))
                    , S.y2 (intToPx ys)
                    , S.stroke strokeColor
                    ]
                    []
                ]

        rightLine =
            if east then
                []
            else
                [ S.line
                    [ S.x1 (intToPx (xs + blockSize))
                    , S.y1 (intToPx ys)
                    , S.x2 (intToPx (xs + blockSize))
                    , S.y2 (intToPx (ys + blockSize))
                    , S.stroke strokeColor
                    ]
                    []
                ]

        bottomLine =
            if south then
                []
            else
                [ S.line
                    [ S.x1 (intToPx (xs + blockSize))
                    , S.y1 (intToPx (ys + blockSize))
                    , S.x2 (intToPx xs)
                    , S.y2 (intToPx (ys + blockSize))
                    , S.stroke strokeColor
                    ]
                    []
                ]

        leftLine =
            if west then
                []
            else
                [ S.line
                    [ S.x1 (intToPx xs)
                    , S.y1 (intToPx ys)
                    , S.x2 (intToPx xs)
                    , S.y2 (intToPx (ys + blockSize))
                    , S.stroke strokeColor
                    ]
                    []
                ]

        cellLines =
            [ S.rect
                [ S.width <| intToPx blockSize
                , S.height <| intToPx blockSize
                , S.stroke fillColor
                , S.fill
                    <| if isCenter && mode == Playing then
                        "lightblue"
                       else
                        fillColor
                , S.x (intToPx xs)
                , S.y (intToPx ys)
                ]
                []
            ]
                ++ topLine
                ++ rightLine
                ++ bottomLine
                ++ leftLine
    in
        S.g []
            cellLines
