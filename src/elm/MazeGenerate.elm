module MazeGenerate
    exposing
        ( Msg(..)
        , Model
        , emptyModel
        , update
        , Cell
        )

import Dict
import Random
import Task


-- LOCAL IMPORTS


type Direction
    = North
    | East
    | South
    | West



{- :
   To Do:
   1. Random generation requires use of update, etc. Plan out messages used.
   2. Plan out what is needed in the model: seed at least?
   x. Create initial model so that main program can initialize with it.
   x. Create mazeGeneration field in Maze module.
   5. Do the Binary Tree algorithm. Consider starting in upper left corner.
   6. Add tests.
   7. Go through all code and standardize on north or top, etc.
   x. Get rid of original cells field in maze.
   9. Create a random exit on the maze at the end.
   10. Do the Sidewinder algorithm.
   11. Turn the whole thing into a package.
-}


type alias Model =
    { cells : Dict.Dict (List Int) Cell
    , mazeSize : Int
    , currCol : Int
    , currRow : Int
    }


emptyModel : Model
emptyModel =
    { cells = Dict.empty
    , mazeSize = 10
    , currCol = 1
    , currRow = 1
    }


type alias Cell =
    { col : Int
    , row : Int
    , northLink : Bool
    , eastLink : Bool
    , southLink : Bool
    , westLink : Bool
    }


type Msg
    = BinaryTreeInit Int
    | BinaryTreeUpdate Bool
    | BinaryTreeComplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BinaryTreeInit size ->
            let
                -- Initialize the cells to the proper size.
                initializedModel =
                    initModel { emptyModel | mazeSize = size }

                -- Start the process of stepping through the maze generation.
                newCmd =
                    Random.generate BinaryTreeUpdate Random.bool
            in
                initializedModel ! [ newCmd ]

        BinaryTreeUpdate bool ->
            let
                ( newModel, newCmd ) =
                    generateBinaryTree model bool
            in
                newModel ! [ newCmd ]

        BinaryTreeComplete ->
            model ! []


{-| Initialize the cells field of the model with unlinked cells according to
    the mazeSize field.
-}
initModel : Model -> Model
initModel model =
    let
        -- top, left, right, bottom as a List
        outerWall =
            List.map (\x -> ( [ x, 1 ], Cell x 1 False False False False )) [1..model.mazeSize]
                ++ List.map (\x -> ( [ 1, x ], Cell 1 x False False False False )) [1..model.mazeSize]
                ++ List.map (\x -> ( [ model.mazeSize, x ], Cell model.mazeSize x False False False False )) [1..model.mazeSize]
                ++ List.map (\x -> ( [ x, model.mazeSize ], Cell x model.mazeSize False False False False )) [1..model.mazeSize]

        -- All cells not on the edge.
        inner =
            List.map (\x -> List.map (\y -> ( [ x, y ], Cell x y False False False False )) [2..(model.mazeSize - 1)]) [2..(model.mazeSize - 1)]
                |> List.concat
    in
        { model | cells = Dict.fromList (outerWall ++ inner) }


{-| Generate a single step of the maze using the Binary Tree algorithm. Returns the
    updated model as well as a Cmd that leads to the next step.
-}
generateBinaryTree : Model -> Bool -> ( Model, Cmd Msg )
generateBinaryTree model bool =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        cell =
            Dict.get [ col, row ] model.cells

        ( newModel, isCompleted ) =
            case cell of
                Just c ->
                    if col == model.mazeSize then
                        link c South model
                            |> advanceByCell
                    else if row == model.mazeSize then
                        link c East model
                            |> advanceByCell
                    else
                        link c
                            (if bool then
                                East
                             else
                                South
                            )
                            model
                            |> advanceByCell

                Nothing ->
                    -- If we get here, something is wrong.
                    ( model, True )

        newCmd =
            if isCompleted then
                Task.perform (always BinaryTreeComplete) (always BinaryTreeComplete) (Task.succeed True)
            else
                Random.generate BinaryTreeUpdate Random.bool
    in
        newModel ! [ newCmd ]


{-| Advance the currCol and currRow fields of the model by cell moving by
    row and then cell within row ascending. Return True in the second element
    of the tuple to signify completion.
-}
advanceByCell : Model -> ( Model, Bool )
advanceByCell model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        ( newModel, isCompleted ) =
            if row /= model.mazeSize then
                if col == model.mazeSize then
                    ( { model | currCol = 1, currRow = row + 1 }, False )
                else
                    ( { model | currCol = col + 1 }, False )
            else if col /= model.mazeSize then
                ( { model | currCol = col + 1 }, False )
            else
                ( model, True )
    in
        ( newModel, isCompleted )


{-| "Link" two cells by setting the link field on their facing walls to True.
    Linked cells do not have a wall between them.
-}
link : Cell -> Direction -> Model -> Model
link cell direction model =
    let
        -- Set the directional link in the cell passed and the corresponding cell
        -- if there is one (cells on a border may not have one).
        ( updatedCell, linkedCell ) =
            case direction of
                North ->
                    ( { cell | northLink = True }
                    , case Dict.get [ cell.col, max 0 (cell.row - 1) ] model.cells of
                        Nothing ->
                            Nothing

                        Just c ->
                            Just { c | southLink = True }
                    )

                East ->
                    ( { cell | eastLink = True }
                    , case Dict.get [ min model.mazeSize (cell.col + 1), cell.row ] model.cells of
                        Nothing ->
                            Nothing

                        Just c ->
                            Just { c | westLink = True }
                    )

                South ->
                    ( { cell | southLink = True }
                    , case Dict.get [ cell.col, min model.mazeSize (cell.row + 1) ] model.cells of
                        Nothing ->
                            Nothing

                        Just c ->
                            Just { c | northLink = True }
                    )

                West ->
                    ( { cell | westLink = True }
                    , case Dict.get [ min 0 (cell.col - 1), cell.row ] model.cells of
                        Nothing ->
                            Nothing

                        Just c ->
                            Just { c | eastLink = True }
                    )

        -- Create a new set of cells with the updated cell and possibly the linked cell.
        newCells =
            case linkedCell of
                Just c2 ->
                    Dict.insert [ updatedCell.col, updatedCell.row ] updatedCell model.cells
                        |> Dict.insert [ c2.col, c2.row ] c2

                Nothing ->
                    Dict.insert [ updatedCell.col, updatedCell.row ] updatedCell model.cells
    in
        { model | cells = newCells }
