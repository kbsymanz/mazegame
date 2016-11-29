module Tests exposing (..)

import Array
import Expect
import List.Extra as LE
import Random
import String
import Test exposing (..)
import Fuzz


-- Project specific imports.

import Material
import Matrix
import TestData exposing (blockSize, gameWindowSize, displayWindowSize)


-- LOCAL IMPORTS

import Maze as M
import MazeGenerate as MG
import Model exposing (Mode(..), Difficulty(..))


binaryTreeEast : Bool
binaryTreeEast =
    True


binaryTreeSouth : Bool
binaryTreeSouth =
    False


all : Test
all =
    describe "MazeGenerate module"
        [ modelTests
        , advanceByCellTests
        , generateBinaryTreeTests
        , linkTests
        , updateTests
        , calcPointsTests
        , recursiveBacktrackerTests
        , neighborsTests
        , discernLinkDirTests
        , addCellLinksTests
        ]


calcPointsTests : Test
calcPointsTests =
    describe "calcPoints"
        [ test "10x10, 0 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 0 Easy
                in
                    Expect.equal points 200
        , test "10x10, 23 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 23 Easy
                in
                    Expect.equal points 177
        , test "10x10, 119 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 119 Easy
                in
                    Expect.equal points 81
        , test "10x10, 0 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 0 Medium
                in
                    Expect.equal points 300
        , test "10x10, 55 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 55 Medium
                in
                    Expect.equal points 245
        , test "10x10, 89 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 89 Medium
                in
                    Expect.equal points 211
        , test "10x10, 0 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 0 Hard
                in
                    Expect.equal points 400
        , test "20x20, 0 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 0 Easy
                in
                    Expect.equal points 800
        , test "20x20, 45 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 45 Easy
                in
                    Expect.equal points 755
        , test "20x20, 119 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 119 Easy
                in
                    Expect.equal points 681
        , test "20x20, 0 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 0 Medium
                in
                    Expect.equal points 1000
        , test "20x20, 88 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 88 Medium
                in
                    Expect.equal points 912
        , test "20x20, 119 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 119 Medium
                in
                    Expect.equal points 881
        , test "20x20, 0 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 0 Hard
                in
                    Expect.equal points 1200
        , test "20x20, 66 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 66 Hard
                in
                    Expect.equal points 1134
        , test "20x20, 119 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 119 Hard
                in
                    Expect.equal points 1081
        , test "40x40, 0 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 0 Easy
                in
                    Expect.equal points 1800
        , test "40x40, 33 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 33 Easy
                in
                    Expect.equal points 1767
        , test "40x40, 119 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 119 Easy
                in
                    Expect.equal points 1681
        , test "40x40, 0 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 0 Medium
                in
                    Expect.equal points 2200
        , test "40x40, 57 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 57 Medium
                in
                    Expect.equal points 2143
        , test "40x40, 119 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 119 Medium
                in
                    Expect.equal points 2081
        , test "40x40, 0 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 0 Hard
                in
                    Expect.equal points 2600
        , test "40x40, 38 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 38 Hard
                in
                    Expect.equal points 2562
        , test "40x40, 119 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 119 Hard
                in
                    Expect.equal points 2481
        ]


addCellLinksTests : Test
addCellLinksTests =
    describe "addCellLinks"
        [ test "North"
            <| \() ->
                let
                    ( cell1, cell2 ) =
                        ( MG.Cell False False False False False
                        , MG.Cell False False False False False
                        )

                    ( cell3, cell4 ) =
                        MG.addCellLinks cell1 cell2 (Just MG.North)
                in
                    Expect.equal (cell3.northLink && cell4.southLink) True
        , test "East"
            <| \() ->
                let
                    ( cell1, cell2 ) =
                        ( MG.Cell False False False False False
                        , MG.Cell False False False False False
                        )

                    ( cell3, cell4 ) =
                        MG.addCellLinks cell1 cell2 (Just MG.East)
                in
                    Expect.equal (cell3.eastLink && cell4.westLink) True
        , test "South"
            <| \() ->
                let
                    ( cell1, cell2 ) =
                        ( MG.Cell False False False False False
                        , MG.Cell False False False False False
                        )

                    ( cell3, cell4 ) =
                        MG.addCellLinks cell1 cell2 (Just MG.South)
                in
                    Expect.equal (cell3.southLink && cell4.northLink) True
        , test "West"
            <| \() ->
                let
                    ( cell1, cell2 ) =
                        ( MG.Cell False False False False False
                        , MG.Cell False False False False False
                        )

                    ( cell3, cell4 ) =
                        MG.addCellLinks cell1 cell2 (Just MG.West)
                in
                    Expect.equal (cell3.westLink && cell4.eastLink) True
        , test "Nothing"
            <| \() ->
                let
                    ( cell1, cell2 ) =
                        ( MG.Cell False False False False False
                        , MG.Cell False False False False False
                        )

                    ( cell3, cell4 ) =
                        MG.addCellLinks cell1 cell2 Nothing
                in
                    Expect.equal (cell1 == cell3 && cell2 == cell4) True
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "North link: top row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 0
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.North model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasNorthLink, hasSouthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        , case Matrix.get col (row - 1) newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && not hasSouthLink) True
        , test "North link: middle row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 3
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.North model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasNorthLink, hasSouthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        , case Matrix.get col (row - 1) newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "North link: bottom row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 4
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.North model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasNorthLink, hasSouthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        , case Matrix.get col (row - 1) newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "South link: Top row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 0
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.South model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasSouthLink, hasNorthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        , case Matrix.get col (row + 1) newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "South link: Middle row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 3
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.South model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasSouthLink, hasNorthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        , case Matrix.get col (row + 1) newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "South link: Bottom row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 4
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.South model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasSouthLink, hasNorthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        , case Matrix.get col (row + 1) newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasSouthLink && not hasNorthLink) True
        , test "East link: Left column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 0
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.East model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasEastLink, hasWestLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        , case Matrix.get (col + 1) row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasEastLink && hasWestLink) True
        , test "East link: middle column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 2
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.East model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasEastLink, hasWestLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        , case Matrix.get (col + 1) row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasEastLink && hasWestLink) True
        , test "East link: right column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 4
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.East model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasEastLink, hasWestLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        , case Matrix.get (col + 1) row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasEastLink && not hasWestLink) True
        , test "West link: left column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 0
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.West model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasWestLink, hasEastLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        , case Matrix.get (col - 1) row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasWestLink && not hasEastLink) True
        , test "West link: middle column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 2
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.West model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasWestLink, hasEastLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        , case Matrix.get (col - 1) row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasWestLink && hasEastLink) True
        , test "West link: right column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 4
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.West model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasWestLink, hasEastLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        , case Matrix.get (col - 1) row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasWestLink && hasEastLink) True
        ]


discernLinkDirTests : Test
discernLinkDirTests =
    describe "discernLinkDir"
        [ test "East link"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 1, 1, 2, 1 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Just MG.East == dir) True
        , test "West link"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 1, 1, 0, 1 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Just MG.West == dir) True
        , test "North link"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 1, 1, 1, 0 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Just MG.North == dir) True
        , test "South link"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 1, 1, 1, 2 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Just MG.South == dir) True
        , test "diagonal is not a link"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 1, 1, 0, 0 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Nothing == dir) True
        , test "non-adjacent columns"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 0, 0, 2, 0 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Nothing == dir) True
        , test "non-adjacent rows"
            <| \() ->
                let
                    ( c1, r1, c2, r2 ) =
                        ( 2, 0, 2, 2 )

                    dir =
                        MG.discernLinkDir c1 r1 c2 r2
                in
                    Expect.equal (Nothing == dir) True
        ]


recursiveBacktrackerTests : Test
recursiveBacktrackerTests =
    describe "RecursiveBacktracker"
        [ describe "generateRecursiveBacktracker"
            [ test "returns complete if work field is empty"
                <| \() ->
                    let
                        ( _, isComplete ) =
                            MG.emptyModel
                                |> MG.initModel
                                |> MG.generateRecursiveBacktracker
                    in
                        Expect.equal isComplete True
            , test "returns not complete if work field is not empty"
                <| \() ->
                    let
                        newWork =
                            [ (2, 2) ]

                        ( _, isComplete ) =
                            MG.emptyModel
                                |> (\m -> { m | work = newWork })
                                |> MG.initModel
                                |> MG.generateRecursiveBacktracker
                    in
                        Expect.equal isComplete False
            , test "updates model.seed with a new value"
                <| \() ->
                    let
                        newWork =
                            [ (2, 2) ]

                        startingModel =
                            MG.emptyModel
                                |> (\m -> { m | work = newWork })
                                |> MG.initModel

                        ( newModel, _ ) =
                            MG.generateRecursiveBacktracker startingModel
                    in
                        Expect.equal (newModel.seed /= startingModel.seed) True
            , test "links to the neighbor unvisited if only one unvisited & all others unchanged"
                <| \() ->
                    let
                        -- Start with 1,0 and link to 0,0 then run test with
                        -- 0,0 at top of work stack.
                        ( col1, row1, col2, row2 ) =
                            ( 1, 0, 0, 0 )

                        -- The new cell that should be linked to.
                        ( col3, row3 ) =
                            ( 0, 1 )

                        -- Push onto the work stack.
                        newWork =
                            [ (col2, row2), (col1, row1) ]

                        -- Bring the cells into alignment with the work stack and
                        -- run the process.
                        ( model, _ ) =
                            MG.emptyModel
                                |> (\m -> { m | work = newWork })
                                |> MG.initModel
                                |> \model ->
                                    let
                                        newCells =
                                            model.cells
                                                |> Matrix.set col2 row2 (MG.Cell False True False False False)
                                                |> Matrix.set col1 row1 (MG.Cell False False False True False)
                                    in
                                        { model | cells = newCells }
                                            |> MG.generateRecursiveBacktracker

                        ( workList, cell1, cell2, cell3 ) =
                            ( List.take 3 model.work
                            , Matrix.get col1 row1 model.cells
                            , Matrix.get col2 row2 model.cells
                            , Matrix.get col3 row3 model.cells
                            )

                        finalCells =
                            Matrix.toIndexedArray model.cells
                                |> Array.filter
                                    (\( ( x, y ), cell ) ->
                                        if cell.northLink || cell.eastLink || cell.southLink || cell.westLink then
                                            True
                                        else
                                            False
                                    )

                        finalCellsOk =
                            Array.length finalCells == 3
                                && ( Array.filter (\((c, r), cell) -> c == col3 && r == row3) finalCells
                                        |> Array.length
                                        |> (==) 1
                                   )

                        cellOneOk =
                            case cell1 of
                                Just c ->
                                    not c.northLink && not c.eastLink && not c.southLink && c.westLink

                                Nothing ->
                                    False

                        cellTwoOk =
                            case cell2 of
                                Just c ->
                                    not c.northLink && c.eastLink && c.southLink && not c.westLink

                                Nothing ->
                                    False

                        cellThreeOk =
                            case cell3 of
                                Just c ->
                                    c.northLink && not c.eastLink && not c.southLink && not c.westLink

                                Nothing ->
                                    False
                    in
                        Expect.equal
                            ( cellOneOk
                                && cellTwoOk
                                && cellThreeOk
                                && finalCellsOk
                            )
                            True
            , fuzzWith { runs = 100 } (Fuzz.intRange 0 999999) "Updates the model with the contents of work"
                <| \fuzzInt ->
                    let
                        --_ =
                        --Debug.log "fuzzInt" <| toString fuzzInt
                        ( col1, row1 ) =
                            ( 0, 0 )

                        -- Potential cells chosen randomly, horizontal and vertical.
                        ( horCol, horRow, verCol, verRow ) =
                            ( 1, 0, 0, 1 )

                        -- Set our starting point so we know what to expect.
                        newWork =
                            [ (col1, row1) ]

                        -- Run it.
                        ( model, isComplete ) =
                            MG.emptyModel
                                |> (\m -> { m | work = newWork, mazeSize = 10, seed = Random.initialSeed fuzzInt })
                                |> MG.initModel
                                |> MG.generateRecursiveBacktracker

                        -- Retrieve the results from the model returned.
                        ( workList, cell1, horCell, verCell ) =
                            ( List.take 2 model.work
                            , Matrix.get col1 row1 model.cells
                            , Matrix.get horCol horRow model.cells
                            , Matrix.get verCol verRow model.cells
                            )

                        -- Check the results.
                        isOk =
                            -- Get the cell at the top of the stack so that we can check that
                            -- the corresponding cell has the proper direction set.
                            case List.head workList of
                                Just wu ->
                                    let
                                        --( c, r, cel ) =
                                            --MG.parseWorkUnit wu
                                        (c, r) =
                                            ( fst wu, snd wu )
                                    in
                                        case ( horCell, verCell ) of
                                            ( Just hc, Just vc ) ->
                                                if c == col1 then
                                                    -- Same column, so a vertical link.
                                                    vc.northLink
                                                else
                                                    if r == row1 then
                                                        -- Same row, so a horizontal link.
                                                        hc.westLink
                                                    else
                                                        False
                                            _ ->
                                                False

                                Nothing ->
                                    -- If everything is done, then this is correct.
                                    isComplete
                    in
                        Expect.equal isOk True
            ]
        ]


neighborsTests : Test
neighborsTests =
    describe "tests for neighbor related functions"
        [ describe "neighbors"
            [ test "returns south and east cells if in upper left"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 0, 0 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 0, 1 ), ( 1, 0 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns south, west, and east cells if in top row middle"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 5, 0 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 4, 0 ), ( 6, 0 ), ( 5, 1 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns south, west cells if in top row right"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 9, 0 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 8, 0 ), ( 9, 1 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns north, south, west cells if in middle row left"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 0, 5 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 0, 4 ), ( 1, 5 ), ( 0, 6 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns north, south, east, west cells if in middle row middle"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 5, 5 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 5, 4 ), ( 6, 5 ), ( 5, 6 ), ( 4, 5 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns north, south, west cells if in middle row right"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 9, 5 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 9, 4 ), ( 9, 6 ), ( 8, 5 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns north, west cells if in last row left"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 0, 9 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 0, 8 ), ( 1, 9 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns north, east, west cells if in last row middle"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 5, 9 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 5, 8 ), ( 6, 9 ), ( 4, 9 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            , test "returns north, west cells if in last row right"
                <| \() ->
                    let
                        ( col, row ) =
                            ( 9, 9 )

                        neighbors =
                            MG.emptyModel
                                |> (\m -> { m | mazeSize = 10 })
                                |> MG.initModel
                                |> .mazeSize
                                |> MG.neighbors col row

                        expected =
                            [ ( 9, 8 ), ( 8, 9 ) ]
                    in
                        Expect.equal (List.sort neighbors) (List.sort expected)
            ]
        ]


generateBinaryTreeTests : Test
generateBinaryTreeTests =
    describe "generateBinaryTreeStep"
        [ test "Middle cell can link south"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 2, 2 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeSouth initialModel

                    isLinkSouth =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkSouth True
        , test "Middle cell can link east"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 2, 2 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeEast initialModel

                    isLinkEast =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkEast True
        , test "Last col in row should link south"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 4, 1 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeEast initialModel

                    isLinkSouth =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkSouth True
        , test "A middle cell in last row should link east"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 3, 4 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeSouth initialModel

                    isLinkEast =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkEast True
        , test "Last cell of last row should not link east or south"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 4, 4 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeSouth initialModel

                    ( isEastLink, isSouthLink ) =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                ( c.eastLink, c.southLink )

                            Nothing ->
                                ( True, True )
                in
                    Expect.equal (isEastLink == False && isSouthLink == False) True
        ]


updateTests : Test
updateTests =
    describe "update"
        [ test "BinaryTreeInit creates maze of proper size and id"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 12, 100 )

                    ( mod, _ ) =
                        MG.update (MG.BinaryTreeInit ms id) MG.emptyModel
                in
                    Expect.equal (mod.mazeSize == ms && mod.mazeId == (Just id)) True
        , test "BinaryTreeInit creates Cmd msg"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 14, 200 )

                    ( _, cmd ) =
                        MG.update (MG.BinaryTreeInit ms id) MG.emptyModel
                in
                    Expect.equal (cmd /= Cmd.none) True
        , test "BinaryTreeInit does nothing when status /= Empty or Complete"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 15, 300 )

                    ( mod, _ ) =
                        MG.emptyModel
                            |> (\m -> { m | status = MG.InProcess })
                            |> MG.update (MG.BinaryTreeInit ms id)
                in
                    Expect.equal (mod.mazeSize /= ms && mod.mazeId /= (Just id)) True
        , test "RecursiveBacktrackerInit creates maze of proper size and id"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 12, 100 )

                    ( mod, _ ) =
                        MG.update (MG.RecursiveBacktrackerInit ms id) MG.emptyModel
                in
                    Expect.equal (mod.mazeSize == ms && mod.mazeId == (Just id)) True
        , test "RecursiveBacktrackerInit creates Cmd msg"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 14, 200 )

                    ( _, cmd ) =
                        MG.update (MG.RecursiveBacktrackerInit ms id) MG.emptyModel
                in
                    Expect.equal (cmd /= Cmd.none) True
        , test "RecursiveBacktrackerInit does nothing when status /= Empty or Complete"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 15, 300 )

                    ( mod, _ ) =
                        MG.emptyModel
                            |> (\m -> { m | status = MG.InProcess })
                            |> MG.update (MG.RecursiveBacktrackerInit ms id)
                in
                    Expect.equal (mod.mazeSize /= ms && mod.mazeId /= (Just id)) True
        ]


advanceByCellTests : Test
advanceByCellTests =
    describe "advanceByCell"
        [ test "Advances cell by one when not at end of row or last row and returns not compete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 14, 100, 1, 1 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal
                        (model.currCol
                            == (currCol + 1)
                            && model.currRow
                            == currRow
                            && isComplete
                            == False
                        )
                        True
        , test "Advances cell to next row when not at last row nor at end of column and returns not compete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 15, 100, 14, 0 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal (model.currCol == 0 && model.currRow == (currRow + 1) && isComplete == False)
                        True
        , test "Advances cell when on last row but not last cell and returns not complete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 15, 100, 12, 14 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal (model.currCol == (currCol + 1) && model.currRow == currRow && isComplete == False)
                        True
        , test "Does not advance cell when at last cell in last row and returns complete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 15, 100, 14, 14 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal (model.currCol == currCol && model.currRow == currRow && isComplete == True)
                        True
        ]


modelTests : Test
modelTests =
    describe "emptyModel and initModel"
        [ test "emptyModel should have proper fields"
            <| \() ->
                let
                    model =
                        MG.emptyModel

                    ( isEmpty, currPos, validStatus, unassignedId, validPerc ) =
                        ( Matrix.height model.cells == 0 && Matrix.width model.cells == 0
                        , model.currCol == 0 && model.currRow == 0
                        , model.status == MG.Empty
                        , model.mazeId == Nothing
                        , model.percComplete == 0
                        )
                in
                    Expect.equal (isEmpty && currPos && validStatus && unassignedId && validPerc) True
        , test "Create a maze with initModel of a certain size with proper number of cells"
            <| \() ->
                let
                    ms =
                        6

                    model =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms })
                            |> MG.initModel
                in
                    Expect.equal
                        ((model.mazeSize == ms)
                            && (Matrix.width model.cells == ms)
                            && (Matrix.height model.cells == ms)
                        )
                        True
        , test "Create a maze with initModel and all cells should have links set to False"
            <| \() ->
                let
                    ms =
                        7

                    numLinkedCells =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms })
                            |> MG.initModel
                            |> .cells
                            |> Matrix.filter
                                (\c ->
                                    c.northLink
                                        == True
                                        || c.eastLink
                                        == True
                                        || c.southLink
                                        == True
                                        || c.westLink
                                        == True
                                )
                            |> Array.length
                in
                    Expect.equal numLinkedCells 0
        ]
