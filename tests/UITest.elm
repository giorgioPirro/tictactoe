module UITest exposing (defaultBoardRenderingTests, inPlayBoardRenderingTests)

import Test exposing (Test, describe, test, todo, fuzz, fuzz2)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, class)
import Test.Html.Events as Events exposing (Event(..))
import Expect
import Random.Pcg
import Fuzz exposing (intRange)
import Shrink

import Board exposing (Size(..), Mark(..))
import Game exposing (Status(..), Player(..))
import Main exposing (Msg(..), renderBoard)

type Msg = Ciao

defaultBoardRenderingTests : Test
defaultBoardRenderingTests =
    describe "Regardless of Game Status"
        [ describe "the UI should render the board"
            [ fuzz randomGameStatus "displaying as many cells as there are in the board" <|
                  \status->
                      Board.create Standard
                          |> renderBoard status (Human X)
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.count (Expect.equal 9)

            , fuzz randomGameStatus "arranging the cells in as many rows as there are in the board" <|
                  \status ->
                      Board.create Standard
                          |> renderBoard status (Human X)
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.count (Expect.equal 3)

            , fuzz randomGameStatus "displaying empty cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> renderBoard status (Human X)
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.each (Query.has [class "empty"])

            , fuzz randomGameStatus "displaying crosses' cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> Board.markCell (3, X)
                          |> renderBoard status (Human X)
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.index 3
                          |> Query.has [class "crosses"]

            , fuzz randomGameStatus "displaying noughts' cells with the appropriate class" <|
                  \status ->
                    let
                        a = Board.rowsWithPositions (Board.create Standard)
                    in
                      Board.create Standard
                          |> Board.markCell (5, O)
                          |> renderBoard status (Human X)
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.index 5
                          |> Query.has [class "noughts"]
            ]
        ]

inPlayBoardRenderingTests : Test
inPlayBoardRenderingTests =
    describe "When the game is Ongoing and Human is next"
        [ describe "the UI should render the board"
            [ fuzz2 (intRange 0 2) (intRange 0 2) "with the appropriate commands" <|
                  \row column->
                      Board.create Standard
                          |> renderBoard Ongoing (Human X)
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.index row
                          |> Query.findAll [class "cell"]
                          |> Query.index column
                          |> Events.simulate Click
                          |> Events.expectEvent (HumanMove (twoDtoOneDIndex 3 row column))
            ]
        ]


twoDtoOneDIndex : Int -> Int -> Int -> Int
twoDtoOneDIndex boardWidth row column =
    (row * boardWidth) + column


----HELPER FUZZER
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
intToStatus : Int -> Status
intToStatus statusId =
    case statusId of
        0 -> Ongoing
        1 -> Tie
        2 -> Win X
        _ -> Win O

randomStatusGenerator : Random.Pcg.Generator Status
randomStatusGenerator =
   Random.Pcg.map intToStatus (Random.Pcg.int 0 3)

randomGameStatus : Fuzz.Fuzzer Status
randomGameStatus =
    Fuzz.custom randomStatusGenerator Shrink.noShrink
----HELPER FUZZER
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
