module UI.BoardTest exposing (defaultBoardRenderingTests, inPlayBoardRenderingTests,
                        gameOverBoardRenderingTests)

import Test exposing (Test, describe, test, todo, fuzz, fuzz2, fuzz3)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, class)
import Test.Html.Event as Event exposing (click)
import Expect
import Fuzz exposing (intRange)

import Helpers exposing (standardBoardXwinsHorizontally, randomGameStatus,
                         randomGameOverStatus)

import Board exposing (Size(..), Mark(..))
import Game exposing (Status(..))
import Player exposing (Player(..))
import Msg exposing (Msg(..))
import UI.Board exposing (renderBoard)

defaultBoardRenderingTests : Test
defaultBoardRenderingTests =
    describe "Regardless of Game Status"
        [ describe "the UI should render the board"
            [ fuzz randomGameStatus "displaying as many cells as there are in the board" <|
                  \status->
                      Board.create Standard
                          |> renderBoard status (Just (Human X))
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.count (Expect.equal 9)

            , fuzz randomGameStatus "arranging the cells in as many rows as there are in the board" <|
                  \status ->
                      Board.create Standard
                          |> renderBoard status (Just (Human X))
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.count (Expect.equal 3)

            , fuzz randomGameStatus "displaying empty cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> renderBoard status (Just (Human X))
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.each (Query.has [class "empty"])

            , fuzz randomGameStatus "displaying crosses' cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> Board.markCell (3, X)
                          |> renderBoard status (Just (Human X))
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
                          |> renderBoard status (Just (Human X))
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
            [ fuzz2 (intRange 0 2) (intRange 0 2) "including move events for empty cells" <|
                  \row column->
                      Board.create Standard
                          |> renderBoard Ongoing (Just (Human X))
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.index row
                          |> Query.findAll [class "cell"]
                          |> Query.index column
                          |> Event.simulate click
                          |> Event.expect (MakeMove (twoDtoOneDIndex 3 row column))

            , test "not including move events for cells with a mark in them" <|
                  \() ->
                      Board.create Standard
                          |> Board.markCell (0, X)
                          |> renderBoard Ongoing (Just (Human O))
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.index 0
                          |> Query.findAll [class "cell"]
                          |> Query.index 0
                          |> Event.simulate click
                          |> Event.toResult
                          |> Expect.notEqual (Ok (MakeMove 0))

            , test "not including move events for cells if Computer player is next" <|
                  \() ->
                      Board.create Standard
                          |> renderBoard Ongoing (Just (Computer O))
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.index 0
                          |> Query.findAll [class "cell"]
                          |> Query.index 0
                          |> Event.simulate click
                          |> Event.toResult
                          |> Expect.notEqual (Ok (MakeMove 0))
            ]
        ]

gameOverBoardRenderingTests : Test
gameOverBoardRenderingTests =
    describe "When the game is over"
        [ describe "the UI should render the board"
            [ fuzz3 randomGameOverStatus (intRange 0 2) (intRange 0 2) "without any move events" <|
                \gameOverState row column ->
                    standardBoardXwinsHorizontally
                        |> renderBoard gameOverState Nothing
                        |> Query.fromHtml
                        |> Query.findAll [class "row"]
                        |> Query.index row
                        |> Query.findAll [class "cell"]
                        |> Query.index column
                        |> Event.simulate click
                        |> Event.toResult
                        |> Expect.notEqual (Ok (MakeMove (twoDtoOneDIndex 3 row column)))
            ]
        ]


twoDtoOneDIndex : Int -> Int -> Int -> Int
twoDtoOneDIndex boardWidth row column =
    (row * boardWidth) + column
