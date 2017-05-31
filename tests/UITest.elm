module UITest exposing (defaultBoardRenderingTests, inPlayBoardRenderingTests)

import Test exposing (Test, describe, test, todo, fuzz)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, class)
import Expect
import Random.Pcg
import Fuzz
import Shrink

import Board exposing (Size(..), Mark(..))
import Game exposing (Status(..))
import Main exposing (renderBoard)

defaultBoardRenderingTests : Test
defaultBoardRenderingTests =
    describe "Regardless of Game Status"
        [ describe "the UI should render the board"
            [ fuzz randomGameStatus "displaying as many cells as there are in the board" <|
                  \status->
                      Board.create Standard
                          |> renderBoard status
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.count (Expect.equal 9)

            , fuzz randomGameStatus "arranging the cells in as many rows as there are in the board" <|
                  \status ->
                      Board.create Standard
                          |> renderBoard status
                          |> Query.fromHtml
                          |> Query.findAll [class "row"]
                          |> Query.count (Expect.equal 3)

            , fuzz randomGameStatus "displaying empty cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> renderBoard status
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.each (Query.has [class "empty"])

            , fuzz randomGameStatus "displaying crosses' cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> Board.markCell (3, X)
                          |> renderBoard status
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.index 3
                          |> Query.has [class "crosses"]

            , fuzz randomGameStatus "displaying noughts' cells with the appropriate class" <|
                  \status ->
                      Board.create Standard
                          |> Board.markCell (5, O)
                          |> renderBoard status
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.index 5
                          |> Query.has [class "noughts"]
            ]
        ]

inPlayBoardRenderingTests : Test
inPlayBoardRenderingTests =
    describe "When the game is Ongoing"
        [ describe "the UI should render the board"
            [ todo "with the appropriate commands"
            ]
        ]




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
