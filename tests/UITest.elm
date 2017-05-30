module UITest exposing (uiTests)

import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, class)
import Expect

import Board exposing (Size(..), Mark(..))
import Main exposing (renderBoard)

uiTests : Test
uiTests =
    describe "The UI should"
        [ describe "render the board"
            [ test "displaying as many cells as there are in the board" <|
                  \() ->
                      Board.create Standard
                          |> renderBoard
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.count (Expect.equal 9)

            , test "showing empty cells with the appropriate class" <|
                  \() ->
                      Board.create Standard
                          |> renderBoard
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.each (Query.has [class "empty"])

            , test "showing crosses' cells with the appropriate class" <|
                  \() ->
                      Board.create Standard
                          |> Board.markCell (3, X)
                          |> renderBoard
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.index 3
                          |> Query.has [class "crosses"]

            , test "showing noughts' cells with the appropriate class" <|
                  \() ->
                      Board.create Standard
                          |> Board.markCell (5, O)
                          |> renderBoard
                          |> Query.fromHtml
                          |> Query.findAll [class "cell"]
                          |> Query.index 5
                          |> Query.has [class "noughts"]
            ]
        ]
