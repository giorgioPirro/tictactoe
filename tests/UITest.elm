module UITest exposing (uiTests)

import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, class)
import Expect

import Board exposing (Size(..))
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
            ]
        ]
