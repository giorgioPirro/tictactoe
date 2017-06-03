module UI.WhoseTurnTest exposing (whoseTurnTests)

import Test exposing (Test, test, describe)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, id)

import Main exposing (renderWhoseTurn)
import Game exposing (Player(..))
import Board exposing (Mark(..))

whoseTurnTests : Test
whoseTurnTests =
    describe "The turn display element"
        [ test "does not include the current turn class when it is nobody's turn" <|
              \() ->
                  renderWhoseTurn Nothing
                      |> Query.fromHtml
                      |> Query.findAll [class "turn-display"]
                      |> Query.each (Query.hasNot [class "turn-current"])

        , test "updates crosses' class when it's crosses' turn" <|
              \() ->
                  renderWhoseTurn (Just (Computer X))
                      |> Query.fromHtml
                      |> Query.find [id "turn-crosses"]
                      |> Query.has [class "turn-current"]

        , test "updates crosses' class when it's not its turn" <|
              \() ->
                  renderWhoseTurn (Just (Computer O))
                      |> Query.fromHtml
                      |> Query.find [id "turn-crosses"]
                      |> Query.hasNot [class "turn-current"]

        , test "updates noughts' class when it's noghts' turn" <|
              \() ->
                  renderWhoseTurn (Just (Human O))
                      |> Query.fromHtml
                      |> Query.find [id "turn-noughts"]
                      |> Query.has [class "turn-current"]

        , test "updates noughts' class when it's not its turn" <|
              \() ->
                  renderWhoseTurn (Just (Computer X))
                      |> Query.fromHtml
                      |> Query.find [id "turn-noughts"]
                      |> Query.hasNot [class "turn-current"]
        ]
