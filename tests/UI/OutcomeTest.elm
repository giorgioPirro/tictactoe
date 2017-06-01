module UI.OutcomeTest exposing (outcomeTests)

import Test exposing (Test, test, describe)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)

import Main exposing (renderGameStatus)
import Game exposing (Status(..))
import Board exposing (Mark(..))

outcomeTests : Test
outcomeTests =
  describe "The outcome box"
          [ describe "when the game is ongoing"
              [ test "does not show anything" <|
                    \() ->
                        renderGameStatus Ongoing
                            |> Query.fromHtml
                            |> Query.children [class "outcome-message"]
                            |> Query.count (Expect.equal 0)
              ]

          , describe "when the game has come to a tie"
              [ test "displays the appropriate tie message" <|
                    \() ->
                        renderGameStatus Tie
                            |> Query.fromHtml
                            |> Query.find [class "outcome-message"]
                            |> Query.has [text "It was a tie!!"]
              ]

          , describe "when there is a winner"
              [ test "displays the appropriate tie message (X)" <|
                    \() ->
                        renderGameStatus (Win X)
                            |> Query.fromHtml
                            |> Query.find [class "outcome-message"]
                            |> Query.has [text "X has won!!"]

              , test "displays the appropriate tie message (O)" <|
                    \() ->
                        renderGameStatus (Win O)
                            |> Query.fromHtml
                            |> Query.find [class "outcome-message"]
                            |> Query.has [text "O has won!!"]
              ]
          ]
