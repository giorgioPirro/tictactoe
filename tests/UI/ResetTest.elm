module UI.ResetTest exposing (resetTests)

import Test exposing (Test, test, describe)
import Expect
import Test.Html.Query as Query
import Test.Html.Event as Event exposing (click)

import Msg exposing (Msg(..))
import UI.ResetButton exposing (renderResetButton)
import Helpers exposing (createNewGame, createTieGameStandardSizedBoard)
import Board exposing (Size(..), Mark(..))
import Game exposing (Player(..))
import GameGenerator exposing (GameType(..))

resetTests : Test
resetTests =
    describe "The reset button should"
        [ test "trigger a 'new game' event when clicked (Human v Human), standard" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> renderResetButton
                      |> Query.fromHtml
                      |> Event.simulate click
                      |> Event.expect (NewGame Standard HumanVsHuman)

         , test "trigger a 'new game' event when clicked (Human v Computer), large" <|
              \() ->
                  createNewGame Large (Human X, Computer O)
                      |> renderResetButton
                      |> Query.fromHtml
                      |> Event.simulate click
                      |> Event.expect (NewGame Large HumanVsComputer)

         , test "trigger a 'new game' event when clicked (Computer v Computer), standard" <|
              \() ->
                  createNewGame Standard (Computer X, Computer O)
                      |> renderResetButton
                      |> Query.fromHtml
                      |> Event.simulate click
                      |> Event.expect (NewGame Standard ComputerVsComputer)
        ]
