module UI.ResetTest exposing (resetTests)

import Test exposing (Test, describe, fuzz3, fuzz2 )
import Expect
import Test.Html.Query as Query
import Test.Html.Event as Event exposing (click)
import Test.Html.Selector exposing (tag)

import Msg exposing (Msg(..))
import UI.ResetButton exposing (renderResetButton)
import Helpers exposing (randomGameOverStatus, randomGameType, randomBoardSize)
import Board exposing (Size(..), Mark(..))
import Game exposing (Status(..))
import GameGenerator exposing (GameType(..))

resetTests : Test
resetTests =
    describe "The reset button should"
        [ fuzz3 randomBoardSize randomGameType randomGameOverStatus
              "trigger a 'new game' event when clicked if the game is over" <|
                  \aBoardSize aGameType aGameOverStatus ->
                      renderResetButton aBoardSize aGameType aGameOverStatus
                          |> Query.fromHtml
                          |> Query.find [tag "button"]
                          |> Event.simulate click
                          |> Event.expect (NewGame aBoardSize aGameType)

        , fuzz2 randomBoardSize randomGameType
              "not be visible if the game is Ongoing" <|
                  \aBoardSize aGameType ->
                      renderResetButton aBoardSize aGameType Ongoing
                          |> Query.fromHtml
                          |> Query.findAll [tag "button"]
                          |> Query.count (Expect.equal 0)
        ]
