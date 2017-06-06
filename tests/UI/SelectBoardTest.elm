module UI.SelectBoardTest exposing (selectBoardTests)

import Test exposing (Test, describe, test, fuzz)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, selected, attribute, tag)
import Test.Html.Event as Event exposing (click)

import Helpers exposing (randomBoardSize)

import Msg exposing (Msg(..))
import UI.SelectNewGame exposing (renderSelectNewBoard)
import Board exposing (Mark(..), Size(..))
import Player exposing (Player(..))
import GameGenerator exposing (GameType(..))

selectBoardTests : Test
selectBoardTests =
    describe "The UI should render the select board element"
        [ fuzz randomBoardSize "displaying the board type that is currently being played (Standard)" <|
             \aBoardSize ->
                 renderSelectNewBoard aBoardSize HumanVsHuman
                     |> Query.fromHtml
                     |> Query.find [attribute "value" (toString aBoardSize)]
                     |> Query.has [selected True]

        , fuzz randomBoardSize "including 'newGame' events for each board size" <|
             \aBoardSize->
                 renderSelectNewBoard aBoardSize HumanVsHuman
                     |> Query.fromHtml
                     |> Event.simulate (Event.input (toString aBoardSize))
                     |> Event.expect (NewGame aBoardSize HumanVsHuman)

        , test "does not include any option for games that are not HumanVsHuman (HvC)" <|
            \() ->
                renderSelectNewBoard Standard HumanVsComputer
                     |> Query.fromHtml
                     |> Query.findAll [tag "option"]
                     |> Query.count (Expect.equal 0)

        , test "does not include any option for games that are not HumanVsHuman (CvC)" <|
            \() ->
                renderSelectNewBoard Standard ComputerVsComputer
                     |> Query.fromHtml
                     |> Query.findAll [tag "option"]
                     |> Query.count (Expect.equal 0)
        ]

