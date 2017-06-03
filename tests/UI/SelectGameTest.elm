module UI.SelectGameTest exposing (selectGameTests)

import Test exposing (Test, test, describe, fuzz, fuzz2)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, selected, attribute)
import Test.Html.Event as Event exposing (click)

import Helpers exposing (randomGameType, randomBoardSize)

import Msg exposing (Msg(..))
import Main exposing (renderSelectNewGame)
import Board exposing (Mark(..), Size(..))
import Game exposing (Player(..))
import GameGenerator exposing (GameType(..))

selectGameTests : Test
selectGameTests =
    describe "The UI should render the select game element"
        [ fuzz2 randomGameType randomBoardSize "displaying the game type that is currently being played" <|
             \aGameType aBoardSize ->
                 renderSelectNewGame aBoardSize aGameType
                     |> Query.fromHtml
                     |> Query.find [attribute "value" (toString aGameType)]
                     |> Query.has [selected True]

        , fuzz2 randomGameType randomBoardSize "including 'newGame' events for each game type and board size" <|
             \aGameType aBoardSize->
                 renderSelectNewGame aBoardSize aGameType
                     |> Query.fromHtml
                     |> Event.simulate (Event.input (toString aGameType))
                     |> Event.expect (NewGame aBoardSize aGameType)
        ]
