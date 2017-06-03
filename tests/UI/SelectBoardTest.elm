module UI.SelectBoardTest exposing (selectBoardTests)

import Test exposing (Test, test, describe, fuzz, fuzz2)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, selected, attribute)
import Test.Html.Event as Event exposing (click)

import Helpers exposing (randomGameType, randomBoardSize)

import Main exposing (Msg(..), renderSelectBoard)
import Board exposing (Mark(..), Size(..))
import Game exposing (Player(..))
import GameGenerator exposing (GameType(..))

selectBoardTests : Test
selectBoardTests =
    Test.skip <| describe "The UI should render the select board element"
        [ fuzz2 randomGameType randomBoardSize "displaying the board type that is currently being played" <|
             \aGameType aBoardSize ->
                 renderSelectBoard aBoardSize aGameType
                     |> Query.fromHtml
                     |> Query.find [attribute "value" (toString aBoardSize)]
                     |> Query.has [selected True]

        , fuzz2 randomGameType randomBoardSize "including 'newGame' events for each board type and board size" <|
             \aGameType aBoardSize->
                 renderSelectBoard aBoardSize aGameType
                     |> Query.fromHtml
                     |> Event.simulate (Event.input (toString aBoardSize))
                     |> Event.expect (NewGame aBoardSize aGameType)
        ]

