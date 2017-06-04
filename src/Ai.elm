module Ai exposing(pickBestPosition)

import Board exposing (Position, Mark(..))
import Game exposing (Game, Status(..))
import Utilities.List exposing (maximumBy)

type alias Score = Int
type alias Depth = Int

pickBestPosition : Game -> Maybe Position
pickBestPosition game =
    if ((List.length (Game.positionsAvailable game)) > 8) then
        List.head (Game.positionsAvailable game)
    else
      case (Game.whoseTurn game) of
          Nothing ->
              Nothing
          Just player ->
              Game.positionsAvailable game
                  |> maximumBy (rateMove game (Game.extractMark player) 0)

rateMove : Game -> Mark -> Depth -> Position -> Score
rateMove game maximisingMark depth position =
    let
        gameAfterMove = Game.makeMove position game
    in
        if ((Game.gameIsOver (Game.status gameAfterMove)) || (depth > 4)) then
            rateGameOutcome gameAfterMove maximisingMark depth
        else
            rateOngoingGame gameAfterMove maximisingMark (depth + 1)

rateOngoingGame : Game -> Mark -> Depth -> Score
rateOngoingGame game maximisingMark depth =
    let
        currentPlayer = Game.whoseTurn game
    in
        case currentPlayer of
            Nothing ->
                0
            Just player ->
                if (Game.extractMark player == maximisingMark) then
                    Game.positionsAvailable game
                      |> List.map (rateMove game maximisingMark depth)
                      |> getMaximumOrZero
                else
                    Game.positionsAvailable game
                      |> List.map (rateMove game maximisingMark depth)
                      |> getMinimumOrZero

rateGameOutcome : Game -> Mark -> Depth -> Score
rateGameOutcome game maximisingMark depth =
    case (Game.status game) of
        Win winningMark ->
            if (winningMark == maximisingMark) then
                100 - depth
            else
                depth - 100
        _ ->
            0

getMaximumOrZero : List Int -> Score
getMaximumOrZero list =
    case List.maximum list of
        Just max -> max
        Nothing -> 0

getMinimumOrZero : List Int -> Score
getMinimumOrZero list =
    case List.minimum list of
        Just min -> min
        Nothing -> 0
