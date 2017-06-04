module Ai exposing(pickBestPosition)

import Board exposing (Position, Mark(..))
import Game exposing (Game, Status(..))
import Utilities.List exposing (maximumBy)

type alias Score = Int
type alias Depth = Int
type alias Alpha = Int
type alias Beta = Int

pickBestPosition : Game -> Maybe Position
pickBestPosition game =
    if ((List.length (Game.positionsAvailable game)) > 11) then
        List.head (Game.positionsAvailable game)
    else
      case (Game.whoseTurn game) of
          Nothing ->
              Nothing
          Just player ->
              Game.positionsAvailable game
                  |> maximumBy (rateMove game (Game.extractMark player) 0 -999999 999999)

rateMove : Game -> Mark -> Depth -> Alpha -> Beta -> Position -> Score
rateMove game maximisingMark depth alpha beta position =
    let
        gameAfterMove = Game.makeMove position game
    in
        if ((Game.gameIsOver (Game.status gameAfterMove)) || (depth > 4)) then
            rateGameOutcome gameAfterMove maximisingMark depth
        else
            rateOngoingGame gameAfterMove maximisingMark (depth + 1) alpha beta

rateOngoingGame : Game -> Mark -> Depth -> Alpha -> Beta -> Score
rateOngoingGame game maximisingMark depth alpha beta =
    let
        currentPlayer = Game.whoseTurn game
    in
        case currentPlayer of
            Nothing ->
                0
            Just player ->
                if (Game.extractMark player == maximisingMark) then
                    alphaBetaMaxReduce (rateMove game maximisingMark depth alpha beta) -999999 maximisingMark depth alpha beta (Game.positionsAvailable game) game
                else
                    alphaBetaMinReduce (rateMove game maximisingMark depth alpha beta) 999999 maximisingMark depth alpha beta (Game.positionsAvailable game) game

alphaBetaMaxReduce : (Position -> Score) -> Score -> Mark -> Depth -> Alpha -> Beta -> List Position -> Game -> Score
alphaBetaMaxReduce f bestScoreSoFar maximisingMark depth alpha beta positionsAvailable game =
    case positionsAvailable of
        [] ->
            bestScoreSoFar
        position::otherPositions ->
            if (beta <= alpha) then
                bestScoreSoFar
            else
                alphaBetaMaxReduce f (max bestScoreSoFar (rateMove game maximisingMark (depth + 1) alpha beta position)) maximisingMark depth (max bestScoreSoFar alpha) beta otherPositions game

alphaBetaMinReduce : (Position -> Score) -> Score -> Mark -> Depth -> Alpha -> Beta -> List Position -> Game -> Score
alphaBetaMinReduce f bestScoreSoFar maximisingMark depth alpha beta positionsAvailable game =
    case positionsAvailable of
        [] ->
            bestScoreSoFar
        position::otherPositions ->
            if (beta <= alpha) then
                bestScoreSoFar
            else
                alphaBetaMinReduce f (min bestScoreSoFar (rateMove game maximisingMark (depth + 1) alpha beta position)) maximisingMark depth alpha (min bestScoreSoFar beta) otherPositions game

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
