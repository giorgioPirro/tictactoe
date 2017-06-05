module Ai exposing(pickBestPosition)

import Board exposing (Position, Mark(..))
import Game exposing (Game, Status(..))
import Utilities.List exposing (maximumBy)

type alias Score = Int
type alias Depth = Int
type alias Alpha = Int
type alias Beta = Int

infinity : Int
infinity =
    round (1/0)

initialDepth : Int
initialDepth =
    0

pickBestPosition : Game -> Maybe Position
pickBestPosition game =
    case (Game.whoseTurn game) of
        Nothing ->
            Nothing
        Just player ->
            let
                maximisingMark = Game.extractMark player
                positionsAvailable = Game.positionsAvailable game
            in
                maximumBy
                    (rateMove game maximisingMark initialDepth -infinity infinity)
                    positionsAvailable

rateMove : Game -> Mark -> Depth -> Alpha -> Beta -> Position -> Score
rateMove game maximisingMark depth alpha beta position =
    let
        gameAfterMove = Game.makeMove position game
    in
        if (shouldStopEvaluating gameAfterMove depth) then
            rateGameOutcome gameAfterMove maximisingMark depth
        else
            rateOngoingGame gameAfterMove maximisingMark (depth + 1) alpha beta

shouldStopEvaluating : Game -> Depth -> Bool
shouldStopEvaluating game depth =
    (isGameOver game) ||
    (depth > 4)       ||
    (moreThanNmovesToEvaluate 11 game) ||
    ((moreThanNmovesToEvaluate 7 game) && (depth > 1))

isGameOver : Game -> Bool
isGameOver game =
    (Game.gameIsOver (Game.status game))

moreThanNmovesToEvaluate : Int -> Game -> Bool
moreThanNmovesToEvaluate moves game =
    (List.length (Game.positionsAvailable game)) > moves

rateOngoingGame : Game -> Mark -> Depth -> Alpha -> Beta -> Score
rateOngoingGame game maximisingMark depth alpha beta =
    let
        currentPlayer = Game.whoseTurn game
        positionsAvailable = Game.positionsAvailable game
    in
        case currentPlayer of
            Nothing ->
                0
            Just player ->
                if (Game.extractMark player == maximisingMark) then
                    findHighestScoringMove
                        -infinity maximisingMark depth alpha beta
                        game positionsAvailable
                else
                    findLowestScoringMove
                        infinity maximisingMark depth alpha beta
                         game positionsAvailable

findHighestScoringMove : Score -> Mark -> Depth -> Alpha -> Beta -> Game -> List Position -> Score
findHighestScoringMove bestScoreSoFar maximisingMark depth alpha beta game positionsAvailable =
    case positionsAvailable of
        [] ->
            bestScoreSoFar
        position::otherPositions ->
            if (beta <= alpha) then
                bestScoreSoFar
            else
                findHighestScoringMove
                    (max bestScoreSoFar (rateMove game maximisingMark depth alpha beta position))
                    maximisingMark depth (max bestScoreSoFar alpha) beta game otherPositions

findLowestScoringMove : Score -> Mark -> Depth -> Alpha -> Beta -> Game -> List Position -> Score
findLowestScoringMove bestScoreSoFar maximisingMark depth alpha beta game positionsAvailable =
    case positionsAvailable of
        [] ->
            bestScoreSoFar
        position::otherPositions ->
            if (beta <= alpha) then
                bestScoreSoFar
            else
                findLowestScoringMove
                    (min bestScoreSoFar (rateMove game maximisingMark depth alpha beta position))
                    maximisingMark depth alpha (min bestScoreSoFar beta) game otherPositions

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
