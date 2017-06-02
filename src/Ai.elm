module Ai exposing(pickBestPosition)

import Board exposing (Position)
import Game exposing (Game)

pickBestPosition : Game -> Maybe Position
pickBestPosition game =
    Game.positionsAvailable game
        |> List.head
