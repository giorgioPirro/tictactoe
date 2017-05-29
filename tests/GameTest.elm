module GameTest exposing (gameTests)

import Test exposing (Test, describe, test)
import Expect
import Array

import Game
import Board exposing(Size(..), Mark(..))

gameTests : Test
gameTests =
    describe "A Game should"
        [ test "know whose turn it is at the start of a game" <|
              \() ->
                  let
                      newBoard = Board.create Standard
                      newGame  = Game.create X newBoard
                  in
                      newGame
                          |> Game.whoseTurn
                          |> Expect.equal X

        , test "update the current player after a move is made (X starts)" <|
              \() ->
                  let
                      newBoard = Board.create Standard
                      newGame  = Game.create X newBoard
                  in
                      newGame
                          |> Game.makeMove 1
                          |> Game.whoseTurn
                          |> Expect.equal O

        , test "update the current player after a move is made (O starts)" <|
              \() ->
                  let
                      newBoard = Board.create Standard
                      newGame  = Game.create O newBoard
                  in
                      newGame
                          |> Game.makeMove 1
                          |> Game.whoseTurn
                          |> Expect.equal X

        , test "update the board after a move is made (X starts)" <|
              \() ->
                  let
                      newBoard = Board.create Standard
                      newGame  = Game.create X newBoard
                  in
                      newGame
                          |> Game.makeMove 3
                          |> Game.getBoard
                          |> Board.toArray
                          |> Array.get 3
                          |> Expect.equal (Just (Just X))

        , test "update the board after a move is made (O starts)" <|
              \() ->
                  let
                      newBoard = Board.create Standard
                      newGame  = Game.create O newBoard
                  in
                      newGame
                          |> Game.makeMove 5
                          |> Game.getBoard
                          |> Board.toArray
                          |> Array.get 5
                          |> Expect.equal (Just (Just O))
        ]
