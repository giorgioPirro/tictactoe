module BoardTest exposing (boardTests)

import Test exposing (Test, describe, test)
import Expect
import Array

import Helpers
import Board exposing (Size(..), Mark(..))

boardTests : Test
boardTests =
    describe "A Board should"
        [ test "have a standard size of nine cells" <|
              \() ->
                  Board.create Standard
                      |> Board.toList
                      |> List.length
                      |> Expect.equal 9

         , test "have a large size of sixteen cells" <|
              \() ->
                  Board.create Large
                      |> Board.toList
                      |> List.length
                      |> Expect.equal 16

        , test "have all cells empty by default" <|
              \() ->
                  Board.create Standard
                      |> Board.toList
                      |> List.all (\cell -> cell == Nothing)
                      |> Expect.true "all cells should be empty to begin with"

        , test "add a given mark at the given position (example with X)" <|
              \() ->
                  let
                      move = (7, X)
                  in
                      Board.create Standard
                          |> Board.markCell move
                          |> Board.toArray
                          |> Array.get 7
                          |> Expect.equal (Just (Just X))

        , test "add the given mark at the given position (example with O)" <|
              \() ->
                  let
                      move = (15, O)
                  in
                      Board.create Large
                          |> Board.markCell move
                          |> Board.toArray
                          |> Array.get 15
                          |> Expect.equal (Just (Just O))

        , test "provide a list of Cell rows (empty board)" <|
              \() ->
                  Board.create Standard
                      |> Board.rows
                      |> List.all (\row -> row == [Nothing, Nothing, Nothing])
                      |> Expect.true "a standard board should have 3 rows with 3 cells each"

        , test "provide a list of Cell rows (marked board)" <|
              \() ->
                  Board.create Large
                      |> Board.markCell (2, X)
                      |> Board.rows
                      |> List.head
                      |> Expect.equal (Just ([Nothing, Nothing, (Just X), Nothing]))

        , test "know that a new board is not full" <|
              \() ->
                  Board.create Standard
                      |> Board.isFull
                      |> Expect.false "board should not be full at this point"

        , test "know when a board is full" <|
              \() ->
                  Helpers.createDrawBoardStandardSized
                      |> Board.isFull
                      |> Expect.true "draw board should be full"

        , test "know when a cell is empty" <|
              \() ->
                  Board.create Standard
                      |> Board.isPositionAvailable 0
                      |> Expect.true "position should be available"

        , test "know when a cell is not empty" <|
              \() ->
                  Board.create Standard
                      |> Board.markCell (2, X)
                      |> Board.isPositionAvailable 2
                      |> Expect.false "position should not be available"

        , test "find no winning mark on an empty board" <|
              \() ->
                  Board.create Standard
                      |> Board.winningMark
                      |> Expect.equal Nothing

        , test "find the winning mark when there is one (horizontal X)" <|
              \() ->
                  Helpers.standardBoardXwinsHorizontally
                      |> Board.winningMark
                      |> Expect.equal (Just X)

        , test "find the winning mark when there is one (horizontal O)" <|
              \() ->
                  Helpers.standardBoardOwinsHorizontally
                      |> Board.winningMark
                      |> Expect.equal (Just O)

        , test "find the winning mark when there is one (vertical X)" <|
              \() ->
                  Helpers.standardBoardXwinsVertically
                      |> Board.winningMark
                      |> Expect.equal (Just X)

        , test "find the winning mark when there is one (vertical O)" <|
              \() ->
                  Helpers.standardBoardOwinsVertically
                      |> Board.winningMark
                      |> Expect.equal (Just O)

        , test "find the winning mark when there is one (downward diagonal X)" <|
              \() ->
                  Helpers.standardBoardXwinsDownDiagonal
                      |> Board.winningMark
                      |> Expect.equal (Just X)

        , test "find the winning mark when there is one (upward diagonal O)" <|
              \() ->
                  Helpers.standardBoardOwinsUpDiagonal
                      |> Board.winningMark
                      |> Expect.equal (Just O)
        ]
