module BoardTest exposing (boardTests)

import Test exposing (Test, describe, test)
import Expect
import Array exposing (get)

import Board exposing (Size(..), Mark(..), create, toList, toArray)


boardTests : Test
boardTests =
    describe "A Board should"
        [ test "have a standard size of nine cells" <|
              \() ->
                  Board.create Standard
                      |> Board.toList
                      |> List.length
                      |> Expect.equal 9

        , test "have all cells empty by default" <|
              \() ->
                  Board.create Standard
                      |> Board.toList
                      |> List.all (\cell -> cell == Nothing)
                      |> Expect.true "all cells should be empty to begin with"

        , test "add the given mark at the given position (example one)" <|
              \() ->
                  let
                      move = (7, X)
                  in
                      Board.create Standard
                          |> Board.markCell move
                          |> Board.toArray
                          |> Array.get 7
                          |> Expect.equal (Just (Just X))

        , test "add the given mark at the given position (example two)" <|
              \() ->
                  let
                      move = (5, O)
                  in
                      Board.create Standard
                          |> Board.markCell move
                          |> Board.toArray
                          |> Array.get 5
                          |> Expect.equal (Just (Just O))
        ]
