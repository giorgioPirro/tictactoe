module BoardTest exposing (boardTests)

import Test exposing (Test, describe, test)
import Expect

import Board exposing (Size(..), create, toList)


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
        ]
