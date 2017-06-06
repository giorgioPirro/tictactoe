module Utilities.ListTest exposing (listTests)

import Utilities.List exposing (removeWhen, findFirstWhere, allItemsAreEqual,
                                transpose, chunkify, maximumBy)

import Test exposing (Test, describe, test)
import Expect

listTests : Test
listTests =
    describe "List utilities"
        [ describe "removeWhen should"
            [ test "not alter an empty list" <|
                  \() ->
                      []
                          |> removeWhen (always True)
                          |> Expect.equal []
            , test "remove elements according to the predicate" <|
                  \() ->
                      [33, 42]
                          |> removeWhen (\item -> item == 33)
                          |> Expect.equal [42]
            ]

        , describe "findFirstWhere should" <|
            [ test "find nothing in an empty list" <|
                  \() ->
                      []
                          |> findFirstWhere (always True)
                          |> Expect.equal Nothing

            , test "find nothing if no item satisfies the predicate" <|
                  \() ->
                      [33, 22, 55]
                          |> findFirstWhere (\item -> item == 42)
                          |> Expect.equal Nothing

            , test "find the first item that satisfies the predicate" <|
                  \() ->
                      [33, 42, 55, 42, 42]
                          |> findFirstWhere (\item -> item == 42)
                          |> Expect.equal (Just 42)
            ]

        , describe "allItemsAreEqual should" <|
            [ test "know when not all items are the same" <|
                  \() ->
                      [33, 42, 55, 42, 42]
                          |> allItemsAreEqual
                          |> Expect.false "they are not all equal"

            , test "know when all items are the same" <|
                  \() ->
                      [42, 42, 42]
                          |> allItemsAreEqual
                          |> Expect.true "they are all equal now"
            ]

        , describe "transpose should" <|
            [ test "transpose a list of lists" <|
                  \() ->
                      [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
                          |> transpose
                          |> Expect.equal [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            ]

        , describe "chunkify should" <|
            [ test "leave an empty list alone" <|
                  \() ->
                      []
                          |> chunkify 2
                          |> Expect.equal []

            , test "generate a new list with the given chunk size" <|
                  \() ->
                      [1, 1, 2, 2, 42, 42]
                          |> chunkify 2
                          |> Expect.equal [[1, 1], [2, 2], [42, 42]]
            ]

        , describe "maximumBy should" <|
            [ test "find no maximum in an empty list" <|
                  \() ->
                      []
                          |> maximumBy (\a -> a)
                          |> Expect.equal Nothing

            , test "find the maximum elment after applying the given funciton" <|
                  \() ->
                      ["ciao", "cats", "forty-two"]
                          |> maximumBy (\string -> String.length string)
                          |> Expect.equal (Just "forty-two")
            ]
        ]

