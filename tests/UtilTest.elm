module UtilTest exposing (suite)

import Expect
import FVM.Util exposing (combinations)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "combinations"
        [ test "empty" <|
            \_ ->
                combinations []
                    |> Expect.equal []

        --
        , test "one item" <|
            \_ ->
                combinations [ [ "A1", "A2" ] ]
                    |> Expect.equal [ [ "A1" ], [ "A2" ] ]

        --
        , test "two items, one empty" <|
            \_ ->
                combinations [ [ "A1", "A2" ], [] ]
                    |> Expect.equal []

        --
        , test "two items" <|
            \_ ->
                combinations [ [ "A1", "A2" ], [ "B1", "B2", "B3" ] ]
                    |> Expect.equal
                        [ [ "A1", "B1" ]
                        , [ "A1", "B2" ]
                        , [ "A1", "B3" ]
                        , [ "A2", "B1" ]
                        , [ "A2", "B2" ]
                        , [ "A2", "B3" ]
                        ]

        --
        , test "three items" <|
            \_ ->
                combinations [ [ "A1", "A2" ], [ "B1", "B2", "B3" ], [ "C1", "C2" ] ]
                    |> Expect.equal
                        [ [ "A1", "B1", "C1" ]
                        , [ "A1", "B1", "C2" ]
                        , [ "A1", "B2", "C1" ]
                        , [ "A1", "B2", "C2" ]
                        , [ "A1", "B3", "C1" ]
                        , [ "A1", "B3", "C2" ]
                        , [ "A2", "B1", "C1" ]
                        , [ "A2", "B1", "C2" ]
                        , [ "A2", "B2", "C1" ]
                        , [ "A2", "B2", "C2" ]
                        , [ "A2", "B3", "C1" ]
                        , [ "A2", "B3", "C2" ]
                        ]
        ]
