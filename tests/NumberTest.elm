module NumberTest exposing (suite)

import Expect
import FVM exposing (new, number)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "number"
        [ test "value" <|
            \_ ->
                new
                    |> number 3.14
                    |> dump
                    |> Expect.equal "R 3.14"
        ]
