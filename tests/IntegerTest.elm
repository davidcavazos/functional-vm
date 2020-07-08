module IntegerTest exposing (suite)

import Expect
import FVM exposing (int, new)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "int"
        [ test "value" <|
            \_ ->
                new
                    |> int 42
                    |> dump
                    |> Expect.equal "R 42"
        ]
