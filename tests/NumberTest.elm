module NumberTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
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
