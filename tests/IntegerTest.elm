module IntegerTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
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
