module TupleTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "tuple"
        [ test "0 items" <|
            \_ ->
                new
                    |> tuple []
                    |> dump
                    |> Expect.equal "R ()"

        --
        , test "NameNotFound with 1 item" <|
            \_ ->
                new
                    |> tuple [ load "x" ]
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "1 item" <|
            \_ ->
                new
                    |> tuple [ int 1 ]
                    |> dump
                    |> Expect.equal "R (1)"

        --
        , test "3 items" <|
            \_ ->
                new
                    |> tuple [ int 1, int 2, int 3 ]
                    |> dump
                    |> Expect.equal "R (1,2,3)"
        ]
