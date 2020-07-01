module LoadTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "load"
        [ test "undefined name" <|
            \_ ->
                new
                    |> load "x"
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "defined name" <|
            \_ ->
                new
                    |> withName "x" (Integer 1)
                    |> load "x"
                    |> dump
                    |> Expect.equal "N x=1;R 1"

        --
        , test "defined input" <|
            \_ ->
                new
                    |> withInput "x" IntType
                    |> load "x"
                    |> dump
                    |> Expect.equal "I x=Int;R x"
        ]
