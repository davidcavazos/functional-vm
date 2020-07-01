module ConstructorTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "constructor"
        [ test "TypeNotFound" <|
            \_ ->
                new
                    |> constructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "E TypeNotFound T"

        --
        , test "0 inputs" <|
            \_ ->
                new
                    |> withType "T" []
                    |> constructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T;R T.A"

        --
        , test "1 input" <|
            \_ ->
                new
                    |> withType "T" []
                    |> constructor ( "T", [] ) "A" [ int 1 ]
                    |> dump
                    |> Expect.equal "T T;R (T.A 1)"

        --
        , test "3 inputs" <|
            \_ ->
                new
                    |> withType "T" []
                    |> constructor ( "T", [] ) "A" [ int 1, int 2, int 3 ]
                    |> dump
                    |> Expect.equal "T T;R (T.A 1 2 3)"

        --
        , test "type checking" <|
            \_ ->
                new
                    |> withType "T" []
                    |> constructor ( "T", [ int 1 ] ) "A" []
                    |> dump
                    |> Expect.equal "T T;E TypeInputsMismatch T"
        ]
