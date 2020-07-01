module WithTypeTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Context withType"
        [ test "after no result" <|
            \_ ->
                new
                    |> withType "T" []
                    |> dump
                    |> Expect.equal "T T"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> withType "T" []
                    |> dump
                    |> Expect.equal "T T;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> withType "T" []
                    |> dump
                    |> Expect.equal "T T;E NameNotFound x"

        --
        , test "TypeAlreadyExists" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withType "T" []
                    |> dump
                    |> Expect.equal "T T;E TypeAlreadyExists T"

        --
        , test "many definitions" <|
            \_ ->
                new
                    |> withType "T1" []
                    |> withType "T2" []
                    |> dump
                    |> Expect.equal "T T1;T T2"

        --
        , test "with type inputs" <|
            \_ ->
                new
                    |> withType "T" [ IntType, NumberType ]
                    |> dump
                    |> Expect.equal "T T Int Number"
        ]
