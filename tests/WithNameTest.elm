module WithNameTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Context withName"
        [ test "after no result" <|
            \_ ->
                new
                    |> withName "x" (Integer 1)
                    |> dump
                    |> Expect.equal "N x=1"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> withName "x" (Integer 2)
                    |> dump
                    |> Expect.equal "N x=2;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> withName "y" (Integer 1)
                    |> dump
                    |> Expect.equal "N y=1;E NameNotFound x"

        --
        , test "NameAlreadyExists with name" <|
            \_ ->
                new
                    |> withName "x" (Integer 1)
                    |> withName "x" (Integer 2)
                    |> dump
                    |> Expect.equal "N x=1;E NameAlreadyExists x"

        --
        , test "NameAlreadyExists with input" <|
            \_ ->
                new
                    |> withInput "x" IntType
                    |> withName "x" (Integer 2)
                    |> dump
                    |> Expect.equal "I x=Int;E NameAlreadyExists x"

        --
        , test "many definitions" <|
            \_ ->
                new
                    |> withName "x" (Integer 1)
                    |> withName "y" (Integer 2)
                    |> dump
                    |> Expect.equal "N x=1;N y=2"
        ]
