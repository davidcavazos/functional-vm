module WithInputTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Context withInput"
        [ test "after no result" <|
            \_ ->
                new
                    |> withInput "x" IntType
                    |> dump
                    |> Expect.equal "I x=Int"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> withInput "x" IntType
                    |> dump
                    |> Expect.equal "I x=Int;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> withInput "y" IntType
                    |> dump
                    |> Expect.equal "I y=Int;E NameNotFound x"

        --
        , test "NameAlreadyExists with name" <|
            \_ ->
                new
                    |> withInput "x" IntType
                    |> withInput "x" IntType
                    |> dump
                    |> Expect.equal "I x=Int;E NameAlreadyExists x"

        --
        , test "NameAlreadyExists with input" <|
            \_ ->
                new
                    |> withName "x" (Integer 1)
                    |> withInput "x" IntType
                    |> dump
                    |> Expect.equal "N x=1;E NameAlreadyExists x"

        --
        , test "many definitions" <|
            \_ ->
                new
                    |> withInput "x" IntType
                    |> withInput "y" IntType
                    |> dump
                    |> Expect.equal "I x=Int;I y=Int"
        ]
