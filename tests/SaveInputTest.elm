module SaveInputTest exposing (suite)

import Expect
import FVM exposing (Error(..), Expression(..), Type(..), new, saveInput, saveName, withResult)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "saveInput"
        [ test "after no result" <|
            \_ ->
                new
                    |> saveInput "x" IntType
                    |> dump
                    |> Expect.equal "I x=Int"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> saveInput "x" IntType
                    |> dump
                    |> Expect.equal "I x=Int;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> saveInput "y" IntType
                    |> dump
                    |> Expect.equal "I y=Int;E NameNotFound x"

        --
        , test "NameAlreadyExists with name" <|
            \_ ->
                new
                    |> saveInput "x" IntType
                    |> saveInput "x" IntType
                    |> dump
                    |> Expect.equal "I x=Int;E NameAlreadyExists x"

        --
        , test "NameAlreadyExists with input" <|
            \_ ->
                new
                    |> saveName "x" (Integer 1)
                    |> saveInput "x" IntType
                    |> dump
                    |> Expect.equal "V x=1;E NameAlreadyExists x"

        --
        , test "many definitions" <|
            \_ ->
                new
                    |> saveInput "x" IntType
                    |> saveInput "y" IntType
                    |> dump
                    |> Expect.equal "I x=Int;I y=Int"
        ]
