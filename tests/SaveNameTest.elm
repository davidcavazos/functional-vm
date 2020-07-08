module SaveNameTest exposing (suite)

import Expect
import FVM exposing (Error(..), Expression(..), Type(..), new, saveInput, saveName, withResult)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "saveName"
        [ test "after no result" <|
            \_ ->
                new
                    |> saveName "x" (Integer 1)
                    |> dump
                    |> Expect.equal "V x=1"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> saveName "x" (Integer 2)
                    |> dump
                    |> Expect.equal "V x=2;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> saveName "y" (Integer 1)
                    |> dump
                    |> Expect.equal "V y=1;E NameNotFound x"

        --
        , test "NameAlreadyExists with name" <|
            \_ ->
                new
                    |> saveName "x" (Integer 1)
                    |> saveName "x" (Integer 2)
                    |> dump
                    |> Expect.equal "V x=1;E NameAlreadyExists x"

        --
        , test "NameAlreadyExists with input" <|
            \_ ->
                new
                    |> saveInput "x" IntType
                    |> saveName "x" (Integer 2)
                    |> dump
                    |> Expect.equal "I x=Int;E NameAlreadyExists x"

        --
        , test "many definitions" <|
            \_ ->
                new
                    |> saveName "x" (Integer 1)
                    |> saveName "y" (Integer 2)
                    |> dump
                    |> Expect.equal "V x=1;V y=2"
        ]
