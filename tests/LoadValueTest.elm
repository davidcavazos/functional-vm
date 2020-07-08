module LoadValueTest exposing (suite)

import Expect
import FVM exposing (Expression(..), Type(..), load, new, saveInput, saveName)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "load Value"
        [ test "NameNotFound" <|
            \_ ->
                new
                    |> load "x"
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "defined name" <|
            \_ ->
                new
                    |> saveName "x" (Integer 1)
                    |> load "x"
                    |> dump
                    |> Expect.equal "V x=1;R 1"
        ]
