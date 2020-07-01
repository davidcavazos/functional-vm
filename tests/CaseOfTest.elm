module CaseOfTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "caseOf"
        [ test "CaseWithoutPatterns" <|
            \_ ->
                new
                    |> int 1
                    |> andThen (caseOf [])
                    |> dump
                    |> Expect.equal "E CaseWithoutPatterns 1"
        ]
