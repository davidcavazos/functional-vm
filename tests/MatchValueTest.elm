module MatchValueTest exposing (suite)

import Expect
import FVM exposing (Expression(..), Pattern(..), Type(..), int, load, matchInto, new, number)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "matchInto with values"
        [ test "MatchMissingCases with empty cases" <|
            \_ ->
                new
                    |> matchInto IntType [] (Integer 0)
                    |> dump
                    |> Expect.equal "E MatchMissingCases"

        --
        , test "MatchPatternTypeMismatch" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( NumberPattern 3.14, int 1 ) ]
                        (Integer 0)
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch 0"

        --
        , test "MatchMissingCases with non-empty cases" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( IntPattern 1, int 11 ) ]
                        (Integer 0)
                    |> dump
                    |> Expect.equal "E MatchMissingCases"

        --
        , test "TypeMismatch on case matched" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( IntPattern 1, number 3.14 ) ]
                        (Integer 1)
                    |> dump
                    |> Expect.equal "E TypeMismatch 3.14 Int"

        --
        , test "TypeMismatch on case not matched" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( IntPattern 1, number 3.14 ) ]
                        (Integer 0)
                    |> dump
                    |> Expect.equal "E TypeMismatch 3.14 Int"

        --
        , test "exact match" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( IntPattern 1, int 11 ) ]
                        (Integer 1)
                    |> dump
                    |> Expect.equal "R 11"

        --
        , test "match on anything pattern" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( AnythingPattern Nothing, int 11 ) ]
                        (Integer 0)
                    |> dump
                    |> Expect.equal "R 11"

        --
        , test "match on anything pattern with name" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( AnythingPattern (Just "x"), load "x" ) ]
                        (Integer 0)
                    |> dump
                    |> Expect.equal "R 0"

        --
        , test "MatchCaseAlreadyCovered with duplicate pattern" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( IntPattern 1, int 11 )
                        , ( IntPattern 1, int 22 )
                        ]
                        (Integer 1)
                    |> dump
                    |> Expect.equal "E MatchCaseAlreadyCovered"

        --
        , test "MatchCaseAlreadyCovered with anything pattern" <|
            \_ ->
                new
                    |> matchInto IntType
                        [ ( AnythingPattern Nothing, int 11 )
                        , ( IntPattern 1, int 22 )
                        ]
                        (Integer 1)
                    |> dump
                    |> Expect.equal "E MatchCaseAlreadyCovered"
        ]
