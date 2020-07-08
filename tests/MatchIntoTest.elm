module MatchIntoTest exposing (suite)

import Expect
import FVM exposing (Pattern(..), Type(..), andThen, int, load, matchInto, new, number)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "matchInto"
        [ test "MatchMissingCases with empty cases" <|
            \_ ->
                new
                    |> int 1
                    |> andThen (matchInto IntType [])
                    |> dump
                    |> Expect.equal "E MatchMissingCases"

        --
        , test "MatchPatternTypeMismatch" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( NumberPattern 3.14, int 2 ) ]
                        )
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch 1"

        --
        , test "MatchMissingCases with non-empty cases" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( IntPattern 2, int 2 ) ]
                        )
                    |> dump
                    |> Expect.equal "E MatchMissingCases"

        --
        , test "TypeMismatch on case matched" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( IntPattern 1, number 3.14 ) ]
                        )
                    |> dump
                    |> Expect.equal "E TypeMismatch 3.14 Int"

        --
        , test "TypeMismatch on case not matched" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( IntPattern 2, number 3.14 ) ]
                        )
                    |> dump
                    |> Expect.equal "E TypeMismatch 3.14 Int"

        --
        , test "exact match" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( IntPattern 1, int 2 ) ]
                        )
                    |> dump
                    |> Expect.equal "R 2"

        --
        , test "match on anything pattern" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( AnythingPattern Nothing, int 2 ) ]
                        )
                    |> dump
                    |> Expect.equal "R 2"

        --
        , test "match on anything pattern with name" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( AnythingPattern (Just "x"), load "x" ) ]
                        )
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "MatchCaseAlreadyCovered with duplicate pattern" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( IntPattern 1, int 2 )
                            , ( IntPattern 1, int 3 )
                            ]
                        )
                    |> dump
                    |> Expect.equal "E MatchCaseAlreadyCovered"

        --
        , test "MatchCaseAlreadyCovered with anything pattern" <|
            \_ ->
                new
                    |> int 1
                    |> andThen
                        (matchInto IntType
                            [ ( AnythingPattern Nothing, int 2 )
                            , ( IntPattern 1, int 3 )
                            ]
                        )
                    |> dump
                    |> Expect.equal "E MatchCaseAlreadyCovered"
        ]
