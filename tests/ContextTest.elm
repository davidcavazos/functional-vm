module ContextTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Context"
        [ -- new
          test "new" <|
            \_ ->
                new
                    |> dump
                    |> Expect.equal ""

        -- withResult
        , test "withResult Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 42))
                    |> dump
                    |> Expect.equal "R 42"

        --
        , test "withoutResult Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        -- andThen
        , test "andThen after no result" <|
            \_ ->
                new
                    |> andThen (\x -> withResult (Ok x))
                    |> dump
                    |> Expect.equal ""

        --
        , test "andThen after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> andThen (\x -> withResult (Ok (Tuple [ x, Integer 2 ])))
                    |> dump
                    |> Expect.equal "R (1,2)"

        --
        , test "andThen after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> andThen (\x -> withResult (Ok (Tuple [ x, Integer 2 ])))
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        -- followedBy
        , test "followedBy after no result" <|
            \_ ->
                new
                    |> followedBy (withResult (Ok (Integer 1)))
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "followedBy after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> followedBy (withResult (Ok (Integer 2)))
                    |> dump
                    |> Expect.equal "R 2"

        --
        , test "followedBy after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> followedBy (withResult (Ok (Integer 1)))
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        -- orElse
        , test "orElse after no result" <|
            \_ ->
                new
                    |> orElse (withResult (Ok (Integer 1)))
                    |> dump
                    |> Expect.equal ""

        --
        , test "orElse after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> orElse (withResult (Ok (Integer 2)))
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "orElse after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> orElse (withResult (Ok (Integer 1)))
                    |> dump
                    |> Expect.equal "R 1"

        -- succeed
        , test "succeed after no result" <|
            \_ ->
                new
                    |> succeed (Integer 1)
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "succeed after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> succeed (Integer 2)
                    |> dump
                    |> Expect.equal "R 2"

        --
        , test "succeed after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> succeed (Integer 1)
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        -- fail
        , test "fail after no result" <|
            \_ ->
                new
                    |> fail (NameNotFound "x")
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "fail after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> fail (NameNotFound "x")
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "fail after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> fail (NameNotFound "y")
                    |> dump
                    |> Expect.equal "E NameNotFound y"
        ]
