module LambdaTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "lambda"
        [ test "NameNotFound" <|
            \_ ->
                new
                    |> lambda ( "x", IntType ) (load "y")
                    |> dump
                    |> Expect.equal "I x=Int;E NameNotFound y"

        --
        , test "using existing input" <|
            \_ ->
                new
                    |> lambda ( "x", IntType ) (load "x")
                    |> dump
                    |> Expect.equal "R (x:Int)->x"
        ]
