module LambdaTest exposing (suite)

import Expect
import FVM exposing (Type(..), lambda, load, new)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "lambda"
        [ test "NameNotFound" <|
            \_ ->
                new
                    |> lambda ( "x", IntType ) (load "y")
                    |> dump
                    |> Expect.equal "V x=(x:Int);E NameNotFound y"

        --
        , test "using existing input" <|
            \_ ->
                new
                    |> lambda ( "x", IntType ) (load "x")
                    |> dump
                    |> Expect.equal "R (x:Int)->(x:Int)"
        ]
