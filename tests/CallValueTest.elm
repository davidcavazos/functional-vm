module CallValueTest exposing (suite)

import Expect
import FVM exposing (Expression(..), Type(..), andThen, call, function, int, load, new, tuple)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "call Value"
        [ test "0 inputs on a non-function" <|
            \_ ->
                new
                    |> call [] (Integer 1)
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "CallNonFunction" <|
            \_ ->
                new
                    |> call [ int 1 ] (Integer 2)
                    |> dump
                    |> Expect.equal "E CallNonFunction 2"

        --
        , test "TypeMismatch on input" <|
            \_ ->
                new
                    |> call [ int 1 ]
                        (Lambda ( "x", NumberType ) (Integer 2))
                    |> dump
                    |> Expect.equal "E TypeMismatch 1 Number"

        --
        , test "1 input ignoring the input" <|
            \_ ->
                new
                    |> call [ int 1 ]
                        (Lambda ( "x", IntType ) (Integer 2))
                    |> dump
                    |> Expect.equal "R 2"

        --
        , test "1 input using the input" <|
            \_ ->
                new
                    |> call [ int 1 ]
                        (Lambda ( "x", IntType ) (Input "x" IntType))
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "CallTooManyInputs with function inputs" <|
            \_ ->
                new
                    |> call [ int 1, int 2 ]
                        (Lambda ( "x", IntType ) (Integer 3))
                    |> dump
                    |> Expect.equal "E CallTooManyInputs (x:Int)->3"

        --
        , test "call with too few inputs" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ), ( "y", NumberType ), ( "z", IntType ) ]
                        (tuple [ load "x", load "y", load "z" ])
                    |> andThen (call [ int 1 ])
                    |> dump
                    |> Expect.equal "R (y:Number)->(z:Int)->(1,(y:Number),(z:Int))"
        ]
