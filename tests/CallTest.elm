module CallTest exposing (suite)

import Expect
import FVM exposing (Type(..), andThen, call, function, int, load, new, number, tuple)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "call"
        [ test "0 inputs" <|
            \_ ->
                new
                    |> int 1
                    |> andThen (call [])
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "CallNonFunction" <|
            \_ ->
                new
                    |> int 1
                    |> andThen (call [ int 2 ])
                    |> dump
                    |> Expect.equal "E CallNonFunction 1"

        --
        , test "1 input ignoring the input" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ) ] (int 1)
                    |> andThen (call [ int 2 ])
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "1 input using the input" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ) ] (load "x")
                    |> andThen (call [ int 1 ])
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "TypeMismatch on input" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ) ] (load "x")
                    |> andThen (call [ number 3.14 ])
                    |> dump
                    |> Expect.equal "E TypeMismatch 3.14 Int"

        --
        , test "CallTooManyInputs with function inputs" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ) ] (load "x")
                    |> andThen (call [ int 1, int 2 ])
                    |> dump
                    |> Expect.equal "V x=1;E CallTooManyInputs (x:Int)->(x:Int)"

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
