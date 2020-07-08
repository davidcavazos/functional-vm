module FunctionTest exposing (suite)

import Expect
import FVM exposing (Type(..), function, int, load, new)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "function"
        [ test "0 inputs" <|
            \_ ->
                new
                    |> function [] (int 1)
                    |> dump
                    |> Expect.equal "R 1"

        --
        , test "1 input" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ) ] (load "x")
                    |> dump
                    |> Expect.equal "R (x:Int)->(x:Int)"

        --
        , test "3 inputs" <|
            \_ ->
                new
                    |> function [ ( "x", IntType ), ( "y", NumberType ), ( "z", TupleType [] ) ] (load "z")
                    |> dump
                    |> Expect.equal "R (x:Int)->(y:Number)->(z:())->(z:())"
        ]
