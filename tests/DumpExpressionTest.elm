module DumpExpressionTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Bitcode exposing (dumpExpression)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "dumpExpression"
        [ -- Integer
          test "Integer: 42" <|
            \_ ->
                dumpExpression (Integer 42)
                    |> Expect.equal "42"

        --
        , test "Integer: -42" <|
            \_ ->
                dumpExpression (Integer -42)
                    |> Expect.equal "-42"

        -- Number
        , test "Number: 3.14" <|
            \_ ->
                dumpExpression (Number 3.14)
                    |> Expect.equal "3.14"

        --
        , test "Number: -3.14" <|
            \_ ->
                dumpExpression (Number -3.14)
                    |> Expect.equal "-3.14"

        --
        , test "Number: 42" <|
            \_ ->
                dumpExpression (Number 42)
                    |> Expect.equal "42"

        -- Tuple
        , test "Tuple: ()" <|
            \_ ->
                dumpExpression (Tuple [])
                    |> Expect.equal "()"

        --
        , test "Tuple: (42, 3.14)" <|
            \_ ->
                dumpExpression (Tuple [ Integer 42, Number 3.14 ])
                    |> Expect.equal "(42,3.14)"

        -- Record
        , test "Record: {}" <|
            \_ ->
                dumpExpression (Record Dict.empty)
                    |> Expect.equal "{}"

        --
        , test "Record: {x = 42, y = 3.14}" <|
            \_ ->
                dumpExpression (Record (Dict.fromList [ ( "x", Integer 42 ), ( "y", Number 3.14 ) ]))
                    |> Expect.equal "{x=42,y=3.14}"

        -- Constructor
        , test "Constructor: Bool.True" <|
            \_ ->
                dumpExpression (Constructor ( "Bool", [] ) "True" [])
                    |> Expect.equal "Bool.True"

        --
        , test "Constructor: (Pair.Ints 1 2)" <|
            \_ ->
                dumpExpression (Constructor ( "Pair", [] ) "Ints" [ Integer 1, Integer 2 ])
                    |> Expect.equal "(Pair.Ints 1 2)"

        --
        , test "Constructor: (Vec 0).Vec0" <|
            \_ ->
                dumpExpression (Constructor ( "Vec", [ Integer 0 ] ) "Vec0" [])
                    |> Expect.equal "(Vec 0).Vec0"

        --
        , test "Constructor: ((Vec 2 Int).Vec2 1 2)" <|
            \_ ->
                dumpExpression (Constructor ( "Vec", [ Integer 2, Type IntType ] ) "Vec2" [ Integer 1, Integer 2 ])
                    |> Expect.equal "((Vec 2 Int).Vec2 1 2)"

        -- Lambda
        , test "Lambda: (x : Int) -> 42" <|
            \_ ->
                dumpExpression (Lambda ( "x", IntType ) (Integer 42))
                    |> Expect.equal "(x:Int)->42"

        --
        , test "Lambda: (x : Int) -> (y : Number) -> 42" <|
            \_ ->
                dumpExpression (Lambda ( "x", IntType ) (Lambda ( "y", NumberType ) (Integer 42)))
                    |> Expect.equal "(x:Int)->(y:Number)->42"

        -- Input
        , test "Input: x with type Int" <|
            \_ ->
                dumpExpression (Input "x" IntType)
                    |> Expect.equal "(x:Int)"

        -- Call
        , test "Call: (f : Int -> Int) 1" <|
            \_ ->
                dumpExpression (Call "f" IntType IntType [ Integer 1 ])
                    |> Expect.equal "(f:Int->Int) 1"

        --
        , test "Call: (f : Int -> Int -> Int) 1 2" <|
            \_ ->
                dumpExpression
                    (Call "f"
                        IntType
                        (LambdaType IntType IntType)
                        [ Integer 1, Integer 2 ]
                    )
                    |> Expect.equal "(f:Int->Int->Int) 1 2"
        ]
