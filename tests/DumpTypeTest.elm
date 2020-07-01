module DumpTypeTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dumpType)
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "dumpType"
        [ -- TypeType
          test "TypeType" <|
            \_ ->
                dumpType TypeType
                    |> Expect.equal "Type"

        -- IntType
        , test "IntType" <|
            \_ ->
                dumpType IntType
                    |> Expect.equal "Int"

        -- NumberType
        , test "NumberType" <|
            \_ ->
                dumpType NumberType
                    |> Expect.equal "Number"

        -- TupleType
        , test "TupleType: ()" <|
            \_ ->
                dumpType (TupleType [])
                    |> Expect.equal "()"

        --
        , test "TupleType: (Int, Number)" <|
            \_ ->
                dumpType (TupleType [ IntType, NumberType ])
                    |> Expect.equal "(Int,Number)"

        -- RecordType
        , test "RecordType: ()" <|
            \_ ->
                dumpType (RecordType Dict.empty)
                    |> Expect.equal "()"

        --
        , test "RecordType: (a : Int, b : Number)" <|
            \_ ->
                dumpType (RecordType (Dict.fromList [ ( "a", IntType ), ( "b", NumberType ) ]))
                    |> Expect.equal "(a:Int,b:Number)"

        -- LambbdaType
        , test "LambdaType: Int -> Number" <|
            \_ ->
                dumpType (LambdaType IntType NumberType)
                    |> Expect.equal "Int->Number"

        --
        , test "LambdaType: Int -> Number -> Int" <|
            \_ ->
                dumpType (LambdaType IntType (LambdaType NumberType IntType))
                    |> Expect.equal "Int->Number->Int"

        --
        , test "LambdaType: (Int -> Number) -> Int" <|
            \_ ->
                dumpType (LambdaType (LambdaType IntType NumberType) IntType)
                    |> Expect.equal "(Int->Number)->Int"

        -- GenericType
        , test "GenericType: a" <|
            \_ ->
                dumpType (GenericType "a")
                    |> Expect.equal "a"

        -- NamedType
        , test "NamedType: Bool" <|
            \_ ->
                dumpType (NamedType "Bool" [])
                    |> Expect.equal "Bool"

        --
        , test "NamedType: Vector 3 Int" <|
            \_ ->
                dumpType (NamedType "Vector" [ Integer 3, Type IntType ])
                    |> Expect.equal "Vector 3 Int"
        ]
