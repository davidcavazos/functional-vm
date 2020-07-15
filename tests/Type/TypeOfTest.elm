module Type.TypeOfTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Context
import FVM.Module exposing (addType)
import FVM.Type exposing (typeOf)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        xType =
            NamedType ( "X", [] )
    in
    describe "Type.typeOf"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type xType) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type IntType) FVM.Context.new
                        |> Expect.equal (Ok TypeType)
            ]

        -- Integer
        , describe "Integer"
            [ test "1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Integer 1) FVM.Context.new
                        |> Expect.equal (Ok IntType)
            ]

        -- Number
        , describe "Number"
            [ test "3.14 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Number 3.14) FVM.Context.new
                        |> Expect.equal (Ok NumberType)
            ]

        -- Variable
        , describe "Variable"
            [ test "(x : X) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Variable "x" xType) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Variable "x" IntType) FVM.Context.new
                        |> Expect.equal (Ok IntType)
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : X) -> 3.14 -- TypeNotFound on input type" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", xType ) (Integer 1)) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> (y : X) -- TypeNotFound on output type" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", IntType ) (Variable "y" xType)) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> 3.14 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", IntType ) (Number 3.14)) FVM.Context.new
                        |> Expect.equal (Ok (LambdaType IntType NumberType))
            ]

        -- Tuple
        , describe "Tuple"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple []) FVM.Context.new
                        |> Expect.equal (Ok (TupleType []))

            --
            , test "(1, (x : X)) -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple [ Integer 1, Variable "x" xType ]) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(1, 2) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple [ Integer 1, Integer 2 ]) FVM.Context.new
                        |> Expect.equal (Ok (TupleType [ IntType, IntType ]))
            ]

        -- Record
        , describe "Record"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record Dict.empty) FVM.Context.new
                        |> Expect.equal (Ok (RecordType Dict.empty))

            --
            , test "(a = 1, b = (x : X)) -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record (Dict.fromList [ ( "a", Integer 1 ), ( "b", Variable "x" xType ) ])) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(a = 1, b = 3.14) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record (Dict.fromList [ ( "a", Integer 1 ), ( "b", Number 3.14 ) ])) FVM.Context.new
                        |> Expect.equal (Ok (RecordType (Dict.fromList [ ( "a", IntType ), ( "b", NumberType ) ])))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "T.A -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Constructor ( "T", [] ) "A" []) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "T"))

            --
            , test "type T Int; T.A -- TypeInputsMismatch on type inputs" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [ IntType ] ) Dict.empty
                        |> typeOf (Constructor ( "T", [] ) "A" []) FVM.Context.new
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [], expected = [ IntType ] }))

            --
            , test "type T; T.A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) Dict.empty
                        |> typeOf (Constructor ( "T", [] ) "A" []) FVM.Context.new
                        |> Expect.equal (Ok (NamedType ( "T", [] )))
            ]

        -- Call
        , describe "Call"
            [ test "(x : Int) 1 -- CallNonFunction" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Variable "x" IntType) (Integer 1)) FVM.Context.new
                        |> Expect.equal (Err (CallNonFunction (Variable "x" IntType) (Integer 1)))

            --
            , test "(f : X -> Number) 3.14 -- TypeNotFound on input" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Variable "f" (LambdaType xType NumberType)) (Number 3.14)) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(f : Int -> X) 3.14 -- TypeNotFound on output" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Variable "f" (LambdaType IntType xType)) (Number 3.14)) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(f : Int -> Number) 3.14 -- TypeMismatch on input" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Variable "f" (LambdaType IntType NumberType)) (Number 3.14)) FVM.Context.new
                        |> Expect.equal (Err (TypeMismatch (Number 3.14) IntType))

            --
            , test "(f : Int -> Number) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Variable "f" (LambdaType IntType NumberType)) (Integer 1)) FVM.Context.new
                        |> Expect.equal (Ok NumberType)

            --
            , test "(f : Int -> Number -> Type) 1 3.14 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf
                            (Call
                                (Call
                                    (Variable "f" (LambdaType IntType (LambdaType NumberType TypeType)))
                                    (Integer 1)
                                )
                                (Number 3.14)
                            )
                            FVM.Context.new
                        |> Expect.equal (Ok TypeType)
            ]
        ]
