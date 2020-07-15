module Type.CheckTypeTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Context exposing (bindGeneric)
import FVM.Module exposing (addType)
import FVM.Type exposing (checkType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Type.checkType"
        [ -- TypeType
          test "Type -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType TypeType FVM.Context.new
                    |> Expect.equal (Ok TypeType)

        -- IntType
        , test "Int -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType IntType FVM.Context.new
                    |> Expect.equal (Ok IntType)

        -- NumberType
        , test "Number -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType NumberType FVM.Context.new
                    |> Expect.equal (Ok NumberType)

        -- NamedType
        , test "X -- TypeNotFound" <|
            \_ ->
                FVM.Module.new
                    |> checkType (NamedType ( "X", [] )) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "type T; T -- ok" <|
            \_ ->
                FVM.Module.new
                    |> addType ( "T", [] ) Dict.empty
                    |> checkType (NamedType ( "T", [] )) FVM.Context.new
                    |> Expect.equal (Ok (NamedType ( "T", [] )))

        --
        , test "type T Int; T -- TypeInputsMismatch" <|
            \_ ->
                FVM.Module.new
                    |> addType ( "T", [ IntType ] ) Dict.empty
                    |> checkType (NamedType ( "T", [] )) FVM.Context.new
                    |> Expect.equal (Err (TypeInputsMismatch "T" { got = [], expected = [ IntType ] }))

        --
        , test "type T; T 1 -- TypeInputsMismatch" <|
            \_ ->
                FVM.Module.new
                    |> addType ( "T", [] ) Dict.empty
                    |> checkType (NamedType ( "T", [ Integer 1 ] )) FVM.Context.new
                    |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ Integer 1 ], expected = [] }))

        --
        , test "type T Int; T 3.14 -- TypeMismatch" <|
            \_ ->
                FVM.Module.new
                    |> addType ( "T", [ IntType ] ) Dict.empty
                    |> checkType (NamedType ( "T", [ Number 3.14 ] )) FVM.Context.new
                    |> Expect.equal (Err (TypeMismatch (Number 3.14) IntType))

        --
        , test "type T Int; T 1 -- ok" <|
            \_ ->
                FVM.Module.new
                    |> addType ( "T", [ IntType ] ) Dict.empty
                    |> checkType (NamedType ( "T", [ Integer 1 ] )) FVM.Context.new
                    |> Expect.equal (Ok (NamedType ( "T", [ Integer 1 ] )))

        -- LambdaType
        , test "X -> Y -- TypeNotFound on input type" <|
            \_ ->
                FVM.Module.new
                    |> checkType (LambdaType (NamedType ( "X", [] )) (NamedType ( "Y", [] ))) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "Int -> X -- TypeNotFound on output type" <|
            \_ ->
                FVM.Module.new
                    |> checkType (LambdaType IntType (NamedType ( "X", [] ))) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "Int -> Number -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (LambdaType IntType NumberType) FVM.Context.new
                    |> Expect.equal (Ok (LambdaType IntType NumberType))

        -- TupleType
        , test "() -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (TupleType []) FVM.Context.new
                    |> Expect.equal (Ok (TupleType []))

        --
        , test "(X) -- TypeNotFound" <|
            \_ ->
                FVM.Module.new
                    |> checkType (TupleType [ NamedType ( "X", [] ) ]) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "(Int, Number) -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (TupleType [ IntType, NumberType ]) FVM.Context.new
                    |> Expect.equal (Ok (TupleType [ IntType, NumberType ]))

        -- RecordType
        , test "{} -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (RecordType Dict.empty) FVM.Context.new
                    |> Expect.equal (Ok (RecordType Dict.empty))

        --
        , test "{x : X} -- TypeNotFound" <|
            \_ ->
                FVM.Module.new
                    |> checkType (RecordType (Dict.fromList [ ( "x", NamedType ( "X", [] ) ) ])) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "{x : Int, y : Number} -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (RecordType (Dict.fromList [ ( "x", IntType ), ( "y", NumberType ) ])) FVM.Context.new
                    |> Expect.equal (Ok (RecordType (Dict.fromList [ ( "x", IntType ), ( "y", NumberType ) ])))

        -- GenericType
        , test "a -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (GenericType "a") FVM.Context.new
                    |> Expect.equal (Ok (GenericType "a"))

        --
        , test "with generic a = Int; a -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (GenericType "a") (bindGeneric "a" IntType FVM.Context.new)
                    |> Expect.equal (Ok IntType)

        -- UnionType
        , test "empty -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (UnionType []) FVM.Context.new
                    |> Expect.equal (Ok (UnionType []))

        --
        , test "Int | X -- TypeNotFound" <|
            \_ ->
                FVM.Module.new
                    |> checkType (UnionType [ IntType, NamedType ( "X", [] ) ]) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "Int | Number -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkType (UnionType [ IntType, NumberType ]) FVM.Context.new
                    |> Expect.equal (Ok (UnionType [ IntType, NumberType ]))
        ]
