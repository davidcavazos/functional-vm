module TypecheckTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Type(..), new, saveTaggedUnionType, typecheck)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typecheck"
        [ -- Static types
          test "static types with different types" <|
            \_ ->
                new
                    |> typecheck IntType (Number 3.14)
                    |> dump
                    |> Expect.equal "E TypeMismatch 3.14 Int"

        --
        , test "static types with same types" <|
            \_ ->
                new
                    |> typecheck IntType (Integer 1)
                    |> dump
                    |> Expect.equal ""

        -- GenericType
        , test "unbound generic type" <|
            \_ ->
                new
                    |> typecheck (GenericType "a") (Integer 1)
                    |> dump
                    |> Expect.equal "G a=Int"

        --
        , test "bound generic with different types" <|
            \_ ->
                new
                    |> typecheck (GenericType "a") (Integer 1)
                    |> typecheck (GenericType "a") (Number 3.14)
                    |> dump
                    |> Expect.equal "G a=Int;E TypeMismatch 3.14 Int"

        --
        , test "bound generic with same types" <|
            \_ ->
                new
                    |> typecheck (GenericType "a") (Integer 1)
                    |> typecheck (GenericType "a") (Integer 2)
                    |> dump
                    |> Expect.equal "G a=Int"

        -- NamedType
        , test "TypeMismatch with different type names" <|
            \_ ->
                new
                    |> typecheck (NamedType "T1" []) (Constructor ( "T2", [] ) "A" [])
                    |> dump
                    |> Expect.equal "E TypeMismatch T2.A T1"

        --
        , test "TypeMismatch with different type inputs" <|
            \_ ->
                new
                    |> typecheck (NamedType "T" [ Integer 1 ]) (Constructor ( "T", [ Integer 2 ] ) "A" [])
                    |> dump
                    |> Expect.equal "E TypeMismatch (T 2).A T 1"

        --
        , test "TypeNotFound" <|
            \_ ->
                new
                    |> typecheck (NamedType "T" []) (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "E TypeNotFound T"

        --
        , test "TypeInputsMismatch with too many type inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> typecheck (NamedType "T" [ Integer 1 ]) (Constructor ( "T", [ Integer 1 ] ) "A" [])
                    |> dump
                    |> Expect.equal "T T;E TypeInputsMismatch T"

        --
        , test "TypeInputsMismatch with too few type inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [ IntType ] ) Dict.empty
                    |> typecheck (NamedType "T" []) (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "T T Int;E TypeInputsMismatch T"

        --
        , test "TypeInputsMismatch with type mismatch" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [ IntType ] ) Dict.empty
                    |> typecheck (NamedType "T" [ Number 3.14 ]) (Constructor ( "T", [ Number 3.14 ] ) "A" [])
                    |> dump
                    |> Expect.equal "T T Int;E TypeInputsMismatch T"

        --
        , test "with type inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [ IntType ] ) Dict.empty
                    |> typecheck (NamedType "T" [ Integer 1 ]) (Constructor ( "T", [ Integer 1 ] ) "A" [])
                    |> dump
                    |> Expect.equal "T T Int"
        ]
