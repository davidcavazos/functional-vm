module ConstructorTest exposing (suite)

import Dict
import Expect
import FVM exposing (Type(..), constructor, int, new, number, saveTaggedUnionType)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "constructor"
        [ test "TypeNotFound" <|
            \_ ->
                new
                    |> constructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "E TypeNotFound T"

        -- TODO: check for a constructor for a non-TaggedUnionType, like a TypeAlias
        --
        , test "ConstructorNotFound" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> constructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T;E ConstructorNotFound T A"

        --
        , test "TypeInputsMismatch" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [], [] ) ) ]
                        )
                    |> constructor ( "T", [ int 1 ] ) "A" []
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;E TypeInputsMismatch T"

        --
        , test "ConstructorInputsMismatch too many inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [], [] ) ) ]
                        )
                    |> constructor ( "T", [] ) "A" [ int 1 ]
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;E ConstructorInputsMismatch T A"

        --
        , test "0 inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [], [] ) ) ]
                        )
                    |> constructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;R T.A"

        --
        , test "TypeMismatch on constructor inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [ ( "x", IntType ) ], [] ) ) ]
                        )
                    |> constructor ( "T", [] ) "A" [ number 3.14 ]
                    |> dump
                    |> Expect.equal "T T=A Int;V A=(x:Int)->(T.A (x:Int));E TypeMismatch 3.14 Int"

        --
        , test "1 input" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [ ( "x", IntType ) ], [] ) ) ]
                        )
                    |> constructor ( "T", [] ) "A" [ int 1 ]
                    |> dump
                    |> Expect.equal "T T=A Int;V A=(x:Int)->(T.A (x:Int));R (T.A 1)"
        ]
