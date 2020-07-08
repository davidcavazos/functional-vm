module SaveTaggedUnionTypeTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..), new, number, saveName, saveTaggedUnionType, withResult)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "saveTaggedUnionType"
        [ test "after no result" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> dump
                    |> Expect.equal "T T"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withResult (Ok (Integer 1))
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> dump
                    |> Expect.equal "T T;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withResult (Err (NameNotFound "x"))
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "TypeAlreadyExists" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> dump
                    |> Expect.equal "T T;E TypeAlreadyExists T"

        --
        , test "many definitions" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T1", [] ) Dict.empty
                    |> saveTaggedUnionType ( "T2", [] ) Dict.empty
                    |> dump
                    |> Expect.equal "T T1;T T2"

        --
        , test "with type inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [ IntType, NumberType ] ) Dict.empty
                    |> dump
                    |> Expect.equal "T T Int Number"

        --
        , test "TypeInputsMismatch with constructor type inputs" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [ IntType ] )
                        (Dict.fromList
                            [ ( "A", ( [], [ number 3.14 ] ) ) ]
                        )
                    |> dump
                    |> Expect.equal "E TypeInputsMismatch T"

        --
        , test "NameAlreadyExists with constructor name" <|
            \_ ->
                new
                    |> saveName "A" (Integer 1)
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [ ( "x", IntType ) ], [] ) ) ]
                        )
                    |> dump
                    |> Expect.equal "V A=1;E NameAlreadyExists A"

        --
        , test "with constructors" <|
            \_ ->
                new
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [], [] ) )
                            , ( "B", ( [ ( "x", IntType ), ( "y", IntType ) ], [] ) )
                            ]
                        )
                    |> dump
                    |> Expect.equal "T T=A|B Int Int;V A=T.A;V B=(x:Int)->(y:Int)->(T.B (x:Int) (y:Int))"
        ]
