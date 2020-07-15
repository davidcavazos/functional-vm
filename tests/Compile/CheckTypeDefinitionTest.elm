module Compile.CheckTypeDefinitionTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Compile exposing (checkTypeDefinition)
import FVM.Context
import FVM.Module
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Compile.checkTypeDefinition"
        [ -- with type inputs
          test "type T -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkTypeDefinition ( [], Dict.empty ) FVM.Context.new
                    |> Expect.equal (Ok ( [], Dict.empty ))

        --
        , test "type T X -- TypeNotFound on type inputs" <|
            \_ ->
                FVM.Module.new
                    |> checkTypeDefinition ( [ NamedType ( "X", [] ) ], Dict.empty ) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "type T Int -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkTypeDefinition ( [ IntType ], Dict.empty ) FVM.Context.new
                    |> Expect.equal (Ok ( [ IntType ], Dict.empty ))

        -- with constructors
        , test "type T = A -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkTypeDefinition ( [], Dict.fromList [ ( "A", [] ) ] ) FVM.Context.new
                    |> Expect.equal (Ok ( [], Dict.fromList [ ( "A", [] ) ] ))

        --
        , test "type T = A X -- TypeNotFound on constructor input" <|
            \_ ->
                FVM.Module.new
                    |> checkTypeDefinition ( [], Dict.fromList [ ( "A", [ NamedType ( "X", [] ) ] ) ] ) FVM.Context.new
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "type T = A Int -- ok" <|
            \_ ->
                FVM.Module.new
                    |> checkTypeDefinition ( [], Dict.fromList [ ( "A", [ IntType ] ) ] ) FVM.Context.new
                    |> Expect.equal (Ok ( [], Dict.fromList [ ( "A", [ IntType ] ) ] ))
        ]
