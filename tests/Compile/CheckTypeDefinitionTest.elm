module Compile.CheckTypeDefinitionTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..), new)
import FVM.Compile exposing (checkTypeDefinition)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Compile.checkTypeDefinition"
        [ -- with type inputs
          test "type T -- ok" <|
            \_ ->
                FVM.new
                    |> checkTypeDefinition ( [], Dict.empty )
                    |> Expect.equal (Ok ( [], Dict.empty ))

        --
        , test "type T X -- TypeNotFound on type inputs" <|
            \_ ->
                FVM.new
                    |> checkTypeDefinition ( [ NameT "X" [] ], Dict.empty )
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "type T Int -- ok" <|
            \_ ->
                FVM.new
                    |> checkTypeDefinition ( [ IntT ], Dict.empty )
                    |> Expect.equal (Ok ( [ IntT ], Dict.empty ))

        -- with constructors
        , test "type T = A -- ok" <|
            \_ ->
                FVM.new
                    |> checkTypeDefinition ( [], Dict.fromList [ ( "A", [] ) ] )
                    |> Expect.equal (Ok ( [], Dict.fromList [ ( "A", [] ) ] ))

        --
        , test "type T = A X -- TypeNotFound on constructor input" <|
            \_ ->
                FVM.new
                    |> checkTypeDefinition ( [], Dict.fromList [ ( "A", [ NameT "X" [] ] ) ] )
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "type T = A Int -- ok" <|
            \_ ->
                FVM.new
                    |> checkTypeDefinition ( [], Dict.fromList [ ( "A", [ IntT ] ) ] )
                    |> Expect.equal (Ok ( [], Dict.fromList [ ( "A", [ IntT ] ) ] ))
        ]
