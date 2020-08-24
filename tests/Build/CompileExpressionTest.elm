module Build.CompileExpressionTest exposing (suite)

import ASM
import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Build exposing (compileExpression)
import FVM.Package exposing (letName)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "compileExpression"
        -- Type
        [ describe "Type"
            [ test "Int" <|
                \_ ->
                    compileExpression (Type IntT)
                        |> Expect.equal (ASM.Type ASM.IntT)
            ]

        -- Int
        , describe "Int"
            [ test "int value -- 1" <|
                \_ ->
                    compileExpression (Int 1)
                        |> Expect.equal (ASM.Int 1)
            ]

        -- Number
        , describe "Number"
            [ test "number value -- 1.1" <|
                \_ ->
                    compileExpression (Number 1.1)
                        |> Expect.equal (ASM.Number 1.1)
            ]
        ]
