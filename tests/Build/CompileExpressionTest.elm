module Build.CompileExpressionTest exposing (suite)

import ASM
import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Build exposing (compileExpression)
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

        -- Tuple
        , describe "Tuple"
            [ test "tuple -- (1, 2.2)" <|
                \_ ->
                    compileExpression (Tuple [ Int 1, Number 2.2 ])
                        |> Expect.equal (ASM.Tuple [ ASM.Int 1, ASM.Number 2.2 ])
            ]

        -- Record
        , describe "Record"
            [ test "record -- (x = 1, y = 2.2)" <|
                \_ ->
                    compileExpression (Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Number 2.2 ) ]))
                        |> Expect.equal (ASM.Record (Dict.fromList [ ( "x", ASM.Int 1 ), ( "y", ASM.Number 2.2 ) ]))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "constructor -- (T 1 2.2).A 3 4.4" <|
                \_ ->
                    compileExpression
                        (Constructor ( "T", [ Int 1, Number 2.2 ] )
                            "A"
                            [ Int 3, Number 4.4 ]
                        )
                        |> Expect.equal (ASM.Constructor ( "T", [ ASM.Int 1, ASM.Number 2.2 ] ) "A" [ ASM.Int 3, ASM.Number 4.4 ])
            ]

        -- Let
        , describe "Let"
            [ test "let one variable -- let x = 1; 2" <|
                \_ ->
                    compileExpression (Let ( "x", Int 1 ) (Int 2))
                        |> Expect.equal (ASM.Let (Dict.singleton "x" (ASM.Int 1)) (ASM.Int 2))

            --
            , test "let many variables -- let x = 1; let y = 2; let z = 3; 4" <|
                \_ ->
                    compileExpression
                        (Let ( "x", Int 1 )
                            (Let ( "y", Int 2 )
                                (Let ( "z", Int 3 ) (Int 4))
                            )
                        )
                        |> Expect.equal (ASM.Let (Dict.fromList [ ( "x", ASM.Int 1 ), ( "y", ASM.Int 2 ), ( "z", ASM.Int 3 ) ]) (ASM.Int 4))
            ]

        -- Load
        , describe "Load"
            [ test "load -- x" <|
                \_ ->
                    compileExpression (Load "x" IntT)
                        |> Expect.equal (ASM.Load "x" ASM.IntT)
            ]

        -- Lambda
        , describe "Lambda"
            [ test "lambda with one input -- (x : Int) -> 1.1" <|
                \_ ->
                    compileExpression (Lambda ( "x", IntT ) (Number 1.1))
                        |> Expect.equal (ASM.Function (Dict.singleton "x" ASM.IntT) (ASM.Number 1.1))

            --
            , test "lambda with many inputs -- (x : Int) -> (y : Number) -> (z : Type) -> 1.1" <|
                \_ ->
                    compileExpression
                        (Lambda ( "x", IntT )
                            (Lambda ( "y", NumberT )
                                (Lambda ( "z", TypeT )
                                    (Number 1.1)
                                )
                            )
                        )
                        |> Expect.equal (ASM.Function (Dict.fromList [ ( "x", ASM.IntT ), ( "y", ASM.NumberT ), ( "z", ASM.TypeT ) ]) (ASM.Number 1.1))
            ]
        ]
