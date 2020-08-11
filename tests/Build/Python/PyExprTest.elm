module Build.Python.PyExprTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Build.Python exposing (pyExpr)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "build Python -- pyExpr"
        -- Type
        [ describe "Type"
            [ test "Int" <|
                \_ ->
                    pyExpr (Type IntT)
                        |> Expect.equal "Int"

            --
            , test "X" <|
                \_ ->
                    pyExpr (Type (NameT "X" []))
                        |> Expect.equal "X"
            ]

        -- Int
        , describe "Int"
            [ test "1" <|
                \_ ->
                    pyExpr (Int 1)
                        |> Expect.equal "1"

            --
            , test "-1" <|
                \_ ->
                    pyExpr (Int -1)
                        |> Expect.equal "-1"
            ]

        -- Number
        , describe "Number"
            [ test "1.1" <|
                \_ ->
                    pyExpr (Number 1.1)
                        |> Expect.equal "1.1"

            --
            , test "-1.1" <|
                \_ ->
                    pyExpr (Number -1.1)
                        |> Expect.equal "-1.1"
            ]

        -- Tuple
        , describe "Tuple"
            [ test "()" <|
                \_ ->
                    pyExpr (Tuple [])
                        |> Expect.equal "()"

            --
            , test "(1)" <|
                \_ ->
                    pyExpr (Tuple [ Int 1 ])
                        |> Expect.equal "(1)"

            --
            , test "(1, 2.2)" <|
                \_ ->
                    pyExpr (Tuple [ Int 1, Number 2.2 ])
                        |> Expect.equal "(1, 2.2)"
            ]

        -- Record
        , describe "Record"
            [ test "{}" <|
                \_ ->
                    pyExpr (Record Dict.empty)
                        |> Expect.equal "Record()"

            --
            , test "{x = 1}" <|
                \_ ->
                    pyExpr (Record (Dict.singleton "x" (Int 1)))
                        |> Expect.equal "Record(x=1)"

            --
            , test "{x = 1, y = 1.1}" <|
                \_ ->
                    pyExpr (Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Number 2.2 ) ]))
                        |> Expect.equal "Record(x=1, y=2.2)"
            ]

        -- Constructor
        , describe "Constructor"
            [ test "T.A" <|
                \_ ->
                    pyExpr (Constructor ( "T", [] ) "A" [])
                        |> Expect.equal "A()"

            --
            , test "T.A 1" <|
                \_ ->
                    pyExpr (Constructor ( "T", [] ) "A" [ Int 1 ])
                        |> Expect.equal "A(1)"

            --
            , test "T.A 1 2.2" <|
                \_ ->
                    pyExpr (Constructor ( "T", [] ) "A" [ Int 1, Number 2.2 ])
                        |> Expect.equal "A(1, 2.2)"
            ]

        -- Let
        , describe "Let"
            [ test "let x = 1; 2" <|
                \_ ->
                    pyExpr (Let ( "x", Int 1 ) (Int 2))
                        |> Expect.equal "x = 1\n2"

            --
            , test "let x = 1; let y = 2; 3" <|
                \_ ->
                    pyExpr (Let ( "x", Int 1 ) (Let ( "y", Int 2 ) (Int 3)))
                        |> Expect.equal "x = 1\ny = 2\n3"
            ]

        -- Load
        , describe "Load"
            [ test "x" <|
                \_ ->
                    pyExpr (Load "x")
                        |> Expect.equal "x"
            ]

        -- -- Lambda
        -- , describe "Lambda"
        --     [ test "(x : Int) -> 1" <|
        --         \_ ->
        --             pyExpr (Lambda ( "x", IntT ) (Int 1))
        --                 |> Expect.equal "lambda x: 1"
        --     ]
        ]
