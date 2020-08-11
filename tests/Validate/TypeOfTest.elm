module Validate.TypeOfTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Package exposing (letType)
import FVM.Type exposing (typeOf)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOf"
        -- Type
        [ describe "Type"
            [ test "Int" <|
                \_ ->
                    typeOf (Type IntT)
                        |> Expect.equal TypeT

            --
            , test "T" <|
                \_ ->
                    typeOf (Type (NameT "T" []))
                        |> Expect.equal TypeT
            ]

        -- Int
        , describe "Int"
            [ test "1" <|
                \_ ->
                    typeOf (Int 1)
                        |> Expect.equal IntT
            ]

        -- Number
        , describe "Number"
            [ test "1.1" <|
                \_ ->
                    typeOf (Number 1.1)
                        |> Expect.equal NumberT
            ]

        -- Tuple
        , describe "Tuple"
            [ test "()" <|
                \_ ->
                    typeOf (Tuple [])
                        |> Expect.equal (TupleT [])

            --
            , test "(1, 2.2)" <|
                \_ ->
                    typeOf (Tuple [ Int 1, Number 2.2 ])
                        |> Expect.equal (TupleT [ IntT, NumberT ])
            ]

        -- Record
        , describe "Record"
            [ test "{}" <|
                \_ ->
                    typeOf (Record Dict.empty)
                        |> Expect.equal (RecordT Dict.empty)

            --
            , test "(a = 1, b = 1.1)" <|
                \_ ->
                    typeOf (Record (Dict.fromList [ ( "a", Int 1 ), ( "b", Number 1.1 ) ]))
                        |> Expect.equal (RecordT (Dict.fromList [ ( "a", IntT ), ( "b", NumberT ) ]))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "(T 1).A" <|
                \_ ->
                    typeOf (Constructor ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (NameT "T" [ Int 1 ])
            ]

        -- Let
        , describe "Let"
            [ test "let x = 1; 2.2" <|
                \_ ->
                    typeOf (Let ( "x", Int 1 ) (Number 2.2))
                        |> Expect.equal NumberT
            ]

        -- Load
        , describe "Load"
            [ test "x" <|
                \_ ->
                    typeOf (Load "x" IntT)
                        |> Expect.equal IntT
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : Int) -> 1.1" <|
                \_ ->
                    typeOf (Lambda ( "x", IntT ) (Number 1.1))
                        |> Expect.equal (LambdaT IntT NumberT)
            ]

        -- Call
        , describe "Call"
            [ test "((x : Int) -> Type) 1" <|
                \_ ->
                    typeOf (Call (Lambda ( "x", IntT ) (Type TypeT)) (Number 1.1))
                        |> Expect.equal TypeT

            --
            , test "(x : Int) -> (y: Number) -> Type) 1 2.2" <|
                \_ ->
                    typeOf
                        (Call
                            (Call
                                (Lambda ( "x", IntT ) (Lambda ( "y", NumberT ) (Type TypeT)))
                                (Int 1)
                            )
                            (Number 2.2)
                        )
                        |> Expect.equal TypeT
            ]

        -- CaseOf
        , describe "CaseOf"
            [ test "match 1 to Number of _ -> 1.1" <|
                \_ ->
                    typeOf (CaseOf ( Int 1, NumberT ) [ ( AnyP IntT, Number 1.1 ) ])
                        |> Expect.equal NumberT
            ]
        ]
