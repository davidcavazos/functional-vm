module Validate.TypeOfTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module exposing (withType)
import FVM.Validate exposing (typeOf)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOf"
        -- Validation
        [ describe "validation"
            [ test "X -- TypeNotFound -- check expression" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))
            ]

        -- Type
        , describe "Type"
            [ test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type IntT)
                        |> Expect.equal (Ok TypeT)
            ]

        -- Int
        , describe "Int"
            [ test "1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Int 1)
                        |> Expect.equal (Ok IntT)
            ]

        -- Number
        , describe "Number"
            [ test "1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Number 1.1)
                        |> Expect.equal (Ok NumberT)
            ]

        -- Tuple
        , describe "Tuple"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple [])
                        |> Expect.equal (Ok (TupleT []))

            --
            , test "(X) -- TypeNotFound -- typeOf on List" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple [ Type (NameT "X" []) ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(1, 2) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple [ Int 1, Int 2 ])
                        |> Expect.equal (Ok (TupleT [ IntT, IntT ]))
            ]

        -- Record
        , describe "Record"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record Dict.empty)
                        |> Expect.equal (Ok (RecordT Dict.empty))

            --
            , test "(a = X) -- TypeNotFound -- typeOf on Dict" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record (Dict.singleton "a" (Type (NameT "X" []))))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(a = 1, b = 1.1) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record (Dict.fromList [ ( "a", Int 1 ), ( "b", Number 1.1 ) ]))
                        |> Expect.equal (Ok (RecordT (Dict.fromList [ ( "a", IntT ), ( "b", NumberT ) ])))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "type T Int = A; (T 1).A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [ IntT ] ) (Dict.singleton "A" ( [], [] ))
                        |> typeOf (Constructor ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (Ok (NameT "T" [ Int 1 ]))
            ]

        -- Input
        , describe "Input"
            [ test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Input IntT)
                        |> Expect.equal (Ok IntT)
            ]

        -- Let
        , describe "Let"
            [ test "let x = 1; 2 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Let ( "x", Int 1 ) (Int 2))
                        |> Expect.equal (Ok IntT)
            ]

        -- Load
        , describe "Load"
            [ test "x -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Load "x")
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "let x = 1; x-- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Let ( "x", Int 1 ) (Load "x"))
                        |> Expect.equal (Ok IntT)
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : X) -> 1.1 -- TypeNotFound on input type" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", NameT "X" [] ) (Int 1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> y -- NameNotFound on output type" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", IntT ) (Load "y"))
                        |> Expect.equal (Err (NameNotFound "y"))

            --
            , test "(x : Int) -> 1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", IntT ) (Number 1.1))
                        |> Expect.equal (Ok (LambdaT IntT NumberT))

            --
            , test "(x : Int) -> x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Lambda ( "x", IntT ) (Load "x"))
                        |> Expect.equal (Ok (LambdaT IntT IntT))
            ]

        -- Call
        , describe "Call"
            [ test "1 2 -- CallNonFunction" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Int 1) (Int 2))
                        |> Expect.equal (Err (CallNonFunction (Int 1) (Int 2)))

            --
            , test "(X -> Number) 1.1 -- TypeNotFound on input" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Input (LambdaT (NameT "X" []) NumberT)) (Number 1.1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(Int -> X) 1.1 -- TypeNotFound on output" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Input (LambdaT IntT (NameT "X" []))) (Number 1.1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(Int -> Number) 1.1 -- TypeMismatch on input" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Input (LambdaT IntT NumberT)) (Number 1.1))
                        |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))

            --
            , test "(Int -> Number) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Call (Input (LambdaT IntT NumberT)) (Int 1))
                        |> Expect.equal (Ok NumberT)

            --
            , test "(Int -> Number -> Type) 1 1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf
                            (Call
                                (Call
                                    (Input (LambdaT IntT (LambdaT NumberT TypeT)))
                                    (Int 1)
                                )
                                (Number 1.1)
                            )
                        |> Expect.equal (Ok TypeT)

            -- Call with generic types
            , test "(a -> Number) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf
                            (Call (Input (LambdaT (GenericT "a") NumberT))
                                (Int 1)
                            )
                        |> Expect.equal (Ok NumberT)

            --
            , test "(a -> a -> Number) 1 2.2 -- TypeMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf
                            (Call
                                (Call (Input (LambdaT (GenericT "a") (LambdaT (GenericT "a") NumberT)))
                                    (Int 1)
                                )
                                (Number 2.2)
                            )
                        |> Expect.equal (Err (TypeMismatch (Number 2.2) IntT))

            --
            , test "(a -> a -> a -> Number) 1 2 3 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf
                            (Call
                                (Call
                                    (Call (Input (LambdaT (GenericT "a") (LambdaT (GenericT "a") (LambdaT (GenericT "a") NumberT))))
                                        (Int 1)
                                    )
                                    (Int 2)
                                )
                                (Int 3)
                            )
                        |> Expect.equal (Ok NumberT)
            ]

        -- CaseOf
        , describe "CaseOf"
            [ test "match 1 to Number of _ -> 1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (CaseOf ( Int 1, NumberT ) [ ( AnyP IntT, Number 1.1 ) ])
                        |> Expect.equal (Ok NumberT)
            ]
        ]
