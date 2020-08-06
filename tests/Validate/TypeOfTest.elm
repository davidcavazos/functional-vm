module Validate.TypeOfTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module exposing (addName, addType)
import FVM.Validate exposing (typeOf)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOf"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
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
            , test "(1, x) -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Tuple [ Int 1, Load "x" ])
                        |> Expect.equal (Err (NameNotFound "x"))

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
            , test "(a = 1, b = x) -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record (Dict.fromList [ ( "a", Int 1 ), ( "b", Load "x" ) ]))
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "(a = 1, b = 1.1) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Record (Dict.fromList [ ( "a", Int 1 ), ( "b", Number 1.1 ) ]))
                        |> Expect.equal (Ok (RecordT (Dict.fromList [ ( "a", IntT ), ( "b", NumberT ) ])))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "T.A -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Constructor ( "T", [] ) "A" [])
                        |> Expect.equal (Err (TypeNotFound "T"))

            --
            , test "type T Int; T.A -- TypeInputsMismatch on type inputs" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [ IntT ] ) Dict.empty
                        |> Result.andThen (typeOf (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [], expected = [ IntT ] }))

            --
            , test "type T; T.A -- ConstructorNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) Dict.empty
                        |> Result.andThen (typeOf (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Err (ConstructorNotFound "T" "A"))

            --
            , test "type T = A; T.A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> Result.andThen (typeOf (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Ok (NameT "T" []))

            --
            , test "type T Int = A; (T 1).A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [ IntT ] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> Result.andThen (typeOf (Constructor ( "T", [ Int 1 ] ) "A" []))
                        |> Expect.equal (Ok (NameT "T" [ Int 1 ]))
            ]

        -- Input
        , describe "Input"
            [ test "X -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Input (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Input IntT)
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
                        |> addName "x" (Int 1)
                        |> Result.andThen (typeOf (Load "x"))
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

        -- Let
        , describe "Let"
            [ test "let x = 1; x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Let (Dict.singleton "x" (Int 1)) (Load "x"))
                        |> Expect.equal (Ok IntT)
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

        -- -- CaseOf
        -- , describe "CaseOf"
        --     [ test "match 1 to X of -- TypeNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (CaseOf ( Int 1, (NameT "X" []) ) [])
        --                 |> Expect.equal (Err (TypeNotFound "X"))
        --     --
        --     , test "case 1 -> Number of -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (CaseOf ( Int 1, NumberT ) [])
        --                 |> Expect.equal (Ok NumberT)
        --     --
        --     , test "match 1 to Number of 1.1 -> 1.1 -- PatternMismatch" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf
        --                     (CaseOf ( Int 1, NumberT )
        --                         [ ( NumberPattern 1.1, Number 1.1 ) ]
        --                     )
        --                 |> Expect.equal (Err (PatternMismatch (NumberPattern 1.1) IntT))
        --     --
        --     , test "match 1 to Number of 1 -> x -- NameNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf
        --                     (CaseOf ( Int 1, NumberT )
        --                         [ ( IntPattern 1, Load "x" ) ]
        --                     )
        --                 |> Expect.equal (Err (NameNotFound "x"))
        --     ]
        ]
