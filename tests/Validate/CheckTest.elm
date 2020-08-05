module Validate.CheckTest exposing (suite)

import Dict
import Expect
import FVM exposing (Case(..), Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module exposing (addName, addType)
import FVM.Validate exposing (check)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "check"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check (Type (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Type IntT)
                        |> Expect.equal (Ok (Type IntT))
            ]

        -- Int
        , describe "Int"
            [ test "1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Int 1)
                        |> Expect.equal (Ok (Int 1))
            ]

        -- Number
        , describe "Number"
            [ test "1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Number 1.1)
                        |> Expect.equal (Ok (Number 1.1))
            ]

        -- Tuple
        , describe "Tuple"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Tuple [])
                        |> Expect.equal (Ok (Tuple []))

            --
            , test "(X) -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check (Tuple [ Type (NameT "X" []) ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(1) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Tuple [ Int 1 ])
                        |> Expect.equal (Ok (Tuple [ Int 1 ]))
            ]

        -- Record
        , describe "Record"
            [ test "{} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Record Dict.empty)
                        |> Expect.equal (Ok (Record Dict.empty))

            --
            , test "{a = X} -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check (Record (Dict.fromList [ ( "a", Type (NameT "X" []) ) ]))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "{a = 1} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Record (Dict.fromList [ ( "a", Int 1 ) ]))
                        |> Expect.equal (Ok (Record (Dict.fromList [ ( "a", Int 1 ) ])))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "X.A -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check (Constructor ( "X", [] ) "A" [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "type T; (T 1).A -- TypeInputsMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) Dict.empty
                        |> Result.andThen (check (Constructor ( "T", [ Int 1 ] ) "A" []))
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ IntT ], expected = [] }))

            --
            , test "type T; T.A -- ConstructorNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) Dict.empty
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Err (ConstructorNotFound "T" "A"))

            --
            , test "type T = A; T.A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" []))

            --
            , test "type T = A; T.A 1 -- ConstructorInputsMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" [ Int 1 ]))
                        |> Expect.equal (Err (ConstructorInputsMismatch "T" "A" { got = [ IntT ], expected = [] }))

            --
            , test "type T = A (x : Int); T.A 1.1 -- ConstructorInputsMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [ ( "x", IntT ) ], [] ) ) ])
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" [ Number 1.1 ]))
                        |> Expect.equal (Err (ConstructorInputsMismatch "T" "A" { got = [ NumberT ], expected = [ IntT ] }))

            --
            , test "type T = A (x : Int); T.A 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [ ( "x", IntT ) ], [] ) ) ])
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" [ Int 1 ]))
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" [ Int 1 ]))
            ]

        -- Input
        , describe "Input"
            [ test "input X -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check (Input (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "input Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Input IntT)
                        |> Expect.equal (Ok (Input IntT))
            ]

        -- Load
        , describe "Load"
            [ test "x -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check (Load "x")
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "let x = 1; x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addName "x" (Int 1)
                        |> Result.andThen (check (Load "x"))
                        |> Expect.equal (Ok (Load "x"))

            --
            , test "input x : Int; x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addName "x" (Input IntT)
                        |> Result.andThen (check (Load "x"))
                        |> Expect.equal (Ok (Load "x"))
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : X) -> 1 -- TypeNotFound on input type" <|
                \_ ->
                    FVM.Module.new
                        |> check (Lambda ( "x", NameT "X" [] ) (Int 1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> y -- NameNotFound on output" <|
                \_ ->
                    FVM.Module.new
                        |> check (Lambda ( "x", IntT ) (Load "y"))
                        |> Expect.equal (Err (NameNotFound "y"))

            --
            , test "(x : Int) -> x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Lambda ( "x", IntT ) (Load "x"))
                        |> Expect.equal (Ok (Lambda ( "x", IntT ) (Load "x")))
            ]

        -- Call
        , describe "Call"
            [ test "x 1 -- NameNotFound on function" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Load "x") (Int 1))
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "1 2 -- CallNonFunction" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Int 1) (Int 2))
                        |> Expect.equal (Err (CallNonFunction (Int 1) (Int 2)))

            -- Call Lambda
            , test "((x : Int) -> x) y -- NameNotFound on input" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Lambda ( "x", IntT ) (Load "x")) (Load "y"))
                        |> Expect.equal (Err (NameNotFound "y"))

            --
            , test "((x : Int) -> x) 1.1 -- TypeMismatch on input" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Lambda ( "x", IntT ) (Load "x")) (Number 1.1))
                        |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))

            --
            , test "((x : Int) -> x) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Lambda ( "x", IntT ) (Load "x")) (Int 1))
                        |> Expect.equal (Ok (Call (Lambda ( "x", IntT ) (Load "x")) (Int 1)))

            -- Call Input
            , test "(Int) 1 -- CallNonFunction" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Input IntT) (Int 1))
                        |> Expect.equal (Err (CallNonFunction (Input IntT) (Int 1)))

            --
            , test "(Int -> Number) x -- NameNotFound on input" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Input (LambdaT IntT NumberT)) (Load "x"))
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "(Int -> Number) 1.1 -- TypeMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Input (LambdaT IntT NumberT)) (Number 1.1))
                        |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))

            --
            , test "(Int -> Number) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Input (LambdaT IntT NumberT)) (Int 1))
                        |> Expect.equal (Ok (Call (Input (LambdaT IntT NumberT)) (Int 1)))

            -- Call with generic types
            , test "(a -> Number) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (Call (Input (LambdaT (GenericT "a") NumberT))
                                (Int 1)
                            )
                        |> Expect.equal (Ok (Call (Input (LambdaT (GenericT "a") NumberT)) (Int 1)))

            --
            , test "(a -> a -> Number) 1 2.2 -- TypeMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (Call
                                (Call (Input (LambdaT (GenericT "a") (LambdaT (GenericT "a") NumberT)))
                                    (Int 1)
                                )
                                (Number 2.2)
                            )
                        |> Expect.equal (Err (TypeMismatch (Number 2.2) IntT))

            --
            , test "(a -> a -> a -> Number) 1 2 3.3 -- TypeMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (Call
                                (Call
                                    (Call (Input (LambdaT (GenericT "a") (LambdaT (GenericT "a") (LambdaT (GenericT "a") NumberT))))
                                        (Int 1)
                                    )
                                    (Int 2)
                                )
                                (Number 3.3)
                            )
                        |> Expect.equal (Err (TypeMismatch (Number 3.3) IntT))

            --
            , test "(a -> a -> a -> Number) 1 2 3 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (Call
                                (Call
                                    (Call (Input (LambdaT (GenericT "a") (LambdaT (GenericT "a") (LambdaT (GenericT "a") NumberT))))
                                        (Int 1)
                                    )
                                    (Int 2)
                                )
                                (Int 3)
                            )
                        |> Expect.equal (Ok (Call (Call (Call (Input (LambdaT (GenericT "a") (LambdaT (GenericT "a") (LambdaT (GenericT "a") NumberT)))) (Int 1)) (Int 2)) (Int 3)))
            ]

        -- CaseOf
        , describe "CaseOf"
            [ -- Validation
              test "case X -> Number of -- TypeNotFound on input expression" <|
                \_ ->
                    FVM.Module.new
                        |> check (CaseOf ( Type (NameT "X" []), NumberT ) [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "case 1 -> X of -- TypeNotFound on output type" <|
                \_ ->
                    FVM.Module.new
                        |> check (CaseOf ( Int 1, NameT "X" [] ) [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "case 1 -> Number of 1.1 -> 2.2 -- TypeMismatch on input" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( NumberP 1.1, Number 2.2 ) ]
                            )
                        |> Expect.equal (Err (PatternMismatch (NumberP 1.1) IntT))

            --
            , test "case 1 -> Number of 1 -> 2 -- TypeMismatch on output" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( IntP 1, Int 2 ) ]
                            )
                        |> Expect.equal (Err (TypeMismatch (Int 2) NumberT))

            --
            , test "case 1 -> Number of 1 -> 1.1 -- CasesMissing" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( IntP 1, Number 1.1 ) ]
                            )
                        |> Expect.equal (Err (CasesMissing ( Int 1, NumberT ) [ AnyC IntT ]))

            --
            , test "case 1 -> Int of _ -> x -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, IntT )
                                [ ( AnyP IntT, Load "x" ) ]
                            )
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "case 1 -> Int of x -> x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, IntT )
                                [ ( NameP (AnyP IntT) "x", Load "x" ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Int 1, IntT ) [ ( NameP (AnyP IntT) "x", Load "x" ) ]))

            --
            , test "case 1 -> Number of 1 -> 1.1; 1 -> 2.2 -- CaseAlreadyCovered" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( IntP 1, Number 1.1 )
                                , ( IntP 1, Number 2.2 )
                                ]
                            )
                        |> Expect.equal (Err (CaseAlreadyCovered ( Int 1, NumberT ) ( IntP 1, Number 2.2 )))

            --
            , test "case 1 -> Number of _ -> 1.1; 1 -> 2.2 -- CaseAlreadyCovered" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( AnyP IntT, Number 1.1 )
                                , ( IntP 1, Number 2.2 )
                                ]
                            )
                        |> Expect.equal (Err (CaseAlreadyCovered ( Int 1, NumberT ) ( IntP 1, Number 2.2 )))

            --
            , test "case 1 -> Number of 1 -> 1.1; _ -> 2.2 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( IntP 1, Number 1.1 )
                                , ( AnyP IntT, Number 2.2 )
                                ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Int 1, NumberT ) [ ( IntP 1, Number 1.1 ), ( AnyP IntT, Number 2.2 ) ]))

            -- Tuples
            , test "case (1, 2) -> Number of (3, 4) -> 1.1 -- CasesMissing" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Tuple [ Int 1, Int 2 ], NumberT )
                                [ ( TupleP [ IntP 3, IntP 4 ], Number 1.1 ) ]
                            )
                        |> Expect.equal (Err (CasesMissing ( Tuple [ Int 1, Int 2 ], NumberT ) [ TupleC [ AnyC IntT, AnyC IntT ] ]))

            --
            , test "case (1, 2.2) -> Number of (x, y) -> y -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Tuple [ Int 1, Number 2.2 ], NumberT )
                                [ ( TupleP [ NameP (AnyP IntT) "x", NameP (AnyP NumberT) "y" ], Load "y" ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Tuple [ Int 1, Number 2.2 ], NumberT ) [ ( TupleP [ NameP (AnyP IntT) "x", NameP (AnyP NumberT) "y" ], Load "y" ) ]))

            -- Records
            , test "case {} -> Int of -- MissingCases" <|
                \_ ->
                    FVM.Module.new
                        |> check (CaseOf ( Record Dict.empty, IntT ) [])
                        |> Expect.equal (Err (CasesMissing ( Record Dict.empty, IntT ) [ AnyC (RecordT Dict.empty) ]))

            --
            , test "case {} -> Int of {x : Int} -> 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Record Dict.empty, IntT )
                                [ ( RecordP (Dict.fromList [ ( "x", IntT ) ]), Int 1 ) ]
                            )
                        |> Expect.equal (Err (PatternMismatch (RecordP (Dict.fromList [ ( "x", IntT ) ])) (RecordT Dict.empty)))

            --
            , test "case {} -> Int of {} -> 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Record Dict.empty, IntT )
                                [ ( RecordP Dict.empty, Int 1 ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Record Dict.empty, IntT ) [ ( RecordP Dict.empty, Int 1 ) ]))

            --
            , test "case {x = 1} -> Int of {} -> 2 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ) ]), IntT )
                                [ ( RecordP Dict.empty, Int 2 ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ) ]), IntT ) [ ( RecordP Dict.empty, Int 2 ) ]))

            --
            , test "case {x = 1, y = 2} -> Int of {x : Int} -> 3 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]), IntT )
                                [ ( RecordP (Dict.fromList [ ( "x", IntT ) ]), Int 3 ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]), IntT ) [ ( RecordP (Dict.fromList [ ( "x", IntT ) ]), Int 3 ) ]))

            -- Constructors
            , test "type T = A; case T.A -> Number of A -> 1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> Result.andThen
                            (check
                                (CaseOf ( Constructor ( "T", [] ) "A" [], NumberT )
                                    [ ( ConstructorP ( "T", [] ) "A" [], Number 1.1 ) ]
                                )
                            )
                        |> Expect.equal (Ok (CaseOf ( Constructor ( "T", [] ) "A" [], NumberT ) [ ( ConstructorP ( "T", [] ) "A" [], Number 1.1 ) ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1 -- CasesMissing" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ), ( "B", ( [ ( "b", IntT ) ], [] ) ) ])
                        |> Result.andThen
                            (check
                                (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                                    [ ( ConstructorP ( "T", [] ) "A" [], Int 1 ) ]
                                )
                            )
                        |> Expect.equal (Err (CasesMissing ( Constructor ( "T", [] ) "A" [], IntT ) [ ConstructorC ( "T", [] ) "B" [ AnyC IntT ] ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1; B 2 -> 3 -- CasesMissing" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ), ( "B", ( [ ( "b", IntT ) ], [] ) ) ])
                        |> Result.andThen
                            (check
                                (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                                    [ ( ConstructorP ( "T", [] ) "A" [], Int 1 )
                                    , ( ConstructorP ( "T", [] ) "B" [ IntP 2 ], Int 3 )
                                    ]
                                )
                            )
                        |> Expect.equal (Err (CasesMissing ( Constructor ( "T", [] ) "A" [], IntT ) [ ConstructorC ( "T", [] ) "B" [ AnyC IntT ] ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1; B x -> x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [], [] ) )
                                , ( "B", ( [ ( "b", IntT ) ], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (check
                                (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                                    [ ( ConstructorP ( "T", [] ) "A" [], Int 1 )
                                    , ( ConstructorP ( "T", [] ) "B" [ NameP (AnyP IntT) "x" ], Load "x" )
                                    ]
                                )
                            )
                        |> Expect.equal (Ok (CaseOf ( Constructor ( "T", [] ) "A" [], IntT ) [ ( ConstructorP ( "T", [] ) "A" [], Int 1 ), ( ConstructorP ( "T", [] ) "B" [ NameP (AnyP IntT) "x" ], Load "x" ) ]))
            ]
        ]
