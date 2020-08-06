module Validate.CheckTest exposing (suite)

import Dict
import Expect
import FVM exposing (Case(..), Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module exposing (withType)
import FVM.Validate exposing (check)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "check"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound -- checkT" <|
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
            , test "(X) -- TypeNotFound -- check on List" <|
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
            , test "{a = X} -- TypeNotFound -- check on Dict" <|
                \_ ->
                    FVM.Module.new
                        |> check (Record (Dict.singleton "a" (Type (NameT "X" []))))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "{a = 1} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Record (Dict.singleton "a" (Int 1)))
                        |> Expect.equal (Ok (Record (Dict.singleton "a" (Int 1))))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "type T; (T 1).A -- TypeInputsMismatch -- getTypeDefinition" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) Dict.empty
                        |> Result.andThen (check (Constructor ( "T", [ Int 1 ] ) "A" []))
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ IntT ], expected = [] }))

            --
            , test "type T; T.A -- ConstructorNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) Dict.empty
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Err (ConstructorNotFound ( "T", [] ) "A"))

            --
            , test "type T = A; T.A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.singleton "A" ( [], [] ))
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" []))
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" []))

            --
            , test "type T = A (x : Int); T.A 1.1 -- ConstructorInputsMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] ))
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" [ Number 1.1 ]))
                        |> Expect.equal (Err (ConstructorInputsMismatch ( "T", [] ) "A" { got = [ NumberT ], expected = [ IntT ] }))

            --
            , test "type T = A (x : Int); T.A 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] ))
                        |> Result.andThen (check (Constructor ( "T", [] ) "A" [ Int 1 ]))
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" [ Int 1 ]))
            ]

        -- Input
        , describe "Input"
            [ test "input X -- TypeNotFound -- checkT" <|
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

        -- Let
        , describe "Let"
            [ test "let x = X; 1 -- TypeNotFound -- check value" <|
                \_ ->
                    FVM.Module.new
                        |> check (Let ( "x", Type (NameT "X" []) ) (Int 1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "let x = 1; X -- TypeNotFound -- check output" <|
                \_ ->
                    FVM.Module.new
                        |> check (Let ( "x", Int 1 ) (Type (NameT "X" [])))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "let x = 1; 2 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Let ( "x", Int 1 ) (Int 2))
                        |> Expect.equal (Ok (Let ( "x", Int 1 ) (Int 2)))

            --
            , test "let x = 1; let x = 2; 3 -- NameAlreadyExists -- withName" <|
                \_ ->
                    FVM.Module.new
                        |> check (Let ( "x", Int 1 ) (Let ( "x", Int 2 ) (Int 3)))
                        |> Expect.equal (Err (NameAlreadyExists "x" { got = Int 2, existing = Int 1 }))
            ]

        -- Load
        , describe "Load"
            [ test "x -- NameNotFound -- getName" <|
                \_ ->
                    FVM.Module.new
                        |> check (Load "x")
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "let x = 1; x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check (Let ( "x", Int 1 ) (Load "x"))
                        |> Expect.equal (Ok (Let ( "x", Int 1 ) (Load "x")))
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : X) -> 1 -- TypeNotFound -- checkT on input type" <|
                \_ ->
                    FVM.Module.new
                        |> check (Lambda ( "x", NameT "X" [] ) (Int 1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> y -- NameNotFound -- check on output" <|
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
            [ test "x 1 -- NameNotFound -- typeOf function" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Load "x") (Int 1))
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "1 2 -- CallNonFunction -- typeOf function" <|
                \_ ->
                    FVM.Module.new
                        |> check (Call (Int 1) (Int 2))
                        |> Expect.equal (Err (CallNonFunction (Int 1) (Int 2)))

            --
            , test "(Int -> Number) 1.1 -- TypeMismatch -- typecheck on non-generic input" <|
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

            --
            , test "(a -> Number) 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (Call (Input (LambdaT (GenericT "a") NumberT))
                                (Int 1)
                            )
                        |> Expect.equal (Ok (Call (Input (LambdaT (GenericT "a") NumberT)) (Int 1)))

            --
            , test "(a -> a -> a -> Number) 1 2 3.3 -- TypeMismatch typecheck on generic input" <|
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
            [ test "case X -> Number of -- TypeNotFound -- typeOf on input" <|
                \_ ->
                    FVM.Module.new
                        |> check (CaseOf ( Type (NameT "X" []), NumberT ) [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "case 1 -> X of -- TypeNotFound -- checkT on output type" <|
                \_ ->
                    FVM.Module.new
                        |> check (CaseOf ( Int 1, NameT "X" [] ) [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "case 1 -> Number of 1.1 -> 2.2 -- TypeMismatch -- typecheckP on input" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( NumberP 1.1, Number 2.2 ) ]
                            )
                        |> Expect.equal (Err (PatternMismatch (NumberP 1.1) IntT))

            --
            , test "case 1 -> Int of x -> x -- ok -- withPattern" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, IntT )
                                [ ( NameP (AnyP IntT) "x", Load "x" ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Int 1, IntT ) [ ( NameP (AnyP IntT) "x", Load "x" ) ]))

            --
            , test "case 1 -> Number of 1 -> 2 -- TypeMismatch -- typecheck on output" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( IntP 1, Int 2 ) ]
                            )
                        |> Expect.equal (Err (TypeMismatch (Int 2) NumberT))

            --
            , test "case 1 -> Number of 1 -> 1.1 -- CasesMissing -- on non-record" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Int 1, NumberT )
                                [ ( IntP 1, Number 1.1 ) ]
                            )
                        |> Expect.equal (Err (CasesMissing ( Int 1, NumberT ) [ AnyC IntT ]))

            --
            , test "case 1 -> Int of _ -> x -- NameNotFound -- checkCase" <|
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
            , test "case 1 -> Number of 1 -> 1.1; 1 -> 2.2 -- CaseAlreadyCovered -- isCaseCovered" <|
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
            , test "case 1 -> Number of _ -> 1.1; 1 -> 2.2 -- CaseAlreadyCovered -- isCaseCovered" <|
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

            -- CaseOf Tuple
            , test "case (1, 2) -> Number of (3, 4) -> 1.1 -- CasesMissing -- on non-record with combinations" <|
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

            -- CaseOf Record
            , test "case {} -> Int of -- MissingCases -- on records" <|
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
                                [ ( RecordP (Dict.singleton "x" IntT), Int 1 ) ]
                            )
                        |> Expect.equal (Err (PatternMismatch (RecordP (Dict.singleton "x" IntT)) (RecordT Dict.empty)))

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
                            (CaseOf ( Record (Dict.singleton "x" (Int 1)), IntT )
                                [ ( RecordP Dict.empty, Int 2 ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Record (Dict.singleton "x" (Int 1)), IntT ) [ ( RecordP Dict.empty, Int 2 ) ]))

            --
            , test "case {x = 1, y = 2} -> Int of {x : Int} -> 3 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> check
                            (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]), IntT )
                                [ ( RecordP (Dict.singleton "x" IntT), Int 3 ) ]
                            )
                        |> Expect.equal (Ok (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]), IntT ) [ ( RecordP (Dict.singleton "x" IntT), Int 3 ) ]))

            -- CaseOf Constructor
            , test "type T = A; case T.A -> Number of A -> 1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.singleton "A" ( [], [] ))
                        |> Result.andThen
                            (check
                                (CaseOf ( Constructor ( "T", [] ) "A" [], NumberT )
                                    [ ( ConstructorP ( "T", [] ) "A" [], Number 1.1 ) ]
                                )
                            )
                        |> Expect.equal (Ok (CaseOf ( Constructor ( "T", [] ) "A" [], NumberT ) [ ( ConstructorP ( "T", [] ) "A" [], Number 1.1 ) ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1 -- CasesMissing -- with many constructors" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ), ( "B", ( [ ( "b", IntT ) ], [] ) ) ])
                        |> Result.andThen
                            (check
                                (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                                    [ ( ConstructorP ( "T", [] ) "A" [], Int 1 ) ]
                                )
                            )
                        |> Expect.equal (Err (CasesMissing ( Constructor ( "T", [] ) "A" [], IntT ) [ ConstructorC ( "T", [] ) "B" [ AnyC IntT ] ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1; B 2 -> 3 -- CasesMissing with many constructors" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ), ( "B", ( [ ( "b", IntT ) ], [] ) ) ])
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
                        |> withType ( "T", [] )
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
