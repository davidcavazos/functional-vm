module Validate.CheckTest exposing (suite)

import Dict
import Expect
import FVM exposing (Case(..), Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Package exposing (letType)
import FVM.Validate exposing (check)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "check"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound -- checkT" <|
                \_ ->
                    check FVM.Package.new (Type (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    check FVM.Package.new (Type IntT)
                        |> Expect.equal (Ok (Type IntT))
            ]

        -- Int
        , describe "Int"
            [ test "1 -- ok" <|
                \_ ->
                    check FVM.Package.new (Int 1)
                        |> Expect.equal (Ok (Int 1))
            ]

        -- Number
        , describe "Number"
            [ test "1.1 -- ok" <|
                \_ ->
                    check FVM.Package.new (Number 1.1)
                        |> Expect.equal (Ok (Number 1.1))
            ]

        -- Tuple
        , describe "Tuple"
            [ test "() -- ok" <|
                \_ ->
                    check FVM.Package.new (Tuple [])
                        |> Expect.equal (Ok (Tuple []))

            --
            , test "(X) -- TypeNotFound -- check FVM.Package.new on List" <|
                \_ ->
                    check FVM.Package.new (Tuple [ Type (NameT "X" []) ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(1) -- ok" <|
                \_ ->
                    check FVM.Package.new (Tuple [ Int 1 ])
                        |> Expect.equal (Ok (Tuple [ Int 1 ]))
            ]

        -- Record
        , describe "Record"
            [ test "{} -- ok" <|
                \_ ->
                    check FVM.Package.new (Record Dict.empty)
                        |> Expect.equal (Ok (Record Dict.empty))

            --
            , test "{a = X} -- TypeNotFound -- check FVM.Package.new on Dict" <|
                \_ ->
                    check FVM.Package.new (Record (Dict.singleton "a" (Type (NameT "X" []))))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "{a = 1} -- ok" <|
                \_ ->
                    check FVM.Package.new (Record (Dict.singleton "a" (Int 1)))
                        |> Expect.equal (Ok (Record (Dict.singleton "a" (Int 1))))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "type T; (T 1).A -- TypeInputsMismatch -- getTypeDefinition" <|
                \_ ->
                    check (letType ( "T", [] ) Dict.empty FVM.Package.new)
                        (Constructor ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ IntT ], expected = [] }))

            --
            , test "type T; T.A -- ConstructorNotFound" <|
                \_ ->
                    check (letType ( "T", [] ) Dict.empty FVM.Package.new)
                        (Constructor ( "T", [] ) "A" [])
                        |> Expect.equal (Err (ConstructorNotFound ( "T", [] ) "A"))

            --
            , test "type T = A; T.A -- ok" <|
                \_ ->
                    check (letType ( "T", [] ) (Dict.singleton "A" ( [], [] )) FVM.Package.new)
                        (Constructor ( "T", [] ) "A" [])
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" []))

            --
            , test "type T = A (x : Int); T.A 1.1 -- ConstructorInputsMismatch" <|
                \_ ->
                    check (letType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] )) FVM.Package.new)
                        (Constructor ( "T", [] ) "A" [ Number 1.1 ])
                        |> Expect.equal (Err (ConstructorInputsMismatch ( "T", [] ) "A" { got = [ NumberT ], expected = [ IntT ] }))

            --
            , test "type T = A (x : Int); T.A 1 -- ok" <|
                \_ ->
                    check (letType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] )) FVM.Package.new)
                        (Constructor ( "T", [] ) "A" [ Int 1 ])
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" [ Int 1 ]))
            ]

        -- Let
        , describe "Let"
            [ test "let x = X; 1 -- TypeNotFound -- check FVM.Package.new value" <|
                \_ ->
                    check FVM.Package.new (Let ( "x", Type (NameT "X" []) ) (Int 1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "let x = 1; X -- TypeNotFound -- check FVM.Package.new output" <|
                \_ ->
                    check FVM.Package.new (Let ( "x", Int 1 ) (Type (NameT "X" [])))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "let x = 1; 2 -- ok" <|
                \_ ->
                    check FVM.Package.new (Let ( "x", Int 1 ) (Int 2))
                        |> Expect.equal (Ok (Let ( "x", Int 1 ) (Int 2)))

            --
            , test "let x = 1; let x = 2; 3 -- NameAlreadyExists -- withName" <|
                \_ ->
                    check FVM.Package.new (Let ( "x", Int 1 ) (Let ( "x", Int 2 ) (Int 3)))
                        |> Expect.equal (Err (NameAlreadyExists "x" { got = Int 2, existing = Int 1 }))
            ]

        -- Load
        , describe "Load"
            [ test "x -- NameNotFound -- getName" <|
                \_ ->
                    check FVM.Package.new (Load "x" IntT)
                        |> Expect.equal (Err (NameNotFound "x"))

            -- TODO: typecheck
            --
            , test "let x = 1; x -- ok" <|
                \_ ->
                    check FVM.Package.new (Let ( "x", Int 1 ) (Load "x" IntT))
                        |> Expect.equal (Ok (Let ( "x", Int 1 ) (Load "x" IntT)))
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : X) -> 1 -- TypeNotFound -- checkT on input type" <|
                \_ ->
                    check FVM.Package.new (Lambda ( "x", NameT "X" [] ) (Int 1))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> y -- NameNotFound -- check FVM.Package.new on output" <|
                \_ ->
                    check FVM.Package.new (Lambda ( "x", IntT ) (Load "y" IntT))
                        |> Expect.equal (Err (NameNotFound "y"))

            --
            , test "(x : Int) -> x -- ok" <|
                \_ ->
                    check FVM.Package.new (Lambda ( "x", IntT ) (Load "x" IntT))
                        |> Expect.equal (Ok (Lambda ( "x", IntT ) (Load "x" IntT)))
            ]

        -- Call
        , describe "Call"
            [ test "x 1 -- NameNotFound -- typeOf function" <|
                \_ ->
                    check FVM.Package.new (Call (Load "x" IntT) (Int 1))
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "1 2 -- CallNonFunction -- typeOf function" <|
                \_ ->
                    check FVM.Package.new (Call (Int 1) (Int 2))
                        |> Expect.equal (Err (CallNonFunction (Int 1) (Int 2)))

            --
            , test "((x : Int) -> 0.0) 1.1 -- TypeMismatch -- typecheck FVM.Package.new on input" <|
                \_ ->
                    check FVM.Package.new (Call (Lambda ( "x", IntT ) (Number 0.0)) (Number 1.1))
                        |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))

            --
            , test "((x : Int) -> 0.0) 1 -- ok" <|
                \_ ->
                    check FVM.Package.new (Call (Lambda ( "x", IntT ) (Number 0.0)) (Int 1))
                        |> Expect.equal (Ok (Call (Lambda ( "x", IntT ) (Number 0.0)) (Int 1)))
            ]

        -- Call generics
        , describe "Call generics"
            [ test "((x : a) -> 0.0) 1 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (Call (Lambda ( "x", GenericT "a" ) (Number 0.0))
                            (Int 1)
                        )
                        |> Expect.equal (Ok (Call (Lambda ( "x", GenericT "a" ) (Number 0.0)) (Int 1)))

            --
            , test "((x : a) -> (y : a) -> (z : a) -> 0.0); f 1 2 3.3 -- TypeMismatch typecheck FVM.Package.new on generic input" <|
                \_ ->
                    check FVM.Package.new
                        (Call
                            (Call
                                (Call
                                    (Lambda ( "x", GenericT "a" )
                                        (Lambda ( "y", GenericT "a" )
                                            (Lambda ( "z", GenericT "a" ) (Number 0.0))
                                        )
                                    )
                                    (Int 1)
                                )
                                (Int 2)
                            )
                            (Number 3.3)
                        )
                        |> Expect.equal (Err (TypeMismatch (Number 3.3) IntT))

            --
            , test "((x: a) -> (y : a) -> (z : a) -> 0.0); f 1 2 3 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (Call
                            (Call
                                (Call
                                    (Lambda ( "x", GenericT "a" )
                                        (Lambda ( "y", GenericT "a" )
                                            (Lambda ( "z", GenericT "a" ) (Number 0.0))
                                        )
                                    )
                                    (Int 1)
                                )
                                (Int 2)
                            )
                            (Int 3)
                        )
                        |> Expect.equal (Ok (Call (Call (Call (Lambda ( "x", GenericT "a" ) (Lambda ( "y", GenericT "a" ) (Lambda ( "z", GenericT "a" ) (Number 0)))) (Int 1)) (Int 2)) (Int 3)))
            ]

        -- CaseOf
        , describe "CaseOf"
            [ test "case X -> Number of -- TypeNotFound -- typeOf on input" <|
                \_ ->
                    check FVM.Package.new (CaseOf ( Type (NameT "X" []), NumberT ) [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "case 1 -> X of -- TypeNotFound -- checkT on output type" <|
                \_ ->
                    check FVM.Package.new (CaseOf ( Int 1, NameT "X" [] ) [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "case 1 -> Number of 1.1 -> 2.2 -- TypeMismatch -- typecheckP on input" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, NumberT )
                            [ ( NumberP 1.1, Number 2.2 ) ]
                        )
                        |> Expect.equal (Err (PatternMismatch (NumberP 1.1) IntT))

            --
            , test "case 1 -> Int of x -> x -- ok -- withPattern" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, IntT )
                            [ ( NameP (AnyP IntT) "x", Load "x" IntT ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Int 1, IntT ) [ ( NameP (AnyP IntT) "x", Load "x" IntT ) ]))

            --
            , test "case 1 -> Number of 1 -> 2 -- TypeMismatch -- typecheck FVM.Package.new on output" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, NumberT )
                            [ ( IntP 1, Int 2 ) ]
                        )
                        |> Expect.equal (Err (TypeMismatch (Int 2) NumberT))

            --
            , test "case 1 -> Number of 1 -> 1.1 -- CasesMissing -- on non-record" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, NumberT )
                            [ ( IntP 1, Number 1.1 ) ]
                        )
                        |> Expect.equal (Err (CasesMissing ( Int 1, NumberT ) [ AnyC IntT ]))

            --
            , test "case 1 -> Int of _ -> x -- NameNotFound -- checkCase output" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, IntT )
                            [ ( AnyP IntT, Load "x" IntT ) ]
                        )
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "case 1 -> Int of x -> x -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, IntT )
                            [ ( NameP (AnyP IntT) "x", Load "x" IntT ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Int 1, IntT ) [ ( NameP (AnyP IntT) "x", Load "x" IntT ) ]))

            --
            , test "case 1 -> Number of 1 -> 1.1; 1 -> 2.2 -- CaseAlreadyCovered -- isCaseCovered" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, NumberT )
                            [ ( IntP 1, Number 1.1 )
                            , ( IntP 1, Number 2.2 )
                            ]
                        )
                        |> Expect.equal (Err (CaseAlreadyCovered ( Int 1, NumberT ) ( IntP 1, Number 2.2 )))

            --
            , test "case 1 -> Number of _ -> 1.1; 1 -> 2.2 -- CaseAlreadyCovered -- isCaseCovered" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, NumberT )
                            [ ( AnyP IntT, Number 1.1 )
                            , ( IntP 1, Number 2.2 )
                            ]
                        )
                        |> Expect.equal (Err (CaseAlreadyCovered ( Int 1, NumberT ) ( IntP 1, Number 2.2 )))

            --
            , test "case 1 -> Number of 1 -> 1.1; _ -> 2.2 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Int 1, NumberT )
                            [ ( IntP 1, Number 1.1 )
                            , ( AnyP IntT, Number 2.2 )
                            ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Int 1, NumberT ) [ ( IntP 1, Number 1.1 ), ( AnyP IntT, Number 2.2 ) ]))

            -- CaseOf Tuple
            , test "case (1, 2) -> Number of (3, 4) -> 1.1 -- CasesMissing -- on non-record with combinations" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Tuple [ Int 1, Int 2 ], NumberT )
                            [ ( TupleP [ IntP 3, IntP 4 ], Number 1.1 ) ]
                        )
                        |> Expect.equal (Err (CasesMissing ( Tuple [ Int 1, Int 2 ], NumberT ) [ TupleC [ AnyC IntT, AnyC IntT ] ]))

            --
            , test "case (1, 2.2) -> Number of (x, y) -> y -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Tuple [ Int 1, Number 2.2 ], NumberT )
                            [ ( TupleP [ NameP (AnyP IntT) "x", NameP (AnyP NumberT) "y" ], Load "y" NumberT ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Tuple [ Int 1, Number 2.2 ], NumberT ) [ ( TupleP [ NameP (AnyP IntT) "x", NameP (AnyP NumberT) "y" ], Load "y" NumberT ) ]))

            -- CaseOf Record
            , test "case {} -> Int of -- MissingCases -- on records" <|
                \_ ->
                    check FVM.Package.new (CaseOf ( Record Dict.empty, IntT ) [])
                        |> Expect.equal (Err (CasesMissing ( Record Dict.empty, IntT ) [ AnyC (RecordT Dict.empty) ]))

            --
            , test "case {} -> Int of {x : Int} -> 1 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Record Dict.empty, IntT )
                            [ ( RecordP (Dict.singleton "x" IntT), Int 1 ) ]
                        )
                        |> Expect.equal (Err (PatternMismatch (RecordP (Dict.singleton "x" IntT)) (RecordT Dict.empty)))

            --
            , test "case {} -> Int of {} -> 1 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Record Dict.empty, IntT )
                            [ ( RecordP Dict.empty, Int 1 ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Record Dict.empty, IntT ) [ ( RecordP Dict.empty, Int 1 ) ]))

            --
            , test "case {x = 1} -> Int of {} -> 2 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Record (Dict.singleton "x" (Int 1)), IntT )
                            [ ( RecordP Dict.empty, Int 2 ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Record (Dict.singleton "x" (Int 1)), IntT ) [ ( RecordP Dict.empty, Int 2 ) ]))

            --
            , test "case {x = 1, y = 2} -> Int of {x : Int} -> 3 -- ok" <|
                \_ ->
                    check FVM.Package.new
                        (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]), IntT )
                            [ ( RecordP (Dict.singleton "x" IntT), Int 3 ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]), IntT ) [ ( RecordP (Dict.singleton "x" IntT), Int 3 ) ]))

            -- CaseOf Constructor
            , test "type T = A; case T.A -> Number of A -> 1.1 -- ok" <|
                \_ ->
                    check (letType ( "T", [] ) (Dict.singleton "A" ( [], [] )) FVM.Package.new)
                        (CaseOf ( Constructor ( "T", [] ) "A" [], NumberT )
                            [ ( ConstructorP ( "T", [] ) "A" [], Number 1.1 ) ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Constructor ( "T", [] ) "A" [], NumberT ) [ ( ConstructorP ( "T", [] ) "A" [], Number 1.1 ) ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1 -- CasesMissing -- with many constructors" <|
                \_ ->
                    check (letType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ), ( "B", ( [ ( "b", IntT ) ], [] ) ) ]) FVM.Package.new)
                        (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                            [ ( ConstructorP ( "T", [] ) "A" [], Int 1 ) ]
                        )
                        |> Expect.equal (Err (CasesMissing ( Constructor ( "T", [] ) "A" [], IntT ) [ ConstructorC ( "T", [] ) "B" [ AnyC IntT ] ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1; B 2 -> 3 -- CasesMissing with many constructors" <|
                \_ ->
                    check (letType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ), ( "B", ( [ ( "b", IntT ) ], [] ) ) ]) FVM.Package.new)
                        (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                            [ ( ConstructorP ( "T", [] ) "A" [], Int 1 )
                            , ( ConstructorP ( "T", [] ) "B" [ IntP 2 ], Int 3 )
                            ]
                        )
                        |> Expect.equal (Err (CasesMissing ( Constructor ( "T", [] ) "A" [], IntT ) [ ConstructorC ( "T", [] ) "B" [ AnyC IntT ] ]))

            --
            , test "type T = A | B (b : Int); case T.A -> Int of A -> 1; B x -> x -- ok" <|
                \_ ->
                    check
                        (letType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [], [] ) )
                                , ( "B", ( [ ( "b", IntT ) ], [] ) )
                                ]
                            )
                            FVM.Package.new
                        )
                        (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                            [ ( ConstructorP ( "T", [] ) "A" [], Int 1 )
                            , ( ConstructorP ( "T", [] ) "B" [ NameP (AnyP IntT) "x" ], Load "x" IntT )
                            ]
                        )
                        |> Expect.equal (Ok (CaseOf ( Constructor ( "T", [] ) "A" [], IntT ) [ ( ConstructorP ( "T", [] ) "A" [], Int 1 ), ( ConstructorP ( "T", [] ) "B" [ NameP (AnyP IntT) "x" ], Load "x" IntT ) ]))
            ]
        ]
