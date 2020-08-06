module Validate.ExpandCaseTest exposing (suite)

import Dict
import Expect
import FVM exposing (Case(..), Error(..), Expression(..), Pattern(..), Type(..), new)
import FVM.Validate exposing (expandCase, withType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "expandCase"
        -- Validation
        [ describe "validation"
            [ test "check pattern -- _ : X on _ : Int -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> expandCase (AnyP (NameT "X" [])) (AnyC IntT)
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "check case -- _ : Int on _ : X -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> expandCase (AnyP IntT) (AnyC (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "type mismatch -- _ : Int on _ : Number -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (AnyP IntT) (AnyC NumberT)
                        |> Expect.equal (Ok [ AnyC NumberT ])
            ]

        -- AnyP
        , describe "AnyP"
            [ test "on any -- _ : Int on _ : Int -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (AnyP IntT) (AnyC IntT)
                        |> Expect.equal (Ok [])

            --
            , test "on tuple -- _ : (Int) on (_ : Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (AnyP (TupleT [ IntT ]))
                            (TupleC [ AnyC IntT ])
                        |> Expect.equal (Ok [])

            --
            , test "on constructor -- type T = A; _ : T on T.A -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> Result.andThen
                            (expandCase (AnyP (NameT "T" []))
                                (ConstructorC ( "T", [] ) "A" [])
                            )
                        |> Expect.equal (Ok [])
            ]

        -- NameP
        , describe "NameP"
            [ test "on any -- x : Int on _ : Int -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (NameP (AnyP IntT) "x") (AnyC IntT)
                        |> Expect.equal (Ok [])
            ]

        -- TypeP
        , describe "TypeP"
            [ test "on any -- Int on _ : Type -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TypeP IntT) (AnyC TypeT)
                        |> Expect.equal (Ok [ AnyC TypeT ])
            ]

        -- IntP
        , describe "IntP"
            [ test "on any -- 1 on _ : Int -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (IntP 1) (AnyC IntT)
                        |> Expect.equal (Ok [ AnyC IntT ])
            ]

        -- NumberP
        , describe "NumberP"
            [ test "on any -- 1.1 on _ : Number -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (NumberP 1.1) (AnyC NumberT)
                        |> Expect.equal (Ok [ AnyC NumberT ])
            ]

        -- TupleP on TupleC
        , describe "TupleP on TupleC"
            [ test "empty -- () on () -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP []) (TupleC [])
                        |> Expect.equal (Ok [])

            --
            , test "value -- (1) on (_ : Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP [ IntP 1 ])
                            (TupleC [ AnyC IntT ])
                        |> Expect.equal (Ok [ TupleC [ AnyC IntT ] ])

            --
            , test "any -- (_ : Int) on (_ : Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP [ AnyP IntT ])
                            (TupleC [ AnyC IntT ])
                        |> Expect.equal (Ok [])
            ]

        -- TupleP on AnyC
        , describe "TupleP on AnyC"
            [ test "empty -- () on _ : () -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP []) (AnyC (TupleT []))
                        |> Expect.equal (Ok [])

            --
            , test "value -- (1) on _ : (Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP [ IntP 1 ])
                            (AnyC (TupleT [ IntT ]))
                        |> Expect.equal (Ok [ TupleC [ AnyC IntT ] ])

            --
            , test "any -- (_ : Int) on _ : (Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP [ AnyP IntT ])
                            (AnyC (TupleT [ IntT ]))
                        |> Expect.equal (Ok [])

            --
            , test "recursive value -- ((1)) on _ : ((Int)) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP [ TupleP [ IntP 1 ] ])
                            (AnyC (TupleT [ TupleT [ IntT ] ]))
                        |> Expect.equal (Ok [ TupleC [ TupleC [ AnyC IntT ] ] ])

            --
            , test "recursive any -- (_ : (Int)) on _ : ((Int)) -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (TupleP [ AnyP (TupleT [ IntT ]) ])
                            (AnyC (TupleT [ TupleT [ IntT ] ]))
                        |> Expect.equal (Ok [])
            ]

        -- RecordP
        , describe "RecordP"
            [ test "perfect match -- {x : Int} on _ : {x : Int} -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (RecordP (Dict.fromList [ ( "x", IntT ) ]))
                            (AnyC (RecordT (Dict.fromList [ ( "x", IntT ) ])))
                        |> Expect.equal (Ok [])

            --
            , test "imperfect match -- {} on _ : {x : Int} -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (RecordP Dict.empty)
                            (AnyC (RecordT (Dict.fromList [ ( "x", IntT ) ])))
                        |> Expect.equal (Ok [ AnyC (RecordT (Dict.fromList [ ( "x", IntT ) ])) ])

            --
            , test "mismatch -- {x : Int} on _ : {y : Int} -- ok" <|
                \_ ->
                    FVM.new
                        |> expandCase (RecordP (Dict.fromList [ ( "x", IntT ) ]))
                            (AnyC (RecordT (Dict.fromList [ ( "y", IntT ) ])))
                        |> Expect.equal (Ok [ AnyC (RecordT (Dict.fromList [ ( "y", IntT ) ])) ])
            ]

        -- ConstructorP on ConstructorC
        , describe "ConstructorP on ConstructorC"
            [ test "mismatch -- type T = A | B; T.A on T.B -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [], [] ) )
                                , ( "B", ( [], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [])
                                (ConstructorC ( "T", [] ) "B" [])
                            )
                        |> Expect.equal (Ok [ ConstructorC ( "T", [] ) "B" [] ])

            --
            , test "without inputs -- type T = A | B; T.A on T.A -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [], [] ) )
                                , ( "B", ( [], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [])
                                (ConstructorC ( "T", [] ) "A" [])
                            )
                        |> Expect.equal (Ok [])

            --
            , test "with input value -- type T = A Int | B; T.A 1 on T.A (_ : Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [ ( "x", IntT ) ], [] ) )
                                , ( "B", ( [], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [ IntP 1 ])
                                (ConstructorC ( "T", [] ) "A" [ AnyC IntT ])
                            )
                        |> Expect.equal (Ok [ ConstructorC ( "T", [] ) "A" [ AnyC IntT ] ])

            --
            , test "with input any -- type T = A Int | B; T.A (_ : Int) on T.A (_ : Int) -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [ ( "x", IntT ) ], [] ) )
                                , ( "B", ( [], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [ AnyP IntT ])
                                (ConstructorC ( "T", [] ) "A" [ AnyC IntT ])
                            )
                        |> Expect.equal (Ok [])
            ]

        -- ConstructorP on AnyC
        , describe "ConstructorP on AnyC"
            [ test "without inputs -- type T = A | B; T.A on _ : T -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [], [] ) )
                                , ( "B", ( [], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [])
                                (AnyC (NameT "T" []))
                            )
                        |> Expect.equal (Ok [ ConstructorC ( "T", [] ) "B" [] ])

            --
            , test "with input value -- type T = A Int | B; T.A 1 on _ : T -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [ ( "x", IntT ) ], [] ) )
                                , ( "B", ( [ ( "x", IntT ) ], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [ IntP 1 ])
                                (AnyC (NameT "T" []))
                            )
                        |> Expect.equal
                            (Ok
                                [ ConstructorC ( "T", [] ) "A" [ AnyC IntT ]
                                , ConstructorC ( "T", [] ) "B" [ AnyC IntT ]
                                ]
                            )

            --
            , test "with input any -- type T = A Int | B; T.A (_ : Int) on _ : T -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [ ( "x", IntT ) ], [] ) )
                                , ( "B", ( [ ( "x", IntT ) ], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [ AnyP IntT ])
                                (AnyC (NameT "T" []))
                            )
                        |> Expect.equal (Ok [ ConstructorC ( "T", [] ) "B" [ AnyC IntT ] ])

            --
            , test "with input recursive value -- type T = A T | B; T.A T.B on _ : T -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [ ( "x", NameT "T" [] ) ], [] ) )
                                , ( "B", ( [], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [ ConstructorP ( "T", [] ) "B" [] ])
                                (AnyC (NameT "T" []))
                            )
                        |> Expect.equal
                            (Ok
                                [ ConstructorC ( "T", [] ) "A" [ ConstructorC ( "T", [] ) "A" [ AnyC (NameT "T" []) ] ]
                                , ConstructorC ( "T", [] ) "B" []
                                ]
                            )

            --
            , test "with input recursive any -- type T = A T | B T; T.A (_ : T) on _ : T -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] )
                            (Dict.fromList
                                [ ( "A", ( [ ( "x", NameT "T" [] ) ], [] ) )
                                , ( "B", ( [ ( "x", NameT "T" [] ) ], [] ) )
                                ]
                            )
                        |> Result.andThen
                            (expandCase (ConstructorP ( "T", [] ) "A" [ AnyP (NameT "T" []) ])
                                (AnyC (NameT "T" []))
                            )
                        |> Expect.equal (Ok [ ConstructorC ( "T", [] ) "B" [ AnyC (NameT "T" []) ] ])
            ]
        ]
