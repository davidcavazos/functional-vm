module FVM.TypeOfPatternTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..), typeOfPattern)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOfPattern"
        -- AnyP
        [ describe "AnyP"
            [ test "_ : Int -- ok" <|
                \_ ->
                    typeOfPattern (AnyP IntT)
                        |> Expect.equal IntT

            --
            , test "_ : T -- ok" <|
                \_ ->
                    typeOfPattern (AnyP (NameT "T" []))
                        |> Expect.equal (NameT "T" [])
            ]

        -- NameP
        , describe "NameP"
            [ test "x : Int -- ok" <|
                \_ ->
                    typeOfPattern (NameP (AnyP IntT) "x")
                        |> Expect.equal IntT
            ]

        -- TypeP
        , describe "TypeP"
            [ test "Int -- ok" <|
                \_ ->
                    typeOfPattern (TypeP IntT)
                        |> Expect.equal TypeT
            ]

        -- IntP
        , describe "IntP"
            [ test "1 -- ok" <|
                \_ ->
                    typeOfPattern (IntP 1)
                        |> Expect.equal IntT
            ]

        -- NumberP
        , describe "NumberP"
            [ test "1.1 -- ok" <|
                \_ ->
                    typeOfPattern (NumberP 1.1)
                        |> Expect.equal NumberT
            ]

        -- TupleP
        , describe "TupleP"
            [ test "() -- ok" <|
                \_ ->
                    typeOfPattern (TupleP [])
                        |> Expect.equal (TupleT [])

            --
            , test "(_ : Int) -- ok" <|
                \_ ->
                    typeOfPattern (TupleP [ AnyP IntT ])
                        |> Expect.equal (TupleT [ IntT ])
            ]

        -- RecordP
        , describe "RecordP"
            [ test "{} -- ok" <|
                \_ ->
                    typeOfPattern (RecordP Dict.empty)
                        |> Expect.equal (RecordT Dict.empty)

            --
            , test "{a : Int} -- ok" <|
                \_ ->
                    typeOfPattern (RecordP (Dict.singleton "a" IntT))
                        |> Expect.equal (RecordT (Dict.singleton "a" IntT))
            ]

        -- ConstructorP
        , describe "ConstructorP"
            [ test "(T 1).A -- ok" <|
                \_ ->
                    typeOfPattern (ConstructorP ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (NameT "T" [ Int 1 ])
            ]
        ]
