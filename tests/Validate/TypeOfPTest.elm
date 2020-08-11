module Validate.TypeOfPTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Type exposing (typeOfP)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOfP"
        -- AnyP
        [ describe "AnyP"
            [ test "_ : Int -- ok" <|
                \_ ->
                    typeOfP (AnyP IntT)
                        |> Expect.equal IntT

            --
            , test "_ : T -- ok" <|
                \_ ->
                    typeOfP (AnyP (NameT "T" []))
                        |> Expect.equal (NameT "T" [])
            ]

        -- NameP
        , describe "NameP"
            [ test "x : Int -- ok" <|
                \_ ->
                    typeOfP (NameP (AnyP IntT) "x")
                        |> Expect.equal IntT
            ]

        -- TypeP
        , describe "TypeP"
            [ test "Int -- ok" <|
                \_ ->
                    typeOfP (TypeP IntT)
                        |> Expect.equal TypeT
            ]

        -- IntP
        , describe "IntP"
            [ test "1 -- ok" <|
                \_ ->
                    typeOfP (IntP 1)
                        |> Expect.equal IntT
            ]

        -- NumberP
        , describe "NumberP"
            [ test "1.1 -- ok" <|
                \_ ->
                    typeOfP (NumberP 1.1)
                        |> Expect.equal NumberT
            ]

        -- TupleP
        , describe "TupleP"
            [ test "() -- ok" <|
                \_ ->
                    typeOfP (TupleP [])
                        |> Expect.equal (TupleT [])

            --
            , test "(_ : Int) -- ok" <|
                \_ ->
                    typeOfP (TupleP [ AnyP IntT ])
                        |> Expect.equal (TupleT [ IntT ])
            ]

        -- RecordP
        , describe "RecordP"
            [ test "{} -- ok" <|
                \_ ->
                    typeOfP (RecordP Dict.empty)
                        |> Expect.equal (RecordT Dict.empty)

            --
            , test "{a : Int} -- ok" <|
                \_ ->
                    typeOfP (RecordP (Dict.singleton "a" IntT))
                        |> Expect.equal (RecordT (Dict.singleton "a" IntT))
            ]

        -- ConstructorP
        , describe "ConstructorP"
            [ test "(T 1).A -- ok" <|
                \_ ->
                    typeOfP (ConstructorP ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (NameT "T" [ Int 1 ])
            ]
        ]
