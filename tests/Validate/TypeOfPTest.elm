module Validate.TypeOfPTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module exposing (withType)
import FVM.Validate exposing (typeOfP)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOfP"
        -- Validation
        [ describe "validation"
            [ test "_ : X -- TypeNotFound -- checkP pattern" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (AnyP (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))
            ]

        -- AnyP
        , describe "AnyP"
            [ test "_ : Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (AnyP IntT)
                        |> Expect.equal (Ok IntT)
            ]

        -- NameP
        , describe "NameP"
            [ test "x : Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (NameP (AnyP IntT) "x")
                        |> Expect.equal (Ok IntT)
            ]

        -- TypeP
        , describe "TypeP"
            [ test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (TypeP IntT)
                        |> Expect.equal (Ok TypeT)
            ]

        -- IntP
        , describe "IntP"
            [ test "1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (IntP 1)
                        |> Expect.equal (Ok IntT)
            ]

        -- NumberP
        , describe "NumberP"
            [ test "1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (NumberP 1.1)
                        |> Expect.equal (Ok NumberT)
            ]

        -- TupleP
        , describe "TupleP"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (TupleP [])
                        |> Expect.equal (Ok (TupleT []))

            --
            , test "(_ : X) -- TypeNotFound -- typeOfP on List" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (TupleP [ AnyP (NameT "X" []) ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(_ : Int) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (TupleP [ AnyP IntT ])
                        |> Expect.equal (Ok (TupleT [ IntT ]))
            ]

        -- RecordP
        , describe "RecordP"
            [ test "{} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (RecordP Dict.empty)
                        |> Expect.equal (Ok (RecordT Dict.empty))

            --
            , test "{a : Int} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOfP (RecordP (Dict.singleton "a" IntT))
                        |> Expect.equal (Ok (RecordT (Dict.singleton "a" IntT)))
            ]

        -- ConstructorP
        , describe "ConstructorP"
            [ test "type T Int = A; (T 1).A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [ IntT ] ) (Dict.singleton "A" ( [], [] ))
                        |> Result.andThen (typeOfP (ConstructorP ( "T", [ Int 1 ] ) "A" []))
                        |> Expect.equal (Ok (NameT "T" [ Int 1 ]))
            ]
        ]
