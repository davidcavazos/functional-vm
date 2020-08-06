module Validate.CheckPTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module exposing (withType)
import FVM.Validate exposing (checkP)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "checkP"
        -- AnyP
        [ describe "AnyP"
            [ test "_ : X -- TypeNotFound -- checkT" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (AnyP (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "_ : Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (AnyP IntT)
                        |> Expect.equal (Ok (AnyP IntT))
            ]

        -- NameP
        , describe "NameP"
            [ test "X as x -- TypeNotFound -- checkP" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (NameP (AnyP (NameT "X" [])) "x")
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int as x -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (NameP (AnyP IntT) "x")
                        |> Expect.equal (Ok (NameP (AnyP IntT) "x"))
            ]

        -- TypeP
        , describe "TypeP"
            [ test "X -- TypeNotFound -- checkT" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (TypeP (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (TypeP IntT)
                        |> Expect.equal (Ok (TypeP IntT))
            ]

        -- IntP
        , describe "IntP"
            [ test "1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (IntP 1)
                        |> Expect.equal (Ok (IntP 1))
            ]

        -- NumberP
        , describe "NumberP"
            [ test "1.1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (NumberP 1.1)
                        |> Expect.equal (Ok (NumberP 1.1))
            ]

        -- TupleP
        , describe "TupleP"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (TupleP [])
                        |> Expect.equal (Ok (TupleP []))

            --
            , test "(X) -- TypeNotFound -- checkP on List" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (TupleP [ TypeP (NameT "X" []) ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(1) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (TupleP [ IntP 1 ])
                        |> Expect.equal (Ok (TupleP [ IntP 1 ]))
            ]

        -- RecordP
        , describe "RecordP"
            [ test "{} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (RecordP Dict.empty)
                        |> Expect.equal (Ok (RecordP Dict.empty))

            --
            , test "{a : X} -- TypeNotFound -- checkT on Dict" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (RecordP (Dict.singleton "A" (NameT "X" [])))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "{a : Int} -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (RecordP (Dict.singleton "a" IntT))
                        |> Expect.equal (Ok (RecordP (Dict.singleton "a" IntT)))
            ]

        -- ConstructorP
        , describe "ConstructorP"
            [ test "T.A -- TypeNotFound -- getTypeDefinition" <|
                \_ ->
                    FVM.Module.new
                        |> checkP (ConstructorP ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (Err (TypeNotFound "T"))

            --
            , test "type T; (T 1).A -- TypeInputsMismatch -- getTypeDefinition" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) Dict.empty
                        |> Result.andThen (checkP (ConstructorP ( "T", [ Int 1 ] ) "A" []))
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ IntT ], expected = [] }))

            --
            , test "type T; T.A -- ConstructorNotFound -- checkP" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) Dict.empty
                        |> Result.andThen (checkP (ConstructorP ( "T", [] ) "A" []))
                        |> Expect.equal (Err (ConstructorNotFound ( "T", [] ) "A"))

            --
            , test "type T = A; T.A -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.singleton "A" ( [], [] ))
                        |> Result.andThen (checkP (ConstructorP ( "T", [] ) "A" []))
                        |> Expect.equal (Ok (ConstructorP ( "T", [] ) "A" []))

            --
            , test "type T = A (x : Int); T.A 1.1 -- ConstructorInputsMismatch -- checkP" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] ))
                        |> Result.andThen (checkP (ConstructorP ( "T", [] ) "A" [ NumberP 1.1 ]))
                        |> Expect.equal (Err (ConstructorInputsMismatch ( "T", [] ) "A" { got = [ NumberT ], expected = [ IntT ] }))

            --
            , test "type T Int = A (x : Int); (T 1).A 2 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> withType ( "T", [ IntT ] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] ))
                        |> Result.andThen (checkP (ConstructorP ( "T", [ Int 1 ] ) "A" [ IntP 2 ]))
                        |> Expect.equal (Ok (ConstructorP ( "T", [ Int 1 ] ) "A" [ IntP 2 ]))
            ]
        ]
