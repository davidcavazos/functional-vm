module Validate.CheckPatternTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Package exposing (letType)
import FVM.Validate exposing (checkPattern)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "checkPattern"
        -- AnyP
        [ describe "AnyP"
            [ test "_ : X -- TypeNotFound -- checkT" <|
                \_ ->
                    checkPattern FVM.Package.new (AnyP (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "_ : Int -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (AnyP IntT)
                        |> Expect.equal (Ok (AnyP IntT))
            ]

        -- NameP
        , describe "NameP"
            [ test "X as x -- TypeNotFound -- checkPattern" <|
                \_ ->
                    checkPattern FVM.Package.new (NameP (AnyP (NameT "X" [])) "x")
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int as x -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (NameP (AnyP IntT) "x")
                        |> Expect.equal (Ok (NameP (AnyP IntT) "x"))
            ]

        -- TypeP
        , describe "TypeP"
            [ test "X -- TypeNotFound -- checkT" <|
                \_ ->
                    checkPattern FVM.Package.new (TypeP (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (TypeP IntT)
                        |> Expect.equal (Ok (TypeP IntT))
            ]

        -- IntP
        , describe "IntP"
            [ test "1 -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (IntP 1)
                        |> Expect.equal (Ok (IntP 1))
            ]

        -- NumberP
        , describe "NumberP"
            [ test "1.1 -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (NumberP 1.1)
                        |> Expect.equal (Ok (NumberP 1.1))
            ]

        -- TupleP
        , describe "TupleP"
            [ test "() -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (TupleP [])
                        |> Expect.equal (Ok (TupleP []))

            --
            , test "(X) -- TypeNotFound -- checkPattern on List" <|
                \_ ->
                    checkPattern FVM.Package.new (TupleP [ TypeP (NameT "X" []) ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(1) -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (TupleP [ IntP 1 ])
                        |> Expect.equal (Ok (TupleP [ IntP 1 ]))
            ]

        -- RecordP
        , describe "RecordP"
            [ test "{} -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (RecordP Dict.empty)
                        |> Expect.equal (Ok (RecordP Dict.empty))

            --
            , test "{a : X} -- TypeNotFound -- checkT on Dict" <|
                \_ ->
                    checkPattern FVM.Package.new (RecordP (Dict.singleton "A" (NameT "X" [])))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "{a : Int} -- ok" <|
                \_ ->
                    checkPattern FVM.Package.new (RecordP (Dict.singleton "a" IntT))
                        |> Expect.equal (Ok (RecordP (Dict.singleton "a" IntT)))
            ]

        -- ConstructorP
        , describe "ConstructorP"
            [ test "T.A -- TypeNotFound -- getTypeDefinition" <|
                \_ ->
                    checkPattern FVM.Package.new (ConstructorP ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (Err (TypeNotFound "T"))

            --
            , test "type T; (T 1).A -- TypeInputsMismatch -- getTypeDefinition" <|
                \_ ->
                    checkPattern (letType ( "T", [] ) Dict.empty FVM.Package.new)
                        (ConstructorP ( "T", [ Int 1 ] ) "A" [])
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ IntT ], expected = [] }))

            --
            , test "type T; T.A -- ConstructorNotFound -- checkPattern" <|
                \_ ->
                    checkPattern (letType ( "T", [] ) Dict.empty FVM.Package.new)
                        (ConstructorP ( "T", [] ) "A" [])
                        |> Expect.equal (Err (ConstructorNotFound ( "T", [] ) "A"))

            --
            , test "type T = A; T.A -- ok" <|
                \_ ->
                    checkPattern (letType ( "T", [] ) (Dict.singleton "A" ( [], [] )) FVM.Package.new)
                        (ConstructorP ( "T", [] ) "A" [])
                        |> Expect.equal (Ok (ConstructorP ( "T", [] ) "A" []))

            --
            , test "type T = A (x : Int); T.A 1.1 -- ConstructorInputsMismatch -- checkPattern" <|
                \_ ->
                    checkPattern (letType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] )) FVM.Package.new)
                        (ConstructorP ( "T", [] ) "A" [ NumberP 1.1 ])
                        |> Expect.equal (Err (ConstructorInputsMismatch ( "T", [] ) "A" { got = [ NumberT ], expected = [ IntT ] }))

            --
            , test "type T Int = A (x : Int); (T 1).A 2 -- ok" <|
                \_ ->
                    checkPattern (letType ( "T", [ IntT ] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] )) FVM.Package.new)
                        (ConstructorP ( "T", [ Int 1 ] ) "A" [ IntP 2 ])
                        |> Expect.equal (Ok (ConstructorP ( "T", [ Int 1 ] ) "A" [ IntP 2 ]))
            ]
        ]
