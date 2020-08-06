module Validate.CheckTTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..), new)
import FVM.Validate exposing (checkT, withType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "checkT"
        -- TypeT
        [ describe "TypeT"
            [ test "Type -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT TypeT
                        |> Expect.equal (Ok TypeT)
            ]

        -- IntT
        , describe "IntT"
            [ test "Int -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT IntT
                        |> Expect.equal (Ok IntT)
            ]

        -- NumberT
        , describe "NumberT"
            [ test "Number -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT NumberT
                        |> Expect.equal (Ok NumberT)
            ]

        -- NameT
        , describe "NameT"
            [ test "X -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> checkT (NameT "X" [])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "type T; T -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] ) Dict.empty
                        |> Result.andThen (checkT (NameT "T" []))
                        |> Expect.equal (Ok (NameT "T" []))

            --
            , test "type T; T 1 -- TypeInputsMismatch" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [] ) Dict.empty
                        |> Result.andThen (checkT (NameT "T" [ Int 1 ]))
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ IntT ], expected = [] }))

            --
            , test "type T Int; T 1.1 -- TypeInputsMismatch" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [ IntT ] ) Dict.empty
                        |> Result.andThen (checkT (NameT "T" [ Number 1.1 ]))
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ NumberT ], expected = [ IntT ] }))

            --
            , test "type T Int; T 1 -- ok" <|
                \_ ->
                    FVM.new
                        |> withType ( "T", [ IntT ] ) Dict.empty
                        |> Result.andThen (checkT (NameT "T" [ Int 1 ]))
                        |> Expect.equal (Ok (NameT "T" [ Int 1 ]))
            ]

        -- TupleT
        , describe "TupleT"
            [ test "() -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (TupleT [])
                        |> Expect.equal (Ok (TupleT []))

            --
            , test "(X) -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> checkT (TupleT [ NameT "X" [] ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(Int, Number) -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (TupleT [ IntT, NumberT ])
                        |> Expect.equal (Ok (TupleT [ IntT, NumberT ]))
            ]

        -- RecordT
        , describe "RecordT"
            [ test "{} -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (RecordT Dict.empty)
                        |> Expect.equal (Ok (RecordT Dict.empty))

            --
            , test "{x : X} -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> checkT (RecordT (Dict.fromList [ ( "x", NameT "X" [] ) ]))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "{x : Int, y : Number} -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ]))
                        |> Expect.equal (Ok (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ])))
            ]

        -- LambdaT
        , describe "LambdaT"
            [ test "X -> Y -- TypeNotFound on input type" <|
                \_ ->
                    FVM.new
                        |> checkT (LambdaT (NameT "X" []) (NameT "Y" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -> X -- TypeNotFound on output type" <|
                \_ ->
                    FVM.new
                        |> checkT (LambdaT IntT (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -> Number -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (LambdaT IntT NumberT)
                        |> Expect.equal (Ok (LambdaT IntT NumberT))
            ]

        -- GenericT
        , describe "GenericT"
            [ test "a -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (GenericT "a")
                        |> Expect.equal (Ok (GenericT "a"))
            ]

        -- UnionT
        , describe "UnionT"
            [ test "empty -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (UnionT [])
                        |> Expect.equal (Ok (UnionT []))

            --
            , test "Int | X -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> checkT (UnionT [ IntT, NameT "X" [] ])
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int | Number -- ok" <|
                \_ ->
                    FVM.new
                        |> checkT (UnionT [ IntT, NumberT ])
                        |> Expect.equal (Ok (UnionT [ IntT, NumberT ]))
            ]
        ]
