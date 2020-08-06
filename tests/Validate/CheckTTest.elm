module Validate.CheckTTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Package exposing (letType)
import FVM.Validate exposing (checkT)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "checkT"
        [ describe "cases"
            -- TypeT
            [ describe "TypeT"
                [ test "Type -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT TypeT
                            |> Expect.equal (Ok TypeT)
                ]

            -- IntT
            , describe "IntT"
                [ test "Int -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT IntT
                            |> Expect.equal (Ok IntT)
                ]

            -- NumberT
            , describe "NumberT"
                [ test "Number -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT NumberT
                            |> Expect.equal (Ok NumberT)
                ]

            -- NameT
            , describe "NameT"
                [ test "type T Int; T 1.1 -- TypeInputsMismatch -- getTypeDefinition" <|
                    \_ ->
                        FVM.Package.new
                            |> letType ( "T", [ IntT ] ) Dict.empty
                            |> checkT (NameT "T" [ Number 1.1 ])
                            |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ NumberT ], expected = [ IntT ] }))

                --
                , test "type T Int; T 1 -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> letType ( "T", [ IntT ] ) Dict.empty
                            |> checkT (NameT "T" [ Int 1 ])
                            |> Expect.equal (Ok (NameT "T" [ Int 1 ]))
                ]

            -- TupleT
            , describe "TupleT"
                [ test "() -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (TupleT [])
                            |> Expect.equal (Ok (TupleT []))

                --
                , test "(X) -- TypeNotFound -- checkT on List" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (TupleT [ NameT "X" [] ])
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "(Int, Number) -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (TupleT [ IntT, NumberT ])
                            |> Expect.equal (Ok (TupleT [ IntT, NumberT ]))
                ]

            -- RecordT
            , describe "RecordT"
                [ test "{} -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (RecordT Dict.empty)
                            |> Expect.equal (Ok (RecordT Dict.empty))

                --
                , test "{x : X} -- TypeNotFound -- checkT on Dict" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (RecordT (Dict.singleton "x" (NameT "X" [])))
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "{x : Int, y : Number} -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ]))
                            |> Expect.equal (Ok (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ])))
                ]

            -- LambdaT
            , describe "LambdaT"
                [ test "X -> Y -- TypeNotFound -- checkT on input type" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (LambdaT (NameT "X" []) (NameT "Y" []))
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "Int -> X -- TypeNotFound -- checkT output type" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (LambdaT IntT (NameT "X" []))
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "Int -> Number -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (LambdaT IntT NumberT)
                            |> Expect.equal (Ok (LambdaT IntT NumberT))
                ]

            -- GenericT
            , describe "GenericT"
                [ test "a -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (GenericT "a")
                            |> Expect.equal (Ok (GenericT "a"))
                ]

            -- UnionT
            , describe "UnionT"
                [ test "empty -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (UnionT [])
                            |> Expect.equal (Ok (UnionT []))

                --
                , test "Int | X -- TypeNotFound -- checkT on List" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (UnionT [ IntT, NameT "X" [] ])
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "Int | Number -- ok" <|
                    \_ ->
                        FVM.Package.new
                            |> checkT (UnionT [ IntT, NumberT ])
                            |> Expect.equal (Ok (UnionT [ IntT, NumberT ]))
                ]
            ]
        ]
