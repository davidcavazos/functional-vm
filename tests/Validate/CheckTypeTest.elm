module Validate.CheckTypeTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Package exposing (letType)
import FVM.Validate exposing (checkType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "checkType"
        [ describe "cases"
            -- TypeT
            [ describe "TypeT"
                [ test "Type -- ok" <|
                    \_ ->
                        checkType FVM.Package.new TypeT
                            |> Expect.equal (Ok TypeT)
                ]

            -- IntT
            , describe "IntT"
                [ test "Int -- ok" <|
                    \_ ->
                        checkType FVM.Package.new IntT
                            |> Expect.equal (Ok IntT)
                ]

            -- NumberT
            , describe "NumberT"
                [ test "Number -- ok" <|
                    \_ ->
                        checkType FVM.Package.new NumberT
                            |> Expect.equal (Ok NumberT)
                ]

            -- NameT
            , describe "NameT"
                [ test "type T Int; T 1.1 -- TypeInputsMismatch -- getTypeDefinition" <|
                    \_ ->
                        checkType (letType ( "T", [ IntT ] ) Dict.empty FVM.Package.new)
                            (NameT "T" [ Number 1.1 ])
                            |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ NumberT ], expected = [ IntT ] }))

                --
                , test "type T Int; T 1 -- ok" <|
                    \_ ->
                        checkType (letType ( "T", [ IntT ] ) Dict.empty FVM.Package.new)
                            (NameT "T" [ Int 1 ])
                            |> Expect.equal (Ok (NameT "T" [ Int 1 ]))
                ]

            -- TupleT
            , describe "TupleT"
                [ test "() -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (TupleT [])
                            |> Expect.equal (Ok (TupleT []))

                --
                , test "(X) -- TypeNotFound -- checkType on List" <|
                    \_ ->
                        checkType FVM.Package.new (TupleT [ NameT "X" [] ])
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "(Int, Number) -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (TupleT [ IntT, NumberT ])
                            |> Expect.equal (Ok (TupleT [ IntT, NumberT ]))
                ]

            -- RecordT
            , describe "RecordT"
                [ test "{} -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (RecordT Dict.empty)
                            |> Expect.equal (Ok (RecordT Dict.empty))

                --
                , test "{x : X} -- TypeNotFound -- checkType on Dict" <|
                    \_ ->
                        checkType FVM.Package.new (RecordT (Dict.singleton "x" (NameT "X" [])))
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "{x : Int, y : Number} -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ]))
                            |> Expect.equal (Ok (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ])))
                ]

            -- LambdaT
            , describe "LambdaT"
                [ test "X -> Y -- TypeNotFound -- checkType on input type" <|
                    \_ ->
                        checkType FVM.Package.new (LambdaT (NameT "X" []) (NameT "Y" []))
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "Int -> X -- TypeNotFound -- checkType output type" <|
                    \_ ->
                        checkType FVM.Package.new (LambdaT IntT (NameT "X" []))
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "Int -> Number -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (LambdaT IntT NumberT)
                            |> Expect.equal (Ok (LambdaT IntT NumberT))
                ]

            -- GenericT
            , describe "GenericT"
                [ test "a -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (GenericT "a")
                            |> Expect.equal (Ok (GenericT "a"))
                ]

            -- UnionT
            , describe "UnionT"
                [ test "empty -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (UnionT [])
                            |> Expect.equal (Ok (UnionT []))

                --
                , test "Int | X -- TypeNotFound -- checkType on List" <|
                    \_ ->
                        checkType FVM.Package.new (UnionT [ IntT, NameT "X" [] ])
                            |> Expect.equal (Err (TypeNotFound "X"))

                --
                , test "Int | Number -- ok" <|
                    \_ ->
                        checkType FVM.Package.new (UnionT [ IntT, NumberT ])
                            |> Expect.equal (Ok (UnionT [ IntT, NumberT ]))
                ]
            ]
        ]
