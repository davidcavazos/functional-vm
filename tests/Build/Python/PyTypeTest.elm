module Build.Python.PyTypeTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Build.Python exposing (pyType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "build Python -- pyType"
        -- Type
        [ describe "TypeT"
            [ test "Type" <|
                \_ ->
                    pyType TypeT
                        |> Expect.equal "Type"
            ]

        -- IntT
        , describe "IntT"
            [ test "Int" <|
                \_ ->
                    pyType IntT
                        |> Expect.equal "Int"
            ]

        -- NumberT
        , describe "NumberT"
            [ test "Number" <|
                \_ ->
                    pyType NumberT
                        |> Expect.equal "Number"
            ]

        -- NameT
        , describe "NameT"
            [ test "T" <|
                \_ ->
                    pyType (NameT "T" [])
                        |> Expect.equal "T"

            --
            , test "T 1" <|
                \_ ->
                    pyType (NameT "T" [ Int 1 ])
                        |> Expect.equal "T"

            --
            , test "T Int" <|
                \_ ->
                    pyType (NameT "T" [ Type IntT ])
                        |> Expect.equal "T[Int]"

            --
            , test "T 1 Int Number" <|
                \_ ->
                    pyType (NameT "T" [ Int 1, Type IntT, Type NumberT ])
                        |> Expect.equal "T[Int, Number]"
            ]

        -- TupleT
        , describe "TupleT"
            [ test "()" <|
                \_ ->
                    pyType (TupleT [])
                        |> Expect.equal "Tuple[]"

            --
            , test "(Int)" <|
                \_ ->
                    pyType (TupleT [ IntT ])
                        |> Expect.equal "Tuple[Int]"

            --
            , test "(Int, Number)" <|
                \_ ->
                    pyType (TupleT [ IntT, NumberT ])
                        |> Expect.equal "Tuple[Int, Number]"
            ]

        -- RecordT
        , describe "RecordT"
            [ test "{}" <|
                \_ ->
                    pyType (RecordT Dict.empty)
                        |> Expect.equal "RecordType({})"

            --
            , test "{x : Int}" <|
                \_ ->
                    pyType (RecordT (Dict.singleton "x" IntT))
                        |> Expect.equal "RecordType({'x': Int})"

            --
            , test "{x : Int, y : Number}" <|
                \_ ->
                    pyType (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ]))
                        |> Expect.equal "RecordType({'x': Int, 'y': Number})"
            ]

        -- LambdaT
        , describe "LambdaT"
            [ test "Int -> Number" <|
                \_ ->
                    pyType (LambdaT IntT NumberT)
                        |> Expect.equal "Callable[[Int], Number]"

            --
            , test "Int -> Number -> Type" <|
                \_ ->
                    pyType (LambdaT IntT (LambdaT NumberT TypeT))
                        |> Expect.equal "Callable[[Int, Number], Type]"
            ]

        -- GenericT
        , describe "GenericT"
            [ test "a" <|
                \_ ->
                    pyType (GenericT "a")
                        |> Expect.equal "a"
            ]

        -- UnionT
        , describe "UnionT"
            [ test "empty" <|
                \_ ->
                    pyType (UnionT [])
                        |> Expect.equal "Union[]"

            --
            , test "Int" <|
                \_ ->
                    pyType (UnionT [ IntT ])
                        |> Expect.equal "Union[Int]"

            --
            , test "Int | Number" <|
                \_ ->
                    pyType (UnionT [ IntT, NumberT ])
                        |> Expect.equal "Union[Int, Number]"
            ]
        ]
