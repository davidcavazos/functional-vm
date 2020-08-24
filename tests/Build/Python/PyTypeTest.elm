module Build.Python.PyTypeTest exposing (suite)

import ASM exposing (Accessor(..), Condition(..), Expression(..), Type(..))
import Dict
import Expect
import FVM.Build.Python exposing (pyType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "build Python -- pyType"
        -- TypeT
        [ describe "TypeT"
            [ test "type type -- Type" <|
                \_ ->
                    pyType TypeT
                        |> Expect.equal "Type"
            ]

        -- IntT
        , describe "IntT"
            [ test "int type -- Int" <|
                \_ ->
                    pyType IntT
                        |> Expect.equal "Int"
            ]

        -- NumberT
        , describe "NumberT"
            [ test "number type -- Number" <|
                \_ ->
                    pyType NumberT
                        |> Expect.equal "Number"
            ]

        -- TupleT
        , describe "TupleT"
            [ test "tuple type empty -- ()" <|
                \_ ->
                    pyType (TupleT [])
                        |> Expect.equal "Tuple[]"

            --
            , test "tuple type one item -- (Int)" <|
                \_ ->
                    pyType (TupleT [ IntT ])
                        |> Expect.equal "Tuple[Int]"

            --
            , test "tuple type many items -- (Int, Number, Type)" <|
                \_ ->
                    pyType (TupleT [ IntT, NumberT, TypeT ])
                        |> Expect.equal "Tuple[Int,Number,Type]"
            ]

        -- RecordT
        , describe "RecordT"
            [ test "record type empty -- {}" <|
                \_ ->
                    pyType (RecordT Dict.empty)
                        |> Expect.equal "RecordType()"

            --
            , test "record type one field -- {x : Int}" <|
                \_ ->
                    pyType (RecordT (Dict.singleton "x" IntT))
                        |> Expect.equal "RecordType(x=Int)"

            --
            , test "record type many fields -- {x : Int, y : Number, z : Type}" <|
                \_ ->
                    pyType (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ), ( "z", TypeT ) ]))
                        |> Expect.equal "RecordType(x=Int,y=Number,z=Type)"
            ]

        -- NameT
        , describe "NameT"
            [ test "named type without inputs -- T" <|
                \_ ->
                    pyType (NameT "T" [])
                        |> Expect.equal "T"

            --
            , test "named type with one type input -- T Int" <|
                \_ ->
                    pyType (NameT "T" [ Type IntT ])
                        |> Expect.equal "T[Int]"

            --
            , test "named type with one expression input -- T 1" <|
                \_ ->
                    pyType (NameT "T" [ Int 1 ])
                        |> Expect.equal "T"

            --
            , test "named type with many inputs -- T 1 Int 2 Number" <|
                \_ ->
                    pyType (NameT "T" [ Int 1, Type IntT, Int 2, Type NumberT ])
                        |> Expect.equal "T[Int,Number]"
            ]

        -- FunctionT
        , describe "FunctionT"
            [ test "function type without inputs -- () -> Type" <|
                \_ ->
                    pyType (FunctionT [] TypeT)
                        |> Expect.equal "Callable[[],Type]"

            --
            , test "function type with one input -- Int -> Type" <|
                \_ ->
                    pyType (FunctionT [ IntT ] TypeT)
                        |> Expect.equal "Callable[[Int],Type]"

            --
            , test "function type with many inputs -- Int -> Int -> Number -> Type" <|
                \_ ->
                    pyType (FunctionT [ IntT, IntT, NumberT ] TypeT)
                        |> Expect.equal "Callable[[Int,Int,Number],Type]"
            ]

        -- GenericT
        , describe "GenericT"
            [ test "generic type -- a" <|
                \_ ->
                    pyType (GenericT "a")
                        |> Expect.equal "a"
            ]

        -- UnionT
        , describe "UnionT"
            [ test "union type without types" <|
                \_ ->
                    pyType (UnionT [])
                        |> Expect.equal "Union[]"

            --
            , test "union type of one type -- Int" <|
                \_ ->
                    pyType (UnionT [ IntT ])
                        |> Expect.equal "Union[Int]"

            --
            , test "union type of many types -- Int | Number | Type" <|
                \_ ->
                    pyType (UnionT [ IntT, NumberT, TypeT ])
                        |> Expect.equal "Union[Int,Number,Type]"
            ]
        ]
