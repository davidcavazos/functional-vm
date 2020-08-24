module Build.CompileTypeTest exposing (suite)

import ASM
import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Build exposing (compileType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "compileType"
        -- TypeT
        [ describe "TypeT"
            [ test "type type -- Type" <|
                \_ ->
                    compileType TypeT
                        |> Expect.equal ASM.TypeT
            ]

        -- IntT
        , describe "IntT"
            [ test "int type -- Int" <|
                \_ ->
                    compileType IntT
                        |> Expect.equal ASM.IntT
            ]

        -- NumberT
        , describe "NumberT"
            [ test "number type -- Number" <|
                \_ ->
                    compileType NumberT
                        |> Expect.equal ASM.NumberT
            ]

        -- NameT
        , describe "NameT"
            [ test "named type -- T 1 2.2" <|
                \_ ->
                    compileType (NameT "T" [ Int 1, Number 2.2 ])
                        |> Expect.equal (ASM.NameT "T" [ ASM.Int 1, ASM.Number 2.2 ])
            ]

        -- TupleT
        , describe "TupleT"
            [ test "tuple -- (1, 2.2)" <|
                \_ ->
                    compileType (TupleT [ IntT, NumberT ])
                        |> Expect.equal (ASM.TupleT [ ASM.IntT, ASM.NumberT ])
            ]

        -- RecordT
        , describe "RecordT"
            [ test "record -- (x : Int, y : Number)" <|
                \_ ->
                    compileType (RecordT (Dict.fromList [ ( "x", IntT ), ( "y", NumberT ) ]))
                        |> Expect.equal (ASM.RecordT (Dict.fromList [ ( "x", ASM.IntT ), ( "y", ASM.NumberT ) ]))
            ]

        -- LambdaT
        , describe "LambdaT"
            [ test "function with 1 input -- Int -> Number" <|
                \_ ->
                    compileType (LambdaT IntT NumberT)
                        |> Expect.equal (ASM.FunctionT [ ASM.IntT ] ASM.NumberT)

            --
            , test "function with many inputs -- Int -> Number -> Type -> ()" <|
                \_ ->
                    compileType (LambdaT IntT (LambdaT NumberT (LambdaT TypeT (TupleT []))))
                        |> Expect.equal (ASM.FunctionT [ ASM.IntT, ASM.NumberT, ASM.TypeT ] (ASM.TupleT []))
            ]

        -- GenericT
        , describe "GenericT"
            [ test "generic type -- a" <|
                \_ ->
                    compileType (GenericT "a")
                        |> Expect.equal (ASM.GenericT "a")
            ]

        -- UnionT
        , describe "UnionT"
            [ test "union type -- Int | Number | Type" <|
                \_ ->
                    compileType (UnionT [ IntT, NumberT, TypeT ])
                        |> Expect.equal (ASM.UnionT [ ASM.IntT, ASM.NumberT, ASM.TypeT ])
            ]
        ]
