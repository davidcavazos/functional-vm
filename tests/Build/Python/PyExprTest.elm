module Build.Python.PyExprTest exposing (suite)

import ASM exposing (Accessor(..), Condition(..), Expr(..), Type(..))
import Dict
import Expect
import FVM.Build.Python exposing (pyExpr)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "dump thon -- pyExpr"
        -- Type
        [ describe "Type"
            [ test "type -- Type" <|
                \_ ->
                    pyExpr (Type TypeT)
                        |> Expect.equal "Type"
            ]

        -- Int
        , describe "Int"
            [ test "int postive -- 1" <|
                \_ ->
                    pyExpr (Int 1)
                        |> Expect.equal "1"

            --
            , test "int negative -- -1" <|
                \_ ->
                    pyExpr (Int -1)
                        |> Expect.equal "-1"
            ]

        -- Number
        , describe "Number"
            [ test "number positive -- 1.0" <|
                \_ ->
                    pyExpr (Number 1.0)
                        |> Expect.equal "1.0"

            --
            , test "number negative -- -1.0" <|
                \_ ->
                    pyExpr (Number -1.0)
                        |> Expect.equal "-1.0"
            ]

        -- Tuple
        , describe "Tuple"
            [ test "tuple empty -- ()" <|
                \_ ->
                    pyExpr (Tuple [])
                        |> Expect.equal "()"

            --
            , test "tuple with one item -- (1)" <|
                \_ ->
                    pyExpr (Tuple [ Int 1 ])
                        |> Expect.equal "(1,)"

            --
            , test "tuple with many items -- (1, 2, 3)" <|
                \_ ->
                    pyExpr (Tuple [ Int 1, Int 2, Int 3 ])
                        |> Expect.equal "(1,2,3)"
            ]

        -- Record
        , describe "Record"
            [ test "record empty -- {}" <|
                \_ ->
                    pyExpr (Record Dict.empty)
                        |> Expect.equal "Record()"

            --
            , test "record with one item -- {x = 1}" <|
                \_ ->
                    pyExpr (Record (Dict.singleton "x" (Int 1)))
                        |> Expect.equal "Record(x=1)"

            --
            , test "record with many items -- {x = 1, y = 2, z = 3}" <|
                \_ ->
                    pyExpr (Record (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ), ( "z", Int 3 ) ]))
                        |> Expect.equal "Record(x=1,y=2,z=3)"
            ]

        -- Let
        , describe "Let"
            [ test "let empty -- 1" <|
                \_ ->
                    pyExpr (Let Dict.empty (Int 1))
                        |> Expect.equal "1"

            --
            , test "let one variable -- let x = 1; 2" <|
                \_ ->
                    pyExpr (Let (Dict.singleton "x" (Int 1)) (Int 2))
                        |> Expect.equal "(lambda x:2)(1)"

            --
            , test "let many variables -- let x = 1; let y = 2; let y = 3; 4" <|
                \_ ->
                    pyExpr (Let (Dict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ), ( "z", Int 3 ) ]) (Int 4))
                        |> Expect.equal "(lambda x,y,z:4)(1,2,3)"
            ]

        -- Load
        , describe "Load"
            [ test "load variable -- x" <|
                \_ ->
                    pyExpr (Load "x" IntT)
                        |> Expect.equal "x"
            ]

        -- Function
        , describe "Function"
            [ test "function without inputs -- () -> 1" <|
                \_ ->
                    pyExpr (Function Dict.empty (Int 1))
                        |> Expect.equal "lambda:1"

            --
            , test "function with one input -- x -> 1" <|
                \_ ->
                    pyExpr (Function (Dict.singleton "x" IntT) (Int 1))
                        |> Expect.equal "lambda x:1"

            --
            , test "function with many inputs -- x y z -> 1" <|
                \_ ->
                    pyExpr (Function (Dict.fromList [ ( "x", IntT ), ( "y", IntT ), ( "z", IntT ) ]) (Int 1))
                        |> Expect.equal "lambda x,y,z:1"
            ]

        -- Call
        , describe "Call"
            [ test "call without inputs -- let f : () -> Int; f" <|
                \_ ->
                    pyExpr (Call (Load "f" (FunctionT [] IntT)) [])
                        |> Expect.equal "f()"

            --
            , test "call with one input -- let f : Int -> Int; f 1" <|
                \_ ->
                    pyExpr (Call (Load "f" (FunctionT [ IntT ] IntT)) [ Int 1 ])
                        |> Expect.equal "f(1)"

            --
            , test "call with many inputs -- let f : Int -> Int -> Int -> Int; f 1 2 3" <|
                \_ ->
                    pyExpr (Call (Load "f" (FunctionT [ IntT, IntT, IntT ] IntT)) [ Int 1, Int 2, Int 3 ])
                        |> Expect.equal "f(1,2,3)"

            --
            , test "call with currying -- let f : Int -> Int -> Int -> Int; f 1" <|
                \_ ->
                    pyExpr (Call (Load "f" (FunctionT [ IntT, IntT, IntT ] IntT)) [ Int 1 ])
                        |> Expect.equal "lambda _2,_3:f(1,_2,_3)"
            ]

        -- Constructor
        , describe "Constructor"
            [ test "constructor without inputs -- T.A" <|
                \_ ->
                    pyExpr (Constructor ( "T", [] ) "A" [])
                        |> Expect.equal "A()"

            --
            , test "constructor with one input -- T.A 1" <|
                \_ ->
                    pyExpr (Constructor ( "T", [] ) "A" [ Int 1 ])
                        |> Expect.equal "A(1)"

            --
            , test "constructor with many inputs -- T.A 1 2 3" <|
                \_ ->
                    pyExpr (Constructor ( "T", [] ) "A" [ Int 1, Int 2, Int 3 ])
                        |> Expect.equal "A(1,2,3)"
            ]

        -- CaseOf
        , describe "CaseOf"
            [ test "default case -- case 0 of _ -> 1.1" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Int 0, NumberT )
                            []
                            ( Dict.empty, Number 1.1 )
                        )
                        |> Expect.equal "(lambda _:1.1)(0)"

            --
            , test "default case with self variable -- case 0 of x -> 1.1" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Int 0, NumberT )
                            []
                            ( Dict.singleton "x" Self, Number 1.1 )
                        )
                        |> Expect.equal "(lambda _:(lambda x:1.1)(_))(0)"

            --
            , test "default case with tuple item variable -- case (0) of (x) -> 1.1" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Tuple [ Int 1 ], NumberT )
                            []
                            ( Dict.singleton "x" (TupleItem 0), Number 1.1 )
                        )
                        |> Expect.equal "(lambda _:(lambda x:1.1)(_[0]))((1,))"

            --
            , test "default case with record field variable -- case {x = 0} of {x} -> 1.1" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Record (Dict.singleton "x" (Int 0)), NumberT )
                            []
                            ( Dict.singleton "x" (RecordField "x"), Number 1.1 )
                        )
                        |> Expect.equal "(lambda _:(lambda x:1.1)(_.x))(Record(x=0))"

            --
            , test "default case with constructor input variable -- case T.A 0 of A x -> 1.1" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Constructor ( "T", [] ) "A" [ Int 42 ], NumberT )
                            []
                            ( Dict.singleton "x" (ConstructorInput 0), Number 1.1 )
                        )
                        |> Expect.equal "(lambda _:(lambda x:1.1)(_._1))(A(42))"

            --
            , test "default case with many variables -- case 0 of (x as y) as z -> 1.1" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Int 0, NumberT )
                            []
                            ( Dict.fromList [ ( "x", Self ), ( "y", Self ), ( "z", Self ) ], Number 1.1 )
                        )
                        |> Expect.equal "(lambda _:(lambda x,y,z:1.1)(_,_,_))(0)"

            --
            , test "case without conditions -- no valid examples" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Int 0, NumberT )
                            [ ( [], Dict.empty, Number 1.1 ) ]
                            ( Dict.empty, Number 2.2 )
                        )
                        |> Expect.equal "(lambda _:1.1 if True else 2.2)(0)"

            --
            , test "case equals type -- case Type of Int -> 1; _ -> 2" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Type TypeT, NumberT )
                            [ ( [ EqualsType Self IntT ], Dict.empty, Int 1 ) ]
                            ( Dict.empty, Int 2 )
                        )
                        |> Expect.equal "(lambda _:1 if _==Int else 2)(Type)"

            --
            , test "case equals int -- case 0 of 1 -> 1.1; _ -> 2.2" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Int 0, NumberT )
                            [ ( [ EqualsInt Self 1 ], Dict.empty, Number 1.1 ) ]
                            ( Dict.empty, Number 2.2 )
                        )
                        |> Expect.equal "(lambda _:1.1 if _==1 else 2.2)(0)"

            --
            , test "case equals number -- case 0.0 of 1.1 -> 1; _ -> 2" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Number 0.0, IntT )
                            [ ( [ EqualsNumber Self 1.1 ], Dict.empty, Int 1 ) ]
                            ( Dict.empty, Int 2 )
                        )
                        |> Expect.equal "(lambda _:1 if _==1.1 else 2)(0.0)"

            --
            , test "case equals constructor -- case T.A of A -> 1; _ -> 2" <|
                \_ ->
                    pyExpr
                        (CaseOf ( Constructor ( "T", [] ) "A" [], IntT )
                            [ ( [ EqualsConstructor Self ( "T", [] ) "A" ], Dict.empty, Int 1 ) ]
                            ( Dict.empty, Int 2 )
                        )
                        |> Expect.equal "(lambda _:1 if type(_)==A else 2)(A())"
            ]
        ]
