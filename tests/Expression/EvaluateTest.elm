module Expression.EvaluateTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Type(..))
import FVM.Context exposing (addVariable)
import FVM.Expression exposing (evaluate)
import FVM.Module exposing (addType)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Expression.evaluate"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Type (NamedType ( "X", [] ))) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Type IntType) FVM.Context.new
                        |> Expect.equal (Ok (Type IntType))
            ]

        -- Integer
        , describe "Integer"
            [ test "1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Integer 1) FVM.Context.new
                        |> Expect.equal (Ok (Integer 1))
            ]

        -- Number
        , describe "Number"
            [ test "3.14 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Number 3.14) FVM.Context.new
                        |> Expect.equal (Ok (Number 3.14))
            ]

        -- Variable
        , describe "Variable"
            [ test "(x : X) -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Variable "x" (NamedType ( "X", [] ))) FVM.Context.new
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "var x : Int; (x : Number) -- TypeMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Variable "x" NumberType)
                            (addVariable "x" IntType FVM.Context.new)
                        |> Expect.equal (Err (TypeMismatch (Variable "x" NumberType) IntType))

            --
            , test "var x : X; (x : X) -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Variable "x" (NamedType ( "X", [] )))
                            (addVariable "x" (NamedType ( "X", [] )) FVM.Context.new)
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "type T; var x : T; (x : T) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) Dict.empty
                        |> evaluate (Variable "x" (NamedType ( "T", [] )))
                            (addVariable "x" (NamedType ( "T", [] )) FVM.Context.new)
                        |> Expect.equal (Ok (Variable "x" (NamedType ( "T", [] ))))
            ]

        -- Lambda
        , describe "Lambda"
            [ test "(x : X) -> 1 -- TypeNotFound on input type" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Lambda ( "x", NamedType ( "X", [] ) ) (Integer 1)) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "(x : Int) -> (y : Number) -- NameNotFound on output" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Lambda ( "x", IntType ) (Variable "y" NumberType)) FVM.Context.new
                        |> Expect.equal (Err (NameNotFound "y"))

            --
            , test "(x : Int) -> (x : Number) -- TypeMismatch on output type" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Lambda ( "x", IntType ) (Variable "x" NumberType)) FVM.Context.new
                        |> Expect.equal (Err (TypeMismatch (Variable "x" NumberType) IntType))

            --
            , test "(x : Int) -> (x : Int) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Lambda ( "x", IntType ) (Variable "x" IntType)) FVM.Context.new
                        |> Expect.equal (Ok (Lambda ( "x", IntType ) (Variable "x" IntType)))
            ]

        -- Tuple
        , describe "Tuple"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Tuple []) FVM.Context.new
                        |> Expect.equal (Ok (Tuple []))

            --
            , test "(1, (x : Int)) -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Tuple [ Integer 1, Variable "x" IntType ]) FVM.Context.new
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "(1, 2) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Tuple [ Integer 1, Integer 2 ]) FVM.Context.new
                        |> Expect.equal (Ok (Tuple [ Integer 1, Integer 2 ]))
            ]

        -- Record
        , describe "Record"
            [ test "() -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Record Dict.empty) FVM.Context.new
                        |> Expect.equal (Ok (Record Dict.empty))

            --
            , test "(x = 1, y = (x : Int)) -- NameNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Record (Dict.fromList [ ( "x", Integer 1 ), ( "y", Variable "x" IntType ) ])) FVM.Context.new
                        |> Expect.equal (Err (NameNotFound "x"))

            --
            , test "(x = 1, y = 2) -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Record (Dict.fromList [ ( "x", Integer 1 ), ( "y", Integer 2 ) ])) FVM.Context.new
                        |> Expect.equal (Ok (Record (Dict.fromList [ ( "x", Integer 1 ), ( "y", Integer 2 ) ])))
            ]

        -- Constructor
        , describe "Constructor"
            [ test "T.A -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Constructor ( "T", [] ) "A" []) FVM.Context.new
                        |> Expect.equal (Err (TypeNotFound "T"))

            --
            , test "type T; (T 1).A -- TypeInputsMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) Dict.empty
                        |> evaluate (Constructor ( "T", [ Integer 1 ] ) "A" []) FVM.Context.new
                        |> Expect.equal (Err (TypeInputsMismatch "T" { got = [ Integer 1 ], expected = [] }))

            --
            , test "type T Int; (T 3.14).A -- TypeMismatch on type input" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [ IntType ] ) Dict.empty
                        |> evaluate (Constructor ( "T", [ Number 3.14 ] ) "A" []) FVM.Context.new
                        |> Expect.equal (Err (TypeMismatch (Number 3.14) IntType))

            --
            , test "type T Int; (T 1).A -- ConstructorNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [ IntType ] ) Dict.empty
                        |> evaluate (Constructor ( "T", [ Integer 1 ] ) "A" []) FVM.Context.new
                        |> Expect.equal (Err (ConstructorNotFound ( "T", [ Integer 1 ] ) "A"))

            --
            , test "type T = A; T.A 1 -- ConstructorInputsMismatch" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [], [] ) ) ])
                        |> evaluate (Constructor ( "T", [] ) "A" [ Integer 1 ]) FVM.Context.new
                        |> Expect.equal (Err (ConstructorInputsMismatch ( "T", [] ) "A" { got = [ Integer 1 ], expected = [] }))

            --
            , test "type T = A (x : Int); T.A 3.14 -- TypeMismatch on constructor input" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [ ( "x", IntType ) ], [] ) ) ])
                        |> evaluate (Constructor ( "T", [] ) "A" [ Number 3.14 ]) FVM.Context.new
                        |> Expect.equal (Err (TypeMismatch (Number 3.14) IntType))

            --
            , test "type T = A (x : Int); T.A 1 -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> addType ( "T", [] ) (Dict.fromList [ ( "A", ( [ ( "x", IntType ) ], [] ) ) ])
                        |> evaluate (Constructor ( "T", [] ) "A" [ Integer 1 ]) FVM.Context.new
                        |> Expect.equal (Ok (Constructor ( "T", [] ) "A" [ Integer 1 ]))
            ]

        -- Call
        , describe "Call"
            [ test "(1 2 : Int) -- CallNonFunction" <|
                \_ ->
                    FVM.Module.new
                        |> evaluate (Call (Integer 1) (Integer 2)) FVM.Context.new
                        |> Expect.equal (Err (CallNonFunction (Integer 1) (Integer 2)))

            --     --
            --     , test "(x 1 : Int) -- NameNotFound" <|
            --         \_ ->
            --             FVM.Module.new
            --                 |> evaluate (Call (Variable "x" IntType) (Integer 1) IntType) FVM.Context.new
            --                 |> Expect.equal (Err (NameNotFound "x"))
            --     --
            --     , test "with x : Int; (x 1 : Int) -- CallNonFunction" <|
            --         \_ ->
            --             FVM.Module.new
            --                 |> evaluate (Call (Variable "x" IntType) (Integer 1) IntType)
            --                     (addVariable "x" IntType FVM.Context.new)
            --                 |> Expect.equal (Err (CallNonFunction (Variable "x" IntType) (Integer 1)))
            --     --
            --     , test "with f : Int -> Number; (f 3.14 : Int) -- TypeMismatch on input type" <|
            --         \_ ->
            --             FVM.Module.new
            --                 |> evaluate (Call (Variable "x" (LambdaType IntType NumberType)) (Number 3.14) IntType)
            --                     (addVariable "x" (LambdaType IntType NumberType) FVM.Context.new)
            --                 |> Expect.equal (Err (TypeMismatch (Number 3.14) IntType))
            -- --
            -- , test "with f : Int -> Number; (f 1 : Int) -- TypeMismatch on output type" <|
            --     \_ ->
            --         FVM.Module.new
            --             |> evaluate (Call (Variable "x" (LambdaType IntType NumberType)) (Integer 1) IntType)
            --                 (addVariable "x" (LambdaType IntType NumberType) FVM.Context.new)
            --             |> Expect.equal (Err (CallNonFunction (Variable "x" IntType) (Integer 1)))
            ]

        -- Call Expression Expression Type -- (f : Int -> Int) 1
        -- Match Expression (List ( Pattern, Expression )) Type -- case x of 1 -> True; _ -> False
        ]
