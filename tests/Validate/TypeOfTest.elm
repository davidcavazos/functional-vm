module Validate.TypeOfTest exposing (suite)

import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Module
import FVM.Validate exposing (typeOf)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typeOf"
        -- Type
        [ describe "Type"
            [ test "X -- TypeNotFound" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type (NameT "X" []))
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "Int -- ok" <|
                \_ ->
                    FVM.Module.new
                        |> typeOf (Type IntT)
                        |> Expect.equal (Ok TypeT)
            ]

        -- -- Integer
        -- , describe "Integer"
        --     [ test "1 -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Integer 1)
        --                 |> Expect.equal (Ok IntT)
        --     ]
        -- -- Number
        -- , describe "Number"
        --     [ test "1.1 -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Number 1.1)
        --                 |> Expect.equal (Ok NumberType)
        --     ]
        -- -- Input
        -- , describe "Input"
        --     [ test "X -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Input (NameT "X" []))
        --                 |> Expect.equal (Err (TypeNotFound "X"))
        --     --
        --     , test "Int -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Input IntT)
        --                 |> Expect.equal (Ok IntT)
        --     ]
        -- -- Load
        -- , describe "Load"
        --     [ test "x -- NameNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Load "x")
        --                 |> Expect.equal (Err (NameNotFound "x"))
        --     --
        --     , test "let x = 1; x-- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> addName "x" (Integer 1)
        --                 |> typeOf (Load "x")
        --                 |> Expect.equal (Ok IntT)
        --     ]
        -- -- Lambda
        -- , describe "Lambda"
        --     [ test "(x : X) -> 1.1 -- TypeNotFound on input type" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Lambda ( "x", (NameT "X" []) ) (Integer 1))
        --                 |> Expect.equal (Err (TypeNotFound "X"))
        --     --
        --     , test "(x : Int) -> y -- NameNotFound on output type" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Lambda ( "x", IntT ) (Load "y"))
        --                 |> Expect.equal (Err (NameNotFound "y"))
        --     --
        --     , test "(x : Int) -> 1.1 -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Lambda ( "x", IntT ) (Number 1.1))
        --                 |> Expect.equal (Ok (LambdaType IntT NumberType))
        --     --
        --     , test "(x : Int) -> x -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Lambda ( "x", IntT ) (Load "x"))
        --                 |> Expect.equal (Ok (LambdaType IntT IntT))
        --     ]
        -- -- Tuple
        -- , describe "Tuple"
        --     [ test "() -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Tuple [])
        --                 |> Expect.equal (Ok (TupleType []))
        --     --
        --     , test "(1, x) -- NameNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Tuple [ Integer 1, Load "x" ])
        --                 |> Expect.equal (Err (NameNotFound "x"))
        --     --
        --     , test "(1, 2) -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Tuple [ Integer 1, Integer 2 ])
        --                 |> Expect.equal (Ok (TupleType [ IntT, IntT ]))
        --     ]
        -- -- Record
        -- , describe "Record"
        --     [ test "() -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Record Dict.empty)
        --                 |> Expect.equal (Ok (RecordType Dict.empty))
        --     --
        --     , test "(a = 1, b = x) -- NameNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Record (Dict.fromList [ ( "a", Integer 1 ), ( "b", Load "x" ) ]))
        --                 |> Expect.equal (Err (NameNotFound "x"))
        --     --
        --     , test "(a = 1, b = 1.1) -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Record (Dict.fromList [ ( "a", Integer 1 ), ( "b", Number 1.1 ) ]))
        --                 |> Expect.equal (Ok (RecordType (Dict.fromList [ ( "a", IntT ), ( "b", NumberType ) ])))
        --     ]
        -- -- Constructor
        -- , describe "Constructor"
        --     [ test "T.A -- TypeNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Constructor ( "T", [] ) "A" [])
        --                 |> Expect.equal (Err (TypeNotFound "T"))
        --     --
        --     , test "type T Int; T.A -- TypeInputsMismatch on type inputs" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> addType ( "T", [ IntT ] ) Dict.empty
        --                 |> typeOf (Constructor ( "T", [] ) "A" [])
        --                 |> Expect.equal (Err (TypeInputsMismatch "T" { got = [], expected = [ IntT ] }))
        --     --
        --     , test "type T; T.A -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> addType ( "T", [] ) Dict.empty
        --                 |> typeOf (Constructor ( "T", [] ) "A" [])
        --                 |> Expect.equal (Ok (NameT ( "T", [] )))
        --     ]
        -- -- Call
        -- , describe "Call"
        --     [ test "1 2 -- CallNonFunction" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Call (Integer 1) (Integer 2))
        --                 |> Expect.equal (Err (CallNonFunction (Integer 1) (Integer 2)))
        --     --
        --     , test "(X -> Number) 1.1 -- TypeNotFound on input" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Call (Input (LambdaType (NameT "X" []) NumberType)) (Number 1.1))
        --                 |> Expect.equal (Err (TypeNotFound "X"))
        --     --
        --     , test "(Int -> X) 1.1 -- TypeNotFound on output" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Call (Input (LambdaType IntT (NameT "X" []))) (Number 1.1))
        --                 |> Expect.equal (Err (TypeNotFound "X"))
        --     --
        --     , test "(Int -> Number) 1.1 -- TypeMismatch on input" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Call (Input (LambdaType IntT NumberType)) (Number 1.1))
        --                 |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))
        --     --
        --     , test "(Int -> Number) 1 -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (Call (Input (LambdaType IntT NumberType)) (Integer 1))
        --                 |> Expect.equal (Ok NumberType)
        --     --
        --     , test "(Int -> Number -> Type) 1 1.1 -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf
        --                     (Call
        --                         (Call
        --                             (Input (LambdaType IntT (LambdaType NumberType TypeT)))
        --                             (Integer 1)
        --                         )
        --                         (Number 1.1)
        --                     )
        --                 |> Expect.equal (Ok TypeT)
        --     ]
        -- -- CaseOf
        -- , describe "CaseOf"
        --     [ test "match 1 to X of -- TypeNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (CaseOf ( Integer 1, (NameT "X" []) ) [])
        --                 |> Expect.equal (Err (TypeNotFound "X"))
        --     --
        --     , test "case 1 -> Number of -- ok" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf (CaseOf ( Integer 1, NumberType ) [])
        --                 |> Expect.equal (Ok NumberType)
        --     --
        --     , test "match 1 to Number of 1.1 -> 1.1 -- PatternMismatch" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf
        --                     (CaseOf ( Integer 1, NumberType )
        --                         [ ( NumberPattern 1.1, Number 1.1 ) ]
        --                     )
        --                 |> Expect.equal (Err (PatternMismatch (NumberPattern 1.1) IntT))
        --     --
        --     , test "match 1 to Number of 1 -> x -- NameNotFound" <|
        --         \_ ->
        --             FVM.Module.new
        --                 |> typeOf
        --                     (CaseOf ( Integer 1, NumberType )
        --                         [ ( IntPattern 1, Load "x" ) ]
        --                     )
        --                 |> Expect.equal (Err (NameNotFound "x"))
        --     ]
        ]
