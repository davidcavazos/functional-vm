module GetTypeTest exposing (suite)

import AST exposing (..)
import Context exposing (..)
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "getType"
        [ -- Type
          test "with Type" <|
            \_ ->
                getType (Type IntType)
                    |> Expect.equal TypeType

        -- Integer
        , test "with Integer" <|
            \_ ->
                getType (Integer 1)
                    |> Expect.equal IntType

        -- Number
        , test "with Number" <|
            \_ ->
                getType (Number 3.14)
                    |> Expect.equal NumberType

        -- Tuple
        , test "with Tuple empty" <|
            \_ ->
                getType (Tuple [])
                    |> Expect.equal (TupleType [])

        --
        , test "with Tuple with items" <|
            \_ ->
                getType (Tuple [ Integer 1, Number 3.14 ])
                    |> Expect.equal (TupleType [ IntType, NumberType ])

        -- Record
        , test "with Record empty" <|
            \_ ->
                getType (Record Dict.empty)
                    |> Expect.equal (RecordType Dict.empty)

        --
        , test "with Record with items" <|
            \_ ->
                getType (Record (Dict.fromList [ ( "x", Integer 1 ), ( "y", Number 3.14 ) ]))
                    |> Expect.equal (RecordType (Dict.fromList [ ( "x", IntType ), ( "y", NumberType ) ]))

        -- Input
        , test "with Input" <|
            \_ ->
                getType (Input "x" IntType)
                    |> Expect.equal IntType

        -- Lambda
        , test "with Lambda" <|
            \_ ->
                getType (Lambda ( "x", IntType ) (Integer 1))
                    |> Expect.equal (LambdaType IntType IntType)

        -- Constructor
        , test "with Constructor" <|
            \_ ->
                getType (Constructor ( "T", [] ) "A" [])
                    |> Expect.equal (NamedType "T" [])
        ]
