module SavePatternTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Pattern(..), Type(..), int, new, withPattern)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "withPattern"
        [ test "MatchPatternTypeMismatch" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (IntPattern 1) (Number 3.14)
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch 3.14"

        -- TypePattern with Type
        , test "TypePattern with Type, equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (TypePattern IntType) (Type IntType)
                    |> dump
                    |> Expect.equal "R Int"

        --
        , test "TypePattern with Type, not equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (TypePattern IntType) (Type NumberType)
                    |> dump
                    |> Expect.equal ""

        -- IntPattern with Integer
        , test "IntPattern with Integer, equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (IntPattern 2) (Integer 2)
                    |> dump
                    |> Expect.equal "R 2"

        --
        , test "IntPattern with Integer, not equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (IntPattern 2) (Integer 0)
                    |> dump
                    |> Expect.equal ""

        -- NumberPattern with Number
        , test "NumberPattern with Number, equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (NumberPattern 3.14) (Number 3.14)
                    |> dump
                    |> Expect.equal "R 3.14"

        --
        , test "NumberPattern with Number, not equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (NumberPattern 3.14) (Number 2.71)
                    |> dump
                    |> Expect.equal ""

        -- TuplePattern with Tuple
        , test "TuplePattern with Tuple, different lengths" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (TuplePattern [ IntPattern 2 ] Nothing)
                        (Tuple [])
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch ()"

        --
        , test "TuplePattern with Tuple, empty, without name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (TuplePattern [] Nothing) (Tuple [])
                    |> dump
                    |> Expect.equal "R ()"

        --
        , test "TuplePattern with Tuple, empty, with name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (TuplePattern [] (Just "x")) (Tuple [])
                    |> dump
                    |> Expect.equal "V x=();R ()"

        --
        , test "TuplePattern with Tuple, two items, first different" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (TuplePattern [ IntPattern 2, IntPattern 3 ] (Just "x"))
                        (Tuple [ Integer 0, Integer 3 ])
                    |> dump
                    |> Expect.equal ""

        --
        , test "TuplePattern with Tuple, two items, second different" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (TuplePattern [ IntPattern 2, IntPattern 3 ] (Just "x"))
                        (Tuple [ Integer 2, Integer 0 ])
                    |> dump
                    |> Expect.equal ""

        --
        , test "TuplePattern with Tuple, two items, equal" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (TuplePattern [ IntPattern 2, IntPattern 3 ] (Just "x"))
                        (Tuple [ Integer 2, Integer 3 ])
                    |> dump
                    |> Expect.equal "V x=(2,3);R (2,3)"

        -- RecordPattern with Record
        , test "RecordPattern with Record, both empty, without name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (RecordPattern Dict.empty Nothing)
                        (Record Dict.empty)
                    |> dump
                    |> Expect.equal "R {}"

        --
        , test "RecordPattern with Record, both empty, with name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (RecordPattern Dict.empty (Just "x"))
                        (Record Dict.empty)
                    |> dump
                    |> Expect.equal "V x={};R {}"

        --
        , test "RecordPattern with Record, pattern empty, value not empty" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern (RecordPattern Dict.empty (Just "x"))
                        (Record (Dict.fromList [ ( "a", Integer 2 ) ]))
                    |> dump
                    |> Expect.equal "V x={a=2};R {a=2}"

        --
        , test "RecordPattern with Record, pattern with item name not found" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (RecordPattern
                            (Dict.fromList [ ( "a", IntPattern 2 ) ])
                            (Just "x")
                        )
                        (Record Dict.empty)
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch {}"

        --
        , test "RecordPattern with Record, pattern with item type mismatch" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (RecordPattern
                            (Dict.fromList [ ( "a", IntPattern 2 ) ])
                            (Just "x")
                        )
                        (Record (Dict.fromList [ ( "a", Number 3.14 ) ]))
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch 3.14"

        --
        , test "RecordPattern with Record, one item different" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (RecordPattern
                            (Dict.fromList [ ( "a", IntPattern 2 ) ])
                            (Just "x")
                        )
                        (Record (Dict.fromList [ ( "a", Integer 3 ) ]))
                    |> dump
                    |> Expect.equal ""

        --
        , test "RecordPattern with Record, one item equal, without name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (RecordPattern
                            (Dict.fromList [ ( "a", IntPattern 2 ) ])
                            Nothing
                        )
                        (Record (Dict.fromList [ ( "a", Integer 2 ), ( "b", Integer 3 ) ]))
                    |> dump
                    |> Expect.equal "V a=2;R {a=2,b=3}"

        --
        , test "RecordPattern with Record, one item equal, with name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (RecordPattern
                            (Dict.fromList [ ( "a", IntPattern 2 ) ])
                            (Just "x")
                        )
                        (Record (Dict.fromList [ ( "a", Integer 2 ), ( "b", Integer 3 ) ]))
                    |> dump
                    |> Expect.equal "V a=2;V x={a=2,b=3};R {a=2,b=3}"

        -- ConstructorPattern
        , test "ConstructorPattern with Constructor, type name mismatch" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [] Nothing)
                        (Constructor ( "X", [] ) "A" [])
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch X.A"

        --
        , test "ConstructorPattern with Constructor, type inputs mismatch" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (ConstructorPattern ( "T", [ Integer 1 ] ) "A" [] Nothing)
                        (Constructor ( "T", [ Integer 2 ] ) "A" [])
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch (T 2).A"

        --
        , test "ConstructorPattern with Constructor, constructor name mismatch" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [] Nothing)
                        (Constructor ( "T", [] ) "B" [])
                    |> dump
                    |> Expect.equal "E MatchPatternTypeMismatch T.B"

        --
        , test "ConstructorPattern with Constructor, type not found" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [] Nothing)
                        (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "E TypeNotFound T"

        -- --
        -- , test "ConstructorPattern with Constructor, constructor not found" <|
        --     \_ ->
        --         new
        --             |> int 1
        --             |> saveTaggedUnionType ( "T", [] ) Dict.empty
        --             |> withPattern
        --                 (ConstructorPattern ( "T", [] ) "A" [] Nothing)
        --                 (Constructor ( "T", [] ) "A" [])
        --             |> dump
        --             |> Expect.equal "E MatchConstructorNotFound"
        ]
