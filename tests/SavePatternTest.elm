module SavePatternTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Pattern(..), Type(..), int, new, saveTaggedUnionType, withPattern)
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
                    |> Expect.equal "R {a=2,b=3}"

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
                    |> Expect.equal "V x={a=2,b=3};R {a=2,b=3}"

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

        --
        , test "ConstructorPattern with Constructor, constructor not found" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] ) Dict.empty
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [] Nothing)
                        (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "T T;E ConstructorNotFound T A"

        --
        , test "ConstructorPattern with Constructor, different number of inputs" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList [ ( "A", ( [], [] ) ) ])
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [ IntPattern 2 ] Nothing)
                        (Constructor ( "T", [] ) "A" [ Integer 2 ])
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;E ConstructorInputsMismatch T A"

        --
        , test "ConstructorPattern with Constructor, no inputs, without name" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList [ ( "A", ( [], [] ) ) ])
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [] Nothing)
                        (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;R T.A"

        --
        , test "ConstructorPattern with Constructor, no inputs, with name" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList [ ( "A", ( [], [] ) ) ])
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [] (Just "x"))
                        (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;V x=T.A;R T.A"

        --
        , test "ConstructorPattern with Constructor, two inputs, first different" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [ ( "x", IntType ), ( "y", IntType ) ], [] ) ) ]
                        )
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [ IntPattern 2, IntPattern 3 ] (Just "x"))
                        (Constructor ( "T", [] ) "A" [ Integer 0, Integer 3 ])
                    |> dump
                    |> Expect.equal "T T=A Int Int;V A=(x:Int)->(y:Int)->(T.A (x:Int) (y:Int))"

        --
        , test "ConstructorPattern with Constructor, two inputs, second different" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [ ( "x", IntType ), ( "y", IntType ) ], [] ) ) ]
                        )
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [ IntPattern 2, IntPattern 3 ] (Just "x"))
                        (Constructor ( "T", [] ) "A" [ Integer 2, Integer 0 ])
                    |> dump
                    |> Expect.equal "T T=A Int Int;V A=(x:Int)->(y:Int)->(T.A (x:Int) (y:Int))"

        --
        , test "ConstructorPattern with Constructor, two inputs, equal" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList
                            [ ( "A", ( [ ( "x", IntType ), ( "y", IntType ) ], [] ) ) ]
                        )
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [ IntPattern 2, IntPattern 3 ] (Just "x"))
                        (Constructor ( "T", [] ) "A" [ Integer 2, Integer 3 ])
                    |> dump
                    |> Expect.equal "T T=A Int Int;V A=(x:Int)->(y:Int)->(T.A (x:Int) (y:Int));V x=(T.A 2 3);R (T.A 2 3)"

        -- AnythingPattern
        , test "AnythingPattern with Type, without name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (AnythingPattern Nothing)
                        (Type IntType)
                    |> dump
                    |> Expect.equal "R Int"

        --
        , test "AnythingPattern with Type, with name" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (AnythingPattern (Just "x"))
                        (Type IntType)
                    |> dump
                    |> Expect.equal "V x=Int;R Int"

        --
        , test "AnythingPattern with Int" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (AnythingPattern (Just "x"))
                        (Integer 2)
                    |> dump
                    |> Expect.equal "V x=2;R 2"

        --
        , test "AnythingPattern with Number" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (AnythingPattern (Just "x"))
                        (Number 3.14)
                    |> dump
                    |> Expect.equal "V x=3.14;R 3.14"

        --
        , test "AnythingPattern with Tuple" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (AnythingPattern (Just "x"))
                        (Tuple [])
                    |> dump
                    |> Expect.equal "V x=();R ()"

        --
        , test "AnythingPattern with Tuple items" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (TuplePattern [ AnythingPattern (Just "x"), AnythingPattern (Just "y") ] (Just "xy"))
                        (Tuple [ Integer 2, Integer 3 ])
                    |> dump
                    |> Expect.equal "V x=2;V xy=(2,3);V y=3;R (2,3)"

        --
        , test "AnythingPattern with Record" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (AnythingPattern (Just "x"))
                        (Record Dict.empty)
                    |> dump
                    |> Expect.equal "V x={};R {}"

        --
        , test "AnythingPattern with Record items" <|
            \_ ->
                new
                    |> int 1
                    |> withPattern
                        (RecordPattern (Dict.fromList [ ( "a", AnythingPattern (Just "x") ), ( "b", AnythingPattern (Just "y") ) ]) (Just "xy"))
                        (Record (Dict.fromList [ ( "a", Integer 2 ), ( "b", Integer 3 ), ( "c", Integer 4 ) ]))
                    |> dump
                    |> Expect.equal "V x=2;V xy={a=2,b=3,c=4};V y=3;R {a=2,b=3,c=4}"

        --
        , test "AnythingPattern with Constructor" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList [ ( "A", ( [], [] ) ) ])
                    |> withPattern
                        (AnythingPattern (Just "x"))
                        (Constructor ( "T", [] ) "A" [])
                    |> dump
                    |> Expect.equal "T T=A;V A=T.A;V x=T.A;R T.A"

        --
        , test "AnythingPattern with Constructor inputs" <|
            \_ ->
                new
                    |> int 1
                    |> saveTaggedUnionType ( "T", [] )
                        (Dict.fromList [ ( "A", ( [ ( "a", IntType ), ( "b", IntType ) ], [] ) ) ])
                    |> withPattern
                        (ConstructorPattern ( "T", [] ) "A" [ AnythingPattern (Just "x"), AnythingPattern (Just "y") ] (Just "xy"))
                        (Constructor ( "T", [] ) "A" [ Integer 1, Integer 2 ])
                    |> dump
                    |> Expect.equal "T T=A Int Int;V A=(a:Int)->(b:Int)->(T.A (a:Int) (b:Int));V x=1;V xy=(T.A 1 2);V y=2;R (T.A 1 2)"
        ]
