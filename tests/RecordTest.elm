module RecordTest exposing (suite)

import Dict
import Expect
import FVM exposing (int, load, new, record)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "record"
        [ test "0 items" <|
            \_ ->
                new
                    |> record Dict.empty
                    |> dump
                    |> Expect.equal "R {}"

        --
        , test "NameNotFound with 1 item" <|
            \_ ->
                new
                    |> record (Dict.fromList [ ( "x", load "x" ) ])
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "1 item" <|
            \_ ->
                new
                    |> record (Dict.fromList [ ( "x", int 1 ) ])
                    |> dump
                    |> Expect.equal "R {x=1}"

        --
        , test "3 items" <|
            \_ ->
                new
                    |> record
                        (Dict.fromList
                            [ ( "x", int 1 ), ( "y", int 2 ), ( "z", int 3 ) ]
                        )
                    |> dump
                    |> Expect.equal "R {x=1,y=2,z=3}"
        ]
