module LoadInputTest exposing (suite)

import Expect
import FVM exposing (Expression(..), Type(..), load, new, saveInput)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "load Input"
        [ test "defined input" <|
            \_ ->
                new
                    |> saveInput "x" IntType
                    |> load "x"
                    |> dump
                    |> Expect.equal "V x=(x:Int);R (x:Int)"
        ]
