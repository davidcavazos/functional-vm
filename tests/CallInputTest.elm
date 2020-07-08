module CallInputTest exposing (suite)

import Expect
import FVM exposing (Expression(..), Type(..), andThen, call, int, load, new, saveInput)
import FVM.Bitcode exposing (dump)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "call Input"
        [ test "evaluate on empty inputs" <|
            \_ ->
                new
                    |> call [] (Input "x" IntType)
                    |> dump
                    |> Expect.equal "E NameNotFound x"

        --
        , test "0 inputs on a non-function" <|
            \_ ->
                new
                    |> saveInput "x" IntType
                    |> call [] (Input "x" IntType)
                    |> dump
                    |> Expect.equal "V x=(x:Int);R (x:Int)"

        --
        , test "CallNonFunction" <|
            \_ ->
                new
                    |> call [ int 1 ] (Input "x" IntType)
                    |> dump
                    |> Expect.equal "E CallNonFunction (x:Int)"

        --
        , test "TypeMismatch on function type" <|
            \_ ->
                new
                    |> saveInput "f" (LambdaType IntType IntType)
                    |> call [ int 1 ] (Input "f" (LambdaType IntType NumberType))
                    |> dump
                    |> Expect.equal "V f=(f:Int->Int);E TypeMismatch (f:Int->Int) Int->Number"

        --
        , test "1 input" <|
            \_ ->
                new
                    |> saveInput "f" (LambdaType IntType IntType)
                    |> call [ int 1 ] (Input "f" (LambdaType IntType IntType))
                    |> dump
                    |> Expect.equal "V f=(f:Int->Int);R (f:Int->Int) 1"

        --
        , test "CallTooManyInputs with function inputs" <|
            \_ ->
                new
                    |> saveInput "f" (LambdaType IntType IntType)
                    |> call [ int 1, int 2 ]
                        (Input "f" (LambdaType IntType IntType))
                    |> dump
                    |> Expect.equal "V f=(f:Int->Int);E CallTooManyInputs (f:Int->Int)"

        --
        , test "call with too few inputs" <|
            \_ ->
                new
                    |> saveInput "f" (LambdaType IntType (LambdaType NumberType IntType))
                    |> load "f"
                    |> andThen (call [ int 1 ])
                    |> dump
                    |> Expect.equal "V f=(f:Int->Number->Int);R (f:Int->Number->Int) 1"
        ]
