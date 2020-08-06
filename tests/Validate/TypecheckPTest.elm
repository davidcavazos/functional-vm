module Validate.TypecheckPTest exposing (suite)

import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..), new)
import FVM.Validate exposing (typecheckP)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typecheckP"
        [ test "(_ : X) : Int -- TypeNotFound" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP (NameT "X" [])) IntT
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "(_ : Int) : X -- TypeNotFound" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP IntT) (NameT "X" [])
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "(_ : Int) : Number -- PatternMismatch" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP IntT) NumberT
                    |> Expect.equal (Err (PatternMismatch (AnyP IntT) NumberT))

        --
        , test "(_ : Int) : Int -- ok" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP IntT) IntT
                    |> Expect.equal (Ok (AnyP IntT))

        --
        , test "(_ : Int) : Type | Number -- PatternMismatch" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP IntT) (UnionT [ TypeT, NumberT ])
                    |> Expect.equal (Err (PatternMismatch (AnyP IntT) (UnionT [ TypeT, NumberT ])))

        -- --
        -- , test "(_ : Int) : Int | Number -- ok" <|
        --     \_ ->
        --         FVM.new
        --             |> typecheckP (AnyP IntT) (UnionT [ IntT, NumberT ])
        --             |> Expect.equal (Ok (AnyP IntT))
        ]
