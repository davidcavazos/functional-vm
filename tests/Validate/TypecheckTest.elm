module Validate.TypecheckTest exposing (suite)

import Expect
import FVM exposing (Case(..), Error(..), Expression(..), Pattern(..), Type(..), new)
import FVM.Validate exposing (typecheck)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typecheck"
        -- Validation
        [ describe "validation"
            [ test "check expression -- X : Int -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> typecheck (Type (NameT "X" [])) IntT
                        |> Expect.equal (Err (TypeNotFound "X"))

            --
            , test "check type -- 1 : X -- TypeNotFound" <|
                \_ ->
                    FVM.new
                        |> typecheck (Int 1) (NameT "X" [])
                        |> Expect.equal (Err (TypeNotFound "X"))
            ]

        -- Logic
        , describe "logic"
            [ test "type mismatch -- 1.1 : Int -- TypeMismatch" <|
                \_ ->
                    FVM.new
                        |> typecheck (Number 1.1) IntT
                        |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))

            --
            , test "type match -- 1 : Int -- ok" <|
                \_ ->
                    FVM.new
                        |> typecheck (Int 1) IntT
                        |> Expect.equal (Ok (Int 1))
            ]
        ]
