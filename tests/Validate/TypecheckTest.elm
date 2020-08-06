module Validate.TypecheckTest exposing (suite)

import Expect
import FVM exposing (Case(..), Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Package
import FVM.Validate exposing (typecheck)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typecheck"
        [ test "X : Int -- TypeNotFound -- check expression" <|
            \_ ->
                FVM.Package.new
                    |> typecheck (Type (NameT "X" [])) IntT
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "1 : X -- TypeNotFound -- check type" <|
            \_ ->
                FVM.Package.new
                    |> typecheck (Int 1) (NameT "X" [])
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "1.1 : Int -- TypeMismatch -- type not equal" <|
            \_ ->
                FVM.Package.new
                    |> typecheck (Number 1.1) IntT
                    |> Expect.equal (Err (TypeMismatch (Number 1.1) IntT))

        --
        , test "1 : Int -- ok" <|
            \_ ->
                FVM.Package.new
                    |> typecheck (Int 1) IntT
                    |> Expect.equal (Ok (Int 1))
        ]
