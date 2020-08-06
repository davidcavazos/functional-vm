module Validate.TypecheckPTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..), new)
import FVM.Validate exposing (typecheckP)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typecheckP"
        [ test "(_ : X) : Int -- TypeNotFound -- check pattern" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP (NameT "X" [])) IntT
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "(_ : Int) : X -- TypeNotFound -- check type" <|
            \_ ->
                FVM.new
                    |> typecheckP (AnyP IntT) (NameT "X" [])
                    |> Expect.equal (Err (TypeNotFound "X"))

        --
        , test "(_ : Int) : Number -- PatternMismatch -- pattern not equal" <|
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

        -- RecordP
        , test "{} : {} -- ok" <|
            \_ ->
                FVM.new
                    |> typecheckP (RecordP Dict.empty) (RecordT Dict.empty)
                    |> Expect.equal (Ok (RecordP Dict.empty))

        --
        , test "{x : Int} : {} -- PatternMismatch -- missing field" <|
            \_ ->
                FVM.new
                    |> typecheckP (RecordP (Dict.singleton "x" IntT)) (RecordT Dict.empty)
                    |> Expect.equal (Err (PatternMismatch (RecordP (Dict.singleton "x" IntT)) (RecordT Dict.empty)))

        --
        , test "{x : Int} : {x : Number} -- PatternMismatch -- field with type mismatch" <|
            \_ ->
                FVM.new
                    |> typecheckP (RecordP (Dict.singleton "x" IntT)) (RecordT (Dict.singleton "x" NumberT))
                    |> Expect.equal (Err (PatternMismatch (RecordP (Dict.singleton "x" IntT)) (RecordT (Dict.singleton "x" NumberT))))

        --
        , test "{} : {x : Int} -- ok" <|
            \_ ->
                FVM.new
                    |> typecheckP (RecordP Dict.empty) (RecordT (Dict.singleton "x" IntT))
                    |> Expect.equal (Ok (RecordP Dict.empty))
        ]
