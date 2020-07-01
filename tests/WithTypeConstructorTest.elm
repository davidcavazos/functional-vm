module WithTypeConstructorTest exposing (suite)

import AST exposing (..)
import Bitcode exposing (dump)
import Context exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Context withTypeConstructor"
        [ test "TypeNotFound" <|
            \_ ->
                new
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "E TypeNotFound T"

        --
        , test "after no result" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T=A;N A=T.A"

        --
        , test "after Ok" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withResult (Ok (Integer 1))
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T=A;N A=T.A;R 1"

        --
        , test "after Err" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withResult (Err (NameNotFound "x"))
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T;E NameNotFound x"

        --
        , test "NameAlreadyExists duplicated constructor" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> dump
                    |> Expect.equal "T T=A;N A=T.A;E NameAlreadyExists A"

        --
        , test "many constructors" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withTypeConstructor ( "T", [] ) "A" []
                    |> withTypeConstructor ( "T", [] ) "B" []
                    |> withTypeConstructor ( "T", [] ) "C" []
                    |> dump
                    |> Expect.equal "T T=A|B|C;N A=T.A;N B=T.B;N C=T.C"

        --
        , test "type checking" <|
            \_ ->
                new
                    |> withType "T" []
                    |> withTypeConstructor ( "T", [ int 1 ] ) "A" []
                    |> dump
                    |> Expect.equal "T T;E TypeInputsMismatch T"
        ]
