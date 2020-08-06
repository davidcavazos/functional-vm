module Validate.ValidateTest exposing (suite)

import Dict
import Expect
import FVM exposing (Error(..), Expression(..), Pattern(..), Type(..))
import FVM.Package exposing (letName, letType)
import FVM.Validate exposing (validate)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "validate"
        -- Name definitions
        [ describe "names"
            [ test "x = X -- TypeNotFound -- check value" <|
                \_ ->
                    FVM.Package.new
                        |> letName "x" (Type (NameT "X" []))
                        |> validate
                        |> Expect.equal
                            (Err
                                { types = Dict.empty
                                , names = Dict.singleton "x" (TypeNotFound "X")
                                }
                            )
            ]

        -- Type definitions
        , describe "types"
            [ test "type T -- ok" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [] ) Dict.empty
                        |> validate
                        |> Expect.equal
                            (Ok
                                { types = Dict.singleton "T" ( [], Dict.empty )
                                , names = Dict.empty
                                }
                            )

            --
            , test "type T X -- TypeNotFound -- checkT type inputs" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [ NameT "X" [] ] ) Dict.empty
                        |> validate
                        |> Expect.equal
                            (Err
                                { types = Dict.singleton "T" (TypeNotFound "X")
                                , names = Dict.empty
                                }
                            )

            --
            , test "type T X; type X -- ok" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [ NameT "X" [] ] ) Dict.empty
                        |> letType ( "X", [] ) Dict.empty
                        |> validate
                        |> Expect.equal
                            (Ok
                                { types =
                                    Dict.fromList
                                        [ ( "T", ( [ NameT "X" [] ], Dict.empty ) )
                                        , ( "X", ( [], Dict.empty ) )
                                        ]
                                , names = Dict.empty
                                }
                            )

            --
            , test "type T Int -- ok" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [ IntT ] ) Dict.empty
                        |> validate
                        |> Expect.equal
                            (Ok
                                { types = Dict.singleton "T" ( [ IntT ], Dict.empty )
                                , names = Dict.empty
                                }
                            )

            --
            , test "type T = A -- ok" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [] ) (Dict.singleton "A" ( [], [] ))
                        |> validate
                        |> Expect.equal
                            (Ok
                                { types = Dict.singleton "T" ( [], Dict.singleton "A" [] )
                                , names = Dict.singleton "A" (Constructor ( "T", [] ) "A" [])
                                }
                            )

            --
            , test "type T = A (x : X) -- TypeNotFound -- checkT constructor input" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", NameT "X" [] ) ], [] ))
                        |> validate
                        |> Expect.equal
                            (Err
                                { types = Dict.singleton "T" (TypeNotFound "X")
                                , names = Dict.singleton "A" (TypeNotFound "X")
                                }
                            )

            --
            , test "type T = A (x : Int) -- ok" <|
                \_ ->
                    FVM.Package.new
                        |> letType ( "T", [] ) (Dict.singleton "A" ( [ ( "x", IntT ) ], [] ))
                        |> validate
                        |> Expect.equal
                            (Ok
                                { types = Dict.singleton "T" ( [], Dict.singleton "A" [ IntT ] )
                                , names = Dict.singleton "A" (Lambda ( "x", IntT ) (Constructor ( "T", [] ) "A" [ Load "x" ]))
                                }
                            )
            ]
        ]
