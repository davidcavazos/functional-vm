module Build.Python.PyTypeDefTest exposing (suite)

import ASM exposing (Accessor(..), Condition(..), Expression(..), Type(..))
import Dict
import Expect
import FVM.Build.Python exposing (pyTypeDef)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "build Python -- pyTypeDef"
        --
        [ describe "Tagged union type"
            [ test "simple type -- type T" <|
                \_ ->
                    pyTypeDef ( "T", [] ) Dict.empty
                        |> Expect.equal
                            { imports = Set.empty
                            , init = [ "class T:pass" ]
                            }

            --
            , test "type inputs -- type T Int a Number b" <|
                \_ ->
                    pyTypeDef ( "T", [ IntT, GenericT "a", NumberT, GenericT "b" ] ) Dict.empty
                        |> Expect.equal
                            { imports = Set.empty
                            , init = [ "class T(a,b):pass" ]
                            }

            --
            , test "simple constructor -- type T = A" <|
                \_ ->
                    pyTypeDef ( "T", [] ) (Dict.singleton "A" [])
                        |> Expect.equal
                            { imports = Set.empty
                            , init =
                                [ "class T:pass"
                                , "@dataclass"
                                , "class A(T):"
                                , "  pass"
                                ]
                            }

            --
            , test "constructors with inputs -- type T = A Int | B Int Number" <|
                \_ ->
                    pyTypeDef ( "T", [] )
                        (Dict.fromList
                            [ ( "A", [ IntT ] )
                            , ( "B", [ IntT, NumberT ] )
                            ]
                        )
                        |> Expect.equal
                            { imports = Set.empty
                            , init =
                                [ "class T:pass"
                                , "@dataclass"
                                , "class A(T):"
                                , "  _1:Int"
                                , "@dataclass"
                                , "class B(T):"
                                , "  _1:Int"
                                , "  _2:Number"
                                ]
                            }
            ]
        ]
