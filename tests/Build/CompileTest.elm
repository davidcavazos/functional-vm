module Build.CompileTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Build exposing (compile)
import FVM.Package
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "compile"
        -- type definitions
        [ describe "type definitions"
            [ test "empty package" <|
                \_ ->
                    compile FVM.Package.new
                        |> Expect.equal
                            (Ok
                                { types = Dict.empty
                                , names = Dict.empty
                                }
                            )
            ]
        ]
