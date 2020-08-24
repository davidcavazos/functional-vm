module Build.Python.BuildPythonTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Build.Python exposing (buildPython)
import FVM.Package exposing (letName)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "build Python -- buildPython"
        -- base cases
        [ describe "base cases"
            [ test "empty package" <|
                \_ ->
                    buildPython FVM.Package.new
                        |> Expect.equal (Ok "#!/usr/bin/env python3")
            ]

        -- -- type definitions
        -- , describe "type definitions"
        --     --
        --     [ test "TypeT" <|
        --         \_ ->
        --             FVM.Package.new
        --                 |> letName "x" (Type TypeT)
        --                 |> buildPython
        --                 |> Expect.equal
        --                     (Ok
        --                         (String.join "\n"
        --                             [ "#!/usr/bin/env python3"
        --                             , "class Type:pass"
        --                             ]
        --                         )
        --                     )
        --     ]
        ]
