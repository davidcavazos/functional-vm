module Build.BuildTest exposing (suite)

import Dict
import Expect
import FVM exposing (Expression(..), Type(..))
import FVM.Build exposing (build)
import FVM.Package exposing (letName)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        testBuild =
            build
                { typeType = "Type"
                , intType = "Int"
                , numberType = "Number"
                , tupleType = \itemsT -> "(" ++ ")"
                , recordType = \fieldsT -> "{" ++ "}"
                , functionType = \inputsT outputT -> "->"
                , genericType = \name -> "Generic " ++ name
                , typeDef = \( typeName, typeInputs ) constructors -> "type " ++ typeName
                , namedExpression = \name value -> "let " ++ name
                }
                Basics.identity
    in
    describe "build"
        -- find types used
        [ describe "find types used"
            [ test "empty package" <|
                \_ ->
                    testBuild { types = Dict.empty, names = Dict.empty }
                        |> Expect.equal (Ok [])
            ]
        ]
