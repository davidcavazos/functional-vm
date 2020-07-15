module FVM.Expression exposing (evaluate)

import Dict
import FVM exposing (Context, Error(..), Expression(..), Module, Type(..))
import FVM.Context exposing (addVariable, getVariable)
import FVM.Type exposing (checkNamedType, checkType, typecheck)
import FVM.Util exposing (mapDict, mapList, zip2)
import Result



-- EVALUATE


evaluate : Expression -> Context -> Module -> Result Error Expression
evaluate expression ctx m =
    case expression of
        Type typ ->
            Result.map Type (checkType typ ctx m)

        Integer _ ->
            Ok expression

        Number _ ->
            Ok expression

        Variable name _ ->
            Result.andThen
                (\typ -> typecheck ( expression, typ ) ctx m)
                (getVariable name ctx)

        Lambda ( inputName, rawInputType ) output ->
            Result.andThen
                (\inputType ->
                    Result.map (Lambda ( inputName, inputType ))
                        (evaluate output
                            (addVariable inputName rawInputType ctx)
                            m
                        )
                )
                (checkType rawInputType ctx m)

        Tuple items ->
            Result.map Tuple
                (mapList evaluate items ctx m)

        Record namedItems ->
            Result.map Record
                (mapDict (\_ -> evaluate) namedItems ctx m)

        Constructor rawNamedType name inputs ->
            checkNamedType rawNamedType
                (\namedType ctors ->
                    case Dict.get name ctors of
                        Just inputTypes ->
                            if List.length inputs /= List.length inputTypes then
                                Err (ConstructorInputsMismatch namedType name { got = inputs, expected = inputTypes })

                            else
                                Result.map (Constructor namedType name)
                                    (mapList typecheck (zip2 inputs inputTypes) ctx m)

                        Nothing ->
                            Err (ConstructorNotFound namedType name)
                )
                ctx
                m

        Call rawFunction rawInput ->
            Result.andThen
                (\function ->
                    case function of
                        Variable _ (LambdaType inputType outputType) ->
                            Result.map
                                (\input ->
                                    input
                                )
                                (typecheck ( rawInput, inputType ) ctx m)

                        Lambda ( inputName, inputType ) rawOutput ->
                            Debug.todo "compile Call Lambda"

                        Call _ _ ->
                            Debug.todo "compile Call Call"

                        _ ->
                            Err (CallNonFunction function rawInput)
                )
                (evaluate rawFunction ctx m)

        Match input cases outputType ->
            Debug.todo "evaluate Match"
