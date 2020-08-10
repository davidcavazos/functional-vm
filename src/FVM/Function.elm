module FVM.Function exposing
    ( mapFunction
    , mapFunctionType
    )

import FVM exposing (Expression(..), Type(..))



-- MAP FUNCTION


mapFunction : (List Expression -> Expression -> a) -> Expression -> a
mapFunction f expression =
    Debug.todo "mapFunction"


funcExpr : Expression -> ( List Expression, Expression )
funcExpr expression =
    Debug.todo "funcExpr"



-- MAP FUNCTION TYPE


mapFunctionType : (List Type -> Type -> a) -> Type -> a
mapFunctionType f typ =
    let
        ( inputsT, outputsT ) =
            getInputTypes [] typ
    in
    f inputsT outputsT


getInputTypes : List Type -> Type -> ( List Type, Type )
getInputTypes inputsT typ =
    case typ of
        LambdaT inputT outputT ->
            getInputTypes (inputsT ++ [ inputT ]) outputT

        _ ->
            ( inputsT, typ )
