module FVM.Context exposing
    ( addVariable
    , bindGeneric
    , getGeneric
    , getVariable
    , new
    )

import Dict
import FVM exposing (Context, Error(..), Expression(..), Type(..))



-- NEW


new : Context
new =
    { generics = Dict.empty
    , variables = Dict.empty
    }



-- GENERICS


bindGeneric : String -> Type -> Context -> Context
bindGeneric name typ ctx =
    { ctx | generics = Dict.insert name typ ctx.generics }


getGeneric : String -> Context -> Type
getGeneric name ctx =
    case Dict.get name ctx.generics of
        Just typ ->
            typ

        Nothing ->
            GenericType name



-- VARIABLES


addVariable : String -> Type -> Context -> Context
addVariable name typ ctx =
    { ctx | variables = Dict.insert name typ ctx.variables }


getVariable : String -> Context -> Result Error Type
getVariable name ctx =
    case Dict.get name ctx.variables of
        Just typ ->
            Ok typ

        Nothing ->
            Err (NameNotFound name)
