module FVM.Compile exposing
    ( checkTypeDefinition
    , compile
    )

import Dict
import FVM exposing (Context, Error(..), Expression(..), Module, Type(..), TypeDefinition)
import FVM.Context exposing (addVariable, getGeneric, getVariable)
import FVM.Expression exposing (evaluate)
import FVM.Module exposing (getType)
import FVM.Type exposing (checkType)
import FVM.Util exposing (mapDict, mapList)
import Result



-- COMPILE


compile : Module -> Module
compile mod =
    let
        check : (a -> Context -> Module -> Result Error a) -> Module -> String -> Result Error a -> Result Error a
        check f m name result =
            Result.andThen (\x -> f x FVM.Context.new m) result
    in
    mod
        |> (\m -> { m | types = Dict.map (check checkTypeDefinition m) m.types })
        |> (\m -> { m | values = Dict.map (check evaluate m) m.values })



-- CHECK TYPE DEFINITION


checkTypeDefinition : TypeDefinition -> Context -> Module -> Result Error TypeDefinition
checkTypeDefinition ( typeInputTypes, constructors ) ctx m =
    Result.map2 (\typeInputs ctors -> ( typeInputs, ctors ))
        (mapList checkType typeInputTypes ctx m)
        (mapDict (\_ -> mapList checkType) constructors ctx m)
