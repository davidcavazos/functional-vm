module FVM.Module exposing
    ( addName
    , addType
    , getName
    , getTypeDefinition
    , new
    , withGeneric
    )

import Dict exposing (Dict)
import FVM exposing (Error(..), Expression(..), Module, Pattern(..), Type(..), TypeDefinition)



-- NEW


new : Module
new =
    { types = Dict.empty
    , generics = Dict.empty
    , names = Dict.empty
    }



-- TYPES


addType : ( String, List Type ) -> Dict String ( List ( String, Type ), List Expression ) -> Module -> Result Error Module
addType ( typeName, typeInputTypes ) constructors m =
    let
        ctors =
            Dict.map
                (\_ ( namedTypes, _ ) -> List.map Tuple.second namedTypes)
                constructors
    in
    Dict.foldl
        (\name inputTypes -> Result.andThen (addTypeConstructor typeName name inputTypes))
        (Ok { m | types = Dict.insert typeName ( typeInputTypes, ctors ) m.types })
        constructors


addTypeConstructor : String -> String -> ( List ( String, Type ), List Expression ) -> Module -> Result Error Module
addTypeConstructor typeName name ( namedInputTypes, typeInputs ) m =
    let
        ctorInputs =
            List.map (\( n, _ ) -> Load n) namedInputTypes

        ctor =
            List.foldr Lambda
                (Constructor ( typeName, typeInputs ) name ctorInputs)
                namedInputTypes
    in
    addName name ctor m


getTypeDefinition : String -> Module -> Result Error TypeDefinition
getTypeDefinition typeName m =
    case Dict.get typeName m.types of
        Just tdef ->
            Ok tdef

        Nothing ->
            Err (TypeNotFound typeName)


withGeneric : String -> Type -> Module -> Module
withGeneric name typ m =
    { m | generics = Dict.insert name typ m.generics }



-- NAMES


addName : String -> Expression -> Module -> Result Error Module
addName name value m =
    case Dict.get name m.names of
        Just existing ->
            Err (NameAlreadyExists name { got = value, existing = existing })

        Nothing ->
            Ok { m | names = Dict.insert name value m.names }


getName : String -> Module -> Result Error Expression
getName name m =
    case Dict.get name m.names of
        Just value ->
            Ok value

        Nothing ->
            Err (NameNotFound name)
