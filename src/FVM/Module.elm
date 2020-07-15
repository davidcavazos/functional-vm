module FVM.Module exposing
    ( addName
    , addType
    , getName
    , getType
    , hasName
    , hasType
    , new
    , removeName
    , removeType
    )

import Dict exposing (Dict)
import FVM exposing (Error(..), Expression(..), Module, Type(..), TypeDefinition)



-- NEW


new : Module
new =
    { types = Dict.empty
    , values = Dict.empty
    }



-- TYPES


addType : ( String, List Type ) -> Dict String ( List ( String, Type ), List Expression ) -> Module -> Module
addType ( typeName, typeInputTypes ) constructors m =
    let
        ctors =
            Dict.map
                (\_ ( namedTypes, _ ) -> List.map Tuple.second namedTypes)
                constructors
    in
    Dict.foldl
        (addTypeConstructor typeName)
        { m
            | types =
                Dict.insert typeName
                    (Ok ( typeInputTypes, ctors ))
                    m.types
        }
        constructors


addTypeConstructor : String -> String -> ( List ( String, Type ), List Expression ) -> Module -> Module
addTypeConstructor typeName name ( namedInputTypes, typeInputs ) m =
    let
        ctorInputs =
            List.map (\( n, t ) -> Variable n t) namedInputTypes

        ctor =
            List.foldr
                Lambda
                (Constructor ( typeName, typeInputs ) name ctorInputs)
                namedInputTypes
    in
    addName name ctor m


removeType : String -> Module -> Module
removeType typeName m =
    { m | types = Dict.remove typeName m.types }


getType : String -> Module -> Result Error TypeDefinition
getType typeName m =
    case Dict.get typeName m.types of
        Just (Ok tdef) ->
            Ok tdef

        Just (Err e) ->
            Err e

        Nothing ->
            Err (TypeNotFound typeName)


hasType : String -> Module -> Bool
hasType typeName m =
    Dict.member typeName m.types



-- NAMES


addName : String -> Expression -> Module -> Module
addName name value m =
    case Dict.get name m.values of
        Just (Ok existing) ->
            { m
                | values =
                    Dict.insert name
                        (Err (NameAlreadyExists name { got = value, existing = existing }))
                        m.values
            }

        Just (Err _) ->
            m

        Nothing ->
            { m | values = Dict.insert name (Ok value) m.values }


removeName : String -> Module -> Module
removeName name m =
    { m | values = Dict.remove name m.values }


getName : String -> Module -> Result Error Expression
getName name m =
    case Dict.get name m.values of
        Just (Ok value) ->
            Ok value

        Just (Err e) ->
            Err e

        Nothing ->
            Err (NameNotFound name)


hasName : String -> Module -> Bool
hasName name m =
    Dict.member name m.values
