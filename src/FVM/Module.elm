module FVM.Module exposing
    ( new
    , withName
    , withType
    )

import Dict exposing (Dict)
import FVM exposing (Error(..), Expression(..), Module, Type)



-- NEW


new : Module
new =
    { types = Dict.empty
    , names = Dict.empty
    }



-- WITH NAME


withName : String -> Expression -> Module -> Result Error Module
withName name value m =
    case Dict.get name m.names of
        Just existing ->
            Err (NameAlreadyExists name { got = value, existing = existing })

        Nothing ->
            Ok { m | names = Dict.insert name value m.names }



-- WITH TYPE


withType : ( String, List Type ) -> Dict String ( List ( String, Type ), List Expression ) -> Module -> Result Error Module
withType ( typeName, typeInputTypes ) constructors m =
    let
        ctors =
            Dict.map
                (\_ ( namedTs, _ ) -> List.map Tuple.second namedTs)
                constructors
    in
    case Dict.get typeName m.types of
        Just tdef ->
            Err (TypeAlreadyExists typeName { got = ( typeInputTypes, ctors ), existing = tdef })

        Nothing ->
            Dict.foldl
                (\name inputTypes -> Result.andThen (withTypeConstructor typeName name inputTypes))
                (Ok { m | types = Dict.insert typeName ( typeInputTypes, ctors ) m.types })
                constructors


withTypeConstructor : String -> String -> ( List ( String, Type ), List Expression ) -> Module -> Result Error Module
withTypeConstructor typeName name ( namedInputTypes, typeInputs ) m =
    let
        ctorInputs =
            List.map (\( n, _ ) -> Load n) namedInputTypes

        ctor =
            List.foldr Lambda
                (Constructor ( typeName, typeInputs ) name ctorInputs)
                namedInputTypes
    in
    withName name ctor m
