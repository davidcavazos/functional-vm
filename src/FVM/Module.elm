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


withName : String -> Expression -> Module -> Module
withName name value m =
    -- Note: this does not check for existing names
    { m | names = Dict.insert name value m.names }



-- WITH TYPE


withType : ( String, List Type ) -> Dict String ( List ( String, Type ), List Expression ) -> Module -> Module
withType ( typeName, typeInputTypes ) constructors m =
    -- Note: this does not check for existing types
    let
        ctors =
            Dict.map
                (\_ ( namedTs, _ ) -> List.map Tuple.second namedTs)
                constructors
    in
    Dict.foldl
        (\name inputTypes -> withTypeConstructor typeName name inputTypes)
        { m | types = Dict.insert typeName ( typeInputTypes, ctors ) m.types }
        constructors


withTypeConstructor : String -> String -> ( List ( String, Type ), List Expression ) -> Module -> Module
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
