module FVM.Package exposing
    ( letName
    , letType
    , new
    )

import Dict exposing (Dict)
import FVM exposing (Error(..), Expression(..), Package, Type)



-- NEW


new : Package
new =
    { types = Dict.empty
    , names = Dict.empty
    }



-- WITH NAME


letName : String -> Expression -> Package -> Package
letName name value m =
    -- Note: this does not check for existing names
    { m | names = Dict.insert name value m.names }



-- WITH TYPE


letType : ( String, List Type ) -> Dict String ( List ( String, Type ), List Expression ) -> Package -> Package
letType ( typeName, typeInputTypes ) constructors m =
    -- Note: this does not check for existing types
    let
        ctors =
            Dict.map
                (\_ ( namedTs, _ ) -> List.map Tuple.second namedTs)
                constructors
    in
    Dict.foldl
        (\name inputTypes -> letTypeConstructor typeName name inputTypes)
        { m | types = Dict.insert typeName ( typeInputTypes, ctors ) m.types }
        constructors


letTypeConstructor : String -> String -> ( List ( String, Type ), List Expression ) -> Package -> Package
letTypeConstructor typeName name ( namedInputTypes, typeInputs ) m =
    let
        ctorInputs =
            List.map (\( n, _ ) -> Load n) namedInputTypes

        ctor =
            List.foldr Lambda
                (Constructor ( typeName, typeInputs ) name ctorInputs)
                namedInputTypes
    in
    letName name ctor m
