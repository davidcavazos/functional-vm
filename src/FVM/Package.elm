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
letName name value pkg =
    -- Note: this does not check for existing names
    { pkg | names = Dict.insert name value pkg.names }



-- WITH TYPE


letType : ( String, List Type ) -> Dict String ( List ( String, Type ), List Expression ) -> Package -> Package
letType ( typeName, typeInputTypes ) constructors pkg =
    -- Note: this does not check for existing types
    let
        ctors =
            Dict.map
                (\_ ( namedTs, _ ) -> List.map Tuple.second namedTs)
                constructors
    in
    Dict.foldl
        (\name inputTypes -> letTypeConstructor typeName name inputTypes)
        { pkg | types = Dict.insert typeName ( typeInputTypes, ctors ) pkg.types }
        constructors


letTypeConstructor : String -> String -> ( List ( String, Type ), List Expression ) -> Package -> Package
letTypeConstructor typeName name ( namedInputTypes, typeInputs ) pkg =
    let
        ctorInputs =
            List.map (\( n, _ ) -> Load n) namedInputTypes

        ctor =
            List.foldr Lambda
                (Constructor ( typeName, typeInputs ) name ctorInputs)
                namedInputTypes
    in
    letName name ctor pkg
