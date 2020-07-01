module Bitcode exposing
    ( dump
    , dumpExpression
    , dumpType
    )

import AST exposing (..)
import Context exposing (Context)
import Dict exposing (Dict)


type Prefix
    = TypePrefix
    | NamePrefix
    | InputPrefix
    | GenericPrefix
    | ResultPrefix
    | ErrorPrefix


dumpPrefix : Prefix -> String
dumpPrefix prefix =
    case prefix of
        TypePrefix ->
            "T "

        NamePrefix ->
            "N "

        InputPrefix ->
            "I "

        GenericPrefix ->
            "G "

        ResultPrefix ->
            "R "

        ErrorPrefix ->
            "E "



-- DUMP CONTEXT


dump : Context -> String
dump context =
    String.join ";"
        (List.concat
            [ List.map dumpContextType (Dict.toList context.types)
            , List.map dumpContextName (Dict.toList context.names)
            , List.map dumpContextInput (Dict.toList context.inputs)
            , List.map dumpContextGeneric (Dict.toList context.generics)
            , dumpContextResult context.result
            ]
        )


dumpContextType : ( String, ( List Type, List String ) ) -> String
dumpContextType ( name, ( inputTypes, constructors ) ) =
    let
        dumpMany : String -> (a -> String) -> String -> List a -> String
        dumpMany separator f delimiter items =
            if List.isEmpty items then
                ""

            else
                separator ++ String.join delimiter (List.map f items)
    in
    dumpPrefix TypePrefix
        ++ name
        ++ dumpMany " " dumpType " " inputTypes
        ++ dumpMany "=" (\n -> n) "|" constructors


dumpContextName : ( String, Expression ) -> String
dumpContextName ( name, value ) =
    dumpPrefix NamePrefix ++ name ++ "=" ++ dumpExpression value


dumpContextInput : ( String, Type ) -> String
dumpContextInput ( name, typ ) =
    dumpPrefix InputPrefix ++ name ++ "=" ++ dumpType typ


dumpContextGeneric : ( String, Type ) -> String
dumpContextGeneric ( name, typ ) =
    dumpPrefix GenericPrefix ++ name ++ "=" ++ dumpType typ


dumpContextResult : Result Error Expression -> List String
dumpContextResult result =
    case result of
        Ok x ->
            [ dumpPrefix ResultPrefix ++ dumpExpression x ]

        Err NoResult ->
            []

        Err e ->
            [ dumpPrefix ErrorPrefix ++ dumpError e ]



-- DUMP EXPRESSION


dumpExpression : Expression -> String
dumpExpression expression =
    case expression of
        Type typ ->
            dumpType typ

        Integer value ->
            String.fromInt value

        Number value ->
            String.fromFloat value

        Tuple items ->
            "(" ++ String.join "," (List.map dumpExpression items) ++ ")"

        Record namedItems ->
            "{"
                ++ String.join ","
                    (List.map
                        (\( name, value ) -> name ++ "=" ++ dumpExpression value)
                        (Dict.toList namedItems)
                    )
                ++ "}"

        Input name _ ->
            name

        Lambda ( inputName, inputType ) output ->
            "(" ++ inputName ++ ":" ++ dumpType inputType ++ ")->" ++ dumpExpression output

        Constructor ( typeName, typeInputs ) name values ->
            let
                toString : String -> List Expression -> String
                toString nam args =
                    if List.isEmpty args then
                        nam

                    else
                        "(" ++ nam ++ " " ++ String.join " " (List.map dumpExpression args) ++ ")"
            in
            toString (toString typeName typeInputs ++ "." ++ name) values



-- DUMP TYPE


dumpType : Type -> String
dumpType typ =
    case typ of
        TypeType ->
            "Type"

        IntType ->
            "Int"

        NumberType ->
            "Number"

        TupleType types ->
            "(" ++ String.join "," (List.map dumpType types) ++ ")"

        RecordType namedTypes ->
            "("
                ++ String.join ","
                    (List.map
                        (\( name, t ) -> name ++ ":" ++ dumpType t)
                        (Dict.toList namedTypes)
                    )
                ++ ")"

        LambdaType inputType outputType ->
            (-- Lambdas are right-associative: a -> b -> c == a -> (b -> c)
             case inputType of
                LambdaType _ _ ->
                    "(" ++ dumpType inputType ++ ")"

                _ ->
                    dumpType inputType
            )
                ++ "->"
                ++ dumpType outputType

        GenericType name ->
            name

        NamedType name typeInputs ->
            name
                ++ (if List.isEmpty typeInputs then
                        ""

                    else
                        " " ++ String.join " " (List.map dumpExpression typeInputs)
                   )



-- DUMP ERROR


dumpError : Error -> String
dumpError error =
    -- TODO: do some proper serialization here
    case error of
        NoResult ->
            ""

        CallTooManyInputs expr inputs ->
            "CallTooManyInputs " ++ dumpExpression expr

        CaseWithoutPatterns expr ->
            "CaseWithoutPatterns " ++ dumpExpression expr

        NameAlreadyExists name ->
            "NameAlreadyExists " ++ name

        NameNotFound name ->
            "NameNotFound " ++ name

        TypeAlreadyExists name ->
            "TypeAlreadyExists " ++ name

        TypeInputsMismatch typeName { got, expected } ->
            "TypeInputsMismatch " ++ typeName

        TypeMismatch expression typ ->
            "TypeMismatch " ++ dumpExpression expression ++ " " ++ dumpType typ

        TypeNotFound name ->
            "TypeNotFound " ++ name
