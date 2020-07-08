module FVM.Bitcode exposing
    ( dump
    , dumpExpression
    , dumpType
    )

import Dict
import FVM exposing (..)


type Prefix
    = -- TypeDefinition
      TypeAliasPrefix
    | TaggedUnionTypePrefix
    | BoundGenericPrefix
      -- NameDefinition
    | ValuePrefix
      -- Result
    | ResultPrefix
    | ErrorPrefix


dumpPrefix : Prefix -> String
dumpPrefix prefix =
    case prefix of
        TypeAliasPrefix ->
            "A "

        BoundGenericPrefix ->
            "G "

        TaggedUnionTypePrefix ->
            "T "

        ValuePrefix ->
            "V "

        ResultPrefix ->
            "R "

        ErrorPrefix ->
            "E "



-- DUMP CONTEXT


dump : Context -> String
dump context =
    String.join ";"
        (List.concat
            [ List.map dumpTypeDefinition (Dict.toList context.types)
            , List.map dumpNameDefinition (Dict.toList context.names)
            , dumpContextResult context.result
            ]
        )


dumpTypeDefinition : ( String, TypeDefinition ) -> String
dumpTypeDefinition ( name, typeDefinition ) =
    let
        dumpMany : String -> (a -> String) -> String -> List a -> String
        dumpMany separator f delimiter items =
            if List.isEmpty items then
                ""

            else
                separator ++ String.join delimiter (List.map f items)
    in
    case typeDefinition of
        TypeAlias _ ->
            Debug.todo "dumpTypeDefinition TypeAlias"

        BoundGeneric typ ->
            dumpPrefix BoundGenericPrefix ++ name ++ "=" ++ dumpType typ

        TaggedUnionType inputTypes constructors ->
            dumpPrefix TaggedUnionTypePrefix
                ++ name
                ++ dumpMany " " dumpType " " inputTypes
                ++ dumpMany "="
                    (\( n, ctrs ) -> n ++ dumpMany " " dumpType " " ctrs)
                    "|"
                    (Dict.toList constructors)


dumpNameDefinition : ( String, Expression ) -> String
dumpNameDefinition ( name, value ) =
    dumpPrefix ValuePrefix ++ name ++ "=" ++ dumpExpression value


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

        Lambda ( inputName, inputType ) output ->
            dumpExpression (Input inputName inputType) ++ "->" ++ dumpExpression output

        Input name typ ->
            "(" ++ name ++ ":" ++ dumpType typ ++ ")"

        Call name inputType outputType inputs ->
            dumpExpression (Input name (LambdaType inputType outputType))
                ++ " "
                ++ String.join " " (List.map dumpExpression inputs)



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

        CallNonFunction expr ->
            "CallNonFunction " ++ dumpExpression expr

        CallTooManyInputs expr inputs ->
            "CallTooManyInputs " ++ dumpExpression expr

        ConstructorInputsMismatch typeName name { got, expected } ->
            "ConstructorInputsMismatch " ++ typeName ++ " " ++ name

        ConstructorNotFound typeName name ->
            "ConstructorNotFound " ++ typeName ++ " " ++ name

        ConstructorNotOfTaggedUnionType typeName tdef ->
            "ConstructorNotOfTaggedUnionType " ++ typeName

        MatchCaseAlreadyCovered pattern ->
            "MatchCaseAlreadyCovered"

        MatchConstructorNotFound ( typeName, typeInputs ) name ->
            "MatchConstructorNotFound"

        MatchMissingCases patterns ->
            "MatchMissingCases"

        MatchPatternTypeMismatch expression pattern ->
            "MatchPatternTypeMismatch " ++ dumpExpression expression

        NameAlreadyExists name ->
            "NameAlreadyExists " ++ name

        NameNotFound name ->
            "NameNotFound " ++ name

        TypeAlreadyExists name tdef ->
            "TypeAlreadyExists " ++ name

        TypeInputsMismatch typeName { got, expected } ->
            "TypeInputsMismatch " ++ typeName

        TypeMismatch expression typ ->
            "TypeMismatch " ++ dumpExpression expression ++ " " ++ dumpType typ

        TypeNotFound name ->
            "TypeNotFound " ++ name
