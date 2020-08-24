module FVM.Build.Python exposing
    ( buildPython
    , pyExpression
    , pyType
    , pyTypeDef
    )

import ASM exposing (Accessor(..), Condition(..), Expression(..), Type(..), typeOf)
import Dict exposing (Dict)
import FVM exposing (Package, PackageErrors)
import FVM.Build exposing (build)
import List
import Set exposing (Set)


shebang : String
shebang =
    "#!/usr/bin/env python3"



-- BUILD PYTHON


buildPython : Package -> Result PackageErrors String
buildPython pkg =
    build
        { typeType =
            { imports = Set.empty
            , init = [ "class Type:pass" ]
            }
        , intType =
            { imports = Set.empty
            , init = [ "Int=int" ]
            }
        , numberType =
            { imports = Set.empty
            , init = [ "Number=float" ]
            }
        , tupleType =
            \_ ->
                { imports = Set.empty
                , init = []
                }
        , recordType =
            \_ ->
                { imports = Set.empty
                , init = []
                }
        , functionType =
            \_ _ ->
                { imports = Set.empty
                , init = []
                }
        , genericType =
            \_ ->
                { imports = Set.empty
                , init = []
                }
        , typeDef = pyTypeDef
        , namedExpression = pyNamedExpression
        }
        (\defs ->
            let
                inits =
                    List.map (\{ init } -> init) defs
            in
            String.join "\n" (shebang :: List.concat inits)
        )
        pkg



-- PYTHON TYPE DEFINITION


pyTypeDef : ( String, List Type ) -> Dict String (List Type) -> { imports : Set String, init : List String }
pyTypeDef ( typeName, typeInputs ) constructors =
    let
        pyTypeInputs =
            List.filterMap
                (\typ ->
                    case typ of
                        GenericT name ->
                            Just name

                        _ ->
                            Nothing
                )
                typeInputs

        pyParent =
            typeName ++ pyList "[" pyTypeInputs "]"
    in
    { imports = Set.empty
    , init =
        ("class " ++ typeName ++ pyList "(" pyTypeInputs ")" ++ ":pass")
            :: List.concatMap (pyConstructorDef pyParent)
                (Dict.toList constructors)
    }


pyConstructorDef : String -> ( String, List Type ) -> List String
pyConstructorDef pyParent ( name, inputsT ) =
    [ "@dataclass"
    , "class " ++ name ++ "(" ++ pyParent ++ "):"
    ]
        ++ (if List.isEmpty inputsT then
                [ "  pass" ]

            else
                List.indexedMap
                    (\i t ->
                        "  _" ++ String.fromInt (i + 1) ++ ":" ++ pyType t
                    )
                    inputsT
           )


pyList : String -> List String -> String -> String
pyList start xs end =
    if List.isEmpty xs then
        ""

    else
        start ++ String.join "," xs ++ end



-- PYTHON NAMED EXPRESSION


pyNamedExpression : String -> Expression -> { imports : Set String, init : List String }
pyNamedExpression name expr =
    Debug.todo "pyNamedExpression"



-- PYTHON TYPE


pyType : Type -> String
pyType typ =
    case typ of
        TypeT ->
            "Type"

        IntT ->
            "Int"

        NumberT ->
            "Number"

        TupleT itemsT ->
            "Tuple[" ++ String.join "," (List.map pyType itemsT) ++ "]"

        RecordT fieldsT ->
            "RecordType("
                ++ String.join ","
                    (List.map (\( n, t ) -> n ++ "=" ++ pyType t)
                        (Dict.toList fieldsT)
                    )
                ++ ")"

        NameT name inputs ->
            let
                typeInputs =
                    List.filterMap
                        (\input ->
                            case input of
                                Type t ->
                                    Just t

                                _ ->
                                    Nothing
                        )
                        inputs
            in
            if List.isEmpty typeInputs then
                name

            else
                name
                    ++ "["
                    ++ String.join "," (List.map pyType typeInputs)
                    ++ "]"

        FunctionT inputsT outputT ->
            "Callable["
                ++ "["
                ++ String.join "," (List.map pyType inputsT)
                ++ "],"
                ++ pyType outputT
                ++ "]"

        GenericT name ->
            name

        UnionT types ->
            "Union[" ++ String.join "," (List.map pyType types) ++ "]"



-- PYTHON EXPRESSION


pyExpression : Expression -> String
pyExpression expr =
    case expr of
        Type typ ->
            pyType typ

        Int value ->
            String.fromInt value

        Number value ->
            String.fromFloat value
                |> (\s ->
                        if String.contains "." s then
                            s

                        else
                            s ++ ".0"
                   )

        Tuple items ->
            case items of
                [ item ] ->
                    "(" ++ pyExpression item ++ ",)"

                _ ->
                    "(" ++ String.join "," (List.map pyExpression items) ++ ")"

        Record fields ->
            "Record("
                ++ String.join ","
                    (List.map (\( n, v ) -> n ++ "=" ++ pyExpression v)
                        (Dict.toList fields)
                    )
                ++ ")"

        Constructor _ name inputs ->
            name ++ "(" ++ String.join "," (List.map pyExpression inputs) ++ ")"

        Let variables output ->
            pyVariables pyExpression variables output

        Load name _ ->
            name

        Function inputs output ->
            if Dict.isEmpty inputs then
                "lambda:" ++ pyExpression output

            else
                "lambda "
                    ++ String.join "," (Dict.keys inputs)
                    ++ ":"
                    ++ pyExpression output

        Call function inputs ->
            let
                inputsT =
                    case typeOf function of
                        FunctionT inputTypes _ ->
                            inputTypes

                        _ ->
                            []

                inputsLeft =
                    List.map (\i -> "_" ++ String.fromInt i)
                        (List.range (List.length inputs + 1)
                            (List.length inputsT)
                        )
            in
            if List.isEmpty inputsLeft then
                pyExpression function
                    ++ "("
                    ++ String.join "," (List.map pyExpression inputs)
                    ++ ")"

            else
                "lambda "
                    ++ String.join "," inputsLeft
                    ++ ":"
                    ++ pyExpression function
                    ++ "("
                    ++ String.join "," (List.map pyExpression inputs ++ inputsLeft)
                    ++ ")"

        CaseOf ( input, _ ) cases default ->
            "(lambda _:"
                ++ String.join " else "
                    (List.map pyConditionalCase cases ++ [ pyCase default ])
                ++ ")("
                ++ pyExpression input
                ++ ")"


pyVariables : (a -> String) -> Dict String a -> Expression -> String
pyVariables f variables output =
    if Dict.isEmpty variables then
        pyExpression output

    else
        "(lambda "
            ++ String.join "," (Dict.keys variables)
            ++ ":"
            ++ pyExpression output
            ++ ")("
            ++ String.join "," (List.map f (Dict.values variables))
            ++ ")"


pyConditionalCase : ( List Condition, Dict String Accessor, Expression ) -> String
pyConditionalCase ( conditions, variables, output ) =
    pyCase ( variables, output )
        ++ " if "
        ++ (if List.isEmpty conditions then
                "True"

            else
                String.join " and " (List.map pyCondition conditions)
           )


pyCondition : Condition -> String
pyCondition condition =
    case condition of
        EqualsType accessor typ ->
            pyAccessor accessor ++ "==" ++ pyType typ

        EqualsInt accessor value ->
            pyAccessor accessor ++ "==" ++ String.fromInt value

        EqualsNumber accessor value ->
            pyAccessor accessor ++ "==" ++ String.fromFloat value

        EqualsConstructor accessor _ name ->
            "type(" ++ pyAccessor accessor ++ ")==" ++ name


pyCase : ( Dict String Accessor, Expression ) -> String
pyCase ( variables, output ) =
    pyVariables pyAccessor variables output


pyAccessor : Accessor -> String
pyAccessor accessor =
    case accessor of
        Self ->
            "_"

        TupleItem index ->
            "_[" ++ String.fromInt index ++ "]"

        RecordField name ->
            "_." ++ name

        ConstructorInput index ->
            "_._" ++ String.fromInt (index + 1)
