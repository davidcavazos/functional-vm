module FVM.Build.Python exposing
    ( pyExpr
    , pyType
    )

import ASM exposing (Accessor(..), Condition(..), Expr(..), Type(..), typeOf)
import Dict exposing (Dict)
import List



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



-- PYTHON EXPRESSION


pyExpr : Expr -> String
pyExpr expr =
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
                    "(" ++ pyExpr item ++ ",)"

                _ ->
                    "(" ++ String.join "," (List.map pyExpr items) ++ ")"

        Record fields ->
            "Record("
                ++ String.join ","
                    (List.map (\( n, v ) -> n ++ "=" ++ pyExpr v)
                        (Dict.toList fields)
                    )
                ++ ")"

        Constructor _ name inputs ->
            name ++ "(" ++ String.join "," (List.map pyExpr inputs) ++ ")"

        Let variables output ->
            pyVariables pyExpr variables output

        Load name _ ->
            name

        Function inputs output ->
            if Dict.isEmpty inputs then
                "lambda:" ++ pyExpr output

            else
                "lambda "
                    ++ String.join "," (Dict.keys inputs)
                    ++ ":"
                    ++ pyExpr output

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
                pyExpr function
                    ++ "("
                    ++ String.join "," (List.map pyExpr inputs)
                    ++ ")"

            else
                "lambda "
                    ++ String.join "," inputsLeft
                    ++ ":"
                    ++ pyExpr function
                    ++ "("
                    ++ String.join "," (List.map pyExpr inputs ++ inputsLeft)
                    ++ ")"

        CaseOf ( input, _ ) cases default ->
            "(lambda _:"
                ++ String.join " else "
                    (List.map pyConditionalCase cases ++ [ pyCase default ])
                ++ ")("
                ++ pyExpr input
                ++ ")"


pyVariables : (a -> String) -> Dict String a -> Expr -> String
pyVariables f variables output =
    if Dict.isEmpty variables then
        pyExpr output

    else
        "(lambda "
            ++ String.join "," (Dict.keys variables)
            ++ ":"
            ++ pyExpr output
            ++ ")("
            ++ String.join "," (List.map f (Dict.values variables))
            ++ ")"


pyConditionalCase : ( List Condition, Dict String Accessor, Expr ) -> String
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


pyCase : ( Dict String Accessor, Expr ) -> String
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
