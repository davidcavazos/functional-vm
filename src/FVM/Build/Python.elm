module FVM.Build.Python exposing
    ( pyExpr
    , pyType
    , pyTypeDef
    )

import Dict exposing (Dict)
import FVM exposing (Error, Expression(..), Package, PackageErrors, Type(..))
import FVM.Function exposing (mapFunctionType)



-- TYPE


pyType : Type -> String
pyType typ =
    case typ of
        TypeT ->
            "Type"

        IntT ->
            "Int"

        NumberT ->
            "Number"

        NameT name inputs ->
            let
                inputTypes =
                    List.filterMap
                        (\x ->
                            case x of
                                Type t ->
                                    Just (pyType t)

                                _ ->
                                    Nothing
                        )
                        inputs
            in
            if List.isEmpty inputTypes then
                name

            else
                name ++ "[" ++ String.join ", " inputTypes ++ "]"

        TupleT itemsT ->
            "Tuple[" ++ String.join ", " (List.map pyType itemsT) ++ "]"

        RecordT itemsT ->
            "RecordType({"
                ++ String.join ", "
                    (List.map
                        (\( n, t ) -> "'" ++ n ++ "': " ++ pyType t)
                        (Dict.toList itemsT)
                    )
                ++ "})"

        LambdaT _ _ ->
            mapFunctionType
                (\inputsT outputT ->
                    "Callable[["
                        ++ String.join ", " (List.map pyType inputsT)
                        ++ "], "
                        ++ pyType outputT
                        ++ "]"
                )
                typ

        GenericT name ->
            name

        UnionT types ->
            "Union[" ++ String.join ", " (List.map pyType types) ++ "]"



-- EXPRESSION


pyExpr : Expression -> String
pyExpr expression =
    case expression of
        Type typ ->
            pyType typ

        Int value ->
            String.fromInt value

        Number value ->
            String.fromFloat value

        Tuple items ->
            "(" ++ String.join ", " (List.map pyExpr items) ++ ")"

        Record items ->
            "Record("
                ++ String.join ", "
                    (List.map (\( n, x ) -> n ++ "=" ++ pyExpr x)
                        (Dict.toList items)
                    )
                ++ ")"

        Constructor _ name inputs ->
            name ++ "(" ++ String.join ", " (List.map pyExpr inputs) ++ ")"

        Input typ ->
            Debug.todo "pyExpr"

        Let ( name, value ) output ->
            name ++ " = " ++ pyExpr value ++ "\n" ++ pyExpr output

        Load name ->
            name

        Lambda ( name, value ) output ->
            Debug.todo "pyExpr"

        Call function input ->
            Debug.todo "pyExpr"

        CaseOf ( input, outputT ) cases ->
            Debug.todo "pyExpr"



-- TYPE DEFINITION


pyTypeDef : String -> List Type -> Dict String (List Type) -> Package -> Result Error (List String)
pyTypeDef name typeInputs constructors pkg =
    Debug.todo "pyTypeDef"



-- PACKAGE


pyPackage : Package -> Result PackageErrors (List String)
pyPackage pkg =
    -- TypeT, IntT, NumberT must be defined
    -- NameT are classes
    -- RecordT must
    --      from typing import NamedTuple
    --      RecordType = lambda fields: NamedTuple('Record', fields.items())
    --      Record = lambda fields: namedtuple('Record', fields.keys())(**fields)
    -- GenericT must
    --      from typing import TypeVar
    --      a = TypeVar('T')
    Debug.todo "pyPackage"
