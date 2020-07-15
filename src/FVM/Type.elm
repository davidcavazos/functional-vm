module FVM.Type exposing
    ( checkNamedType
    , checkType
    , typeOf
    , typecheck
    )

import Dict exposing (Dict)
import FVM exposing (Context, Error(..), Expression(..), Module, Type(..))
import FVM.Context exposing (getGeneric)
import FVM.Module exposing (getType)
import FVM.Util exposing (andThen2, mapDict, mapList, zip2)
import Result



-- CHECK TYPE


checkType : Type -> Context -> Module -> Result Error Type
checkType typ ctx m =
    case typ of
        TypeType ->
            Ok typ

        IntType ->
            Ok typ

        NumberType ->
            Ok typ

        NamedType namedType ->
            checkNamedType namedType (\t _ -> Ok (NamedType t)) ctx m

        LambdaType inputType outputType ->
            Result.map2 LambdaType
                (checkType inputType ctx m)
                (checkType outputType ctx m)

        TupleType itemTypes ->
            Result.map TupleType
                (mapList checkType itemTypes ctx m)

        RecordType namedItemTypes ->
            Result.map RecordType
                (mapDict (\_ -> checkType) namedItemTypes ctx m)

        GenericType name ->
            Ok (getGeneric name ctx)

        UnionType types ->
            Result.map UnionType
                (mapList checkType types ctx m)



-- CHECK NAMED TYPE


checkNamedType : ( String, List Expression ) -> (( String, List Expression ) -> Dict String (List Type) -> Result Error a) -> Context -> Module -> Result Error a
checkNamedType ( typeName, rawTypeInputs ) f ctx m =
    Result.andThen
        (\( typeInputTypes, ctors ) ->
            if List.length rawTypeInputs /= List.length typeInputTypes then
                Err (TypeInputsMismatch typeName { got = rawTypeInputs, expected = typeInputTypes })

            else
                Result.andThen (\typeInputs -> f ( typeName, typeInputs ) ctors)
                    (mapList typecheck (zip2 rawTypeInputs typeInputTypes) ctx m)
        )
        (getType typeName m)



-- TYPE OF


typeOf : Expression -> Context -> Module -> Result Error Type
typeOf expression ctx m =
    case expression of
        Type typ ->
            Result.map (\_ -> TypeType)
                (checkType typ ctx m)

        Integer _ ->
            Ok IntType

        Number _ ->
            Ok NumberType

        Variable _ typ ->
            checkType typ ctx m

        Lambda ( _, inputType ) output ->
            Result.map2 LambdaType
                (checkType inputType ctx m)
                (typeOf output ctx m)

        Tuple items ->
            Result.map TupleType
                (mapList typeOf items ctx m)

        Record namedItems ->
            Result.map RecordType
                (mapDict (\_ -> typeOf) namedItems ctx m)

        Constructor namedType _ _ ->
            checkNamedType namedType (\t _ -> Ok (NamedType t)) ctx m

        Call function input ->
            case typeOf function ctx m of
                Ok (LambdaType inputType outputType) ->
                    Result.map3 (\_ t _ -> t)
                        (checkType inputType ctx m)
                        (checkType outputType ctx m)
                        (typecheck ( input, inputType ) ctx m)

                Ok _ ->
                    Err (CallNonFunction function input)

                Err e ->
                    Err e

        Match input cases outputType ->
            Ok outputType



-- TYPECHECK


typecheck : ( Expression, Type ) -> Context -> Module -> Result Error Expression
typecheck ( expression, typ ) ctx m =
    andThen2
        (\exprType t ->
            if exprType /= t then
                Err (TypeMismatch expression t)

            else
                Ok expression
        )
        (typeOf expression ctx m)
        (checkType typ ctx m)
