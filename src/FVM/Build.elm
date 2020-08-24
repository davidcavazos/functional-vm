module FVM.Build exposing
    ( build
    , compile
    , compileExpression
    , compileType
    , compileTypeDef
    )

import ASM
import Dict exposing (Dict)
import FVM exposing (Expression(..), Package, PackageErrors, Type(..))
import FVM.Validate exposing (validate)
import Set exposing (Set)


type alias Build a =
    { typeType : a
    , intType : a
    , numberType : a
    , tupleType : List ASM.Type -> a
    , recordType : Dict String ASM.Type -> a
    , functionType : List ASM.Type -> ASM.Type -> a
    , genericType : String -> a
    , typeDef : ( String, List ASM.Type ) -> Dict String (List ASM.Type) -> a
    , namedExpression : String -> ASM.Expression -> a
    }


type alias UsedTypes =
    { typeType : Bool
    , intType : Bool
    , numberType : Bool
    , genericTypes : Set String
    , types : Dict String ( List ASM.Expression, Dict String (List ASM.Type) )
    }



-- BUILD


build : Build a -> (List a -> b) -> Package -> Result PackageErrors b
build { typeType, intType, numberType, recordType, functionType, genericType, typeDef, namedExpression } assemble pkg =
    compile pkg
        |> Result.map (\_ -> [])
        |> Result.map assemble


findTypeDefinitions : ASM.Package -> UsedTypes
findTypeDefinitions pkg =
    Debug.todo "find used types"



-- COMPILE


compile : Package -> Result PackageErrors ASM.Package
compile pkg =
    validate pkg
        |> Result.map
            (\{ types, names } ->
                { types = Dict.map (\_ -> compileTypeDef) types
                , names = Dict.map (\_ -> compileExpression) names
                }
            )



-- COMPILE TYPE DEFINITION


compileTypeDef : ( List Type, Dict String (List Type) ) -> ( List ASM.Type, Dict String (List ASM.Type) )
compileTypeDef ( typeInputsT, constructors ) =
    Debug.todo "compile type def"



-- COMPILE TYPE


compileType : Type -> ASM.Type
compileType typ =
    case typ of
        TypeT ->
            ASM.TypeT

        IntT ->
            ASM.IntT

        NumberT ->
            ASM.NumberT

        NameT typeName typeInputs ->
            ASM.NameT typeName (List.map compileExpression typeInputs)

        TupleT itemsT ->
            ASM.TupleT (List.map compileType itemsT)

        RecordT fieldsT ->
            ASM.RecordT (Dict.map (\_ -> compileType) fieldsT)

        LambdaT _ _ ->
            let
                getFunctionType t =
                    case t of
                        LambdaT inT outT ->
                            getFunctionType outT
                                |> Tuple.mapFirst (\ts -> inT :: ts)

                        _ ->
                            ( [], t )

                ( inputsT, outputT ) =
                    getFunctionType typ
            in
            ASM.FunctionT (List.map compileType inputsT) (compileType outputT)

        GenericT name ->
            ASM.GenericT name

        UnionT types ->
            ASM.UnionT (List.map compileType types)



-- COMPILE EXPRESSION


compileExpression : Expression -> ASM.Expression
compileExpression expression =
    case expression of
        Type typ ->
            ASM.Type (compileType typ)

        Int value ->
            ASM.Int value

        Number value ->
            ASM.Number value

        Tuple items ->
            Debug.todo "compile expression"

        Record fields ->
            Debug.todo "compile expression"

        Constructor namedType name inputs ->
            Debug.todo "compile expression"

        Let ( name, value ) output ->
            Debug.todo "compile expression"

        Load name typ ->
            Debug.todo "compile expression"

        Lambda ( inputName, inputT ) outputT ->
            Debug.todo "compile expression"

        Call function input ->
            Debug.todo "compile expression"

        CaseOf ( input, outputT ) cases ->
            Debug.todo "compile expression"
