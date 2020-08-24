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
                                |> Tuple.mapFirst
                                    (\ts ->
                                        compileType inT :: ts
                                    )

                        _ ->
                            ( [], compileType t )

                ( inputsT, outputT ) =
                    getFunctionType typ
            in
            ASM.FunctionT inputsT outputT

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
            ASM.Tuple (List.map compileExpression items)

        Record fields ->
            ASM.Record (Dict.map (\_ -> compileExpression) fields)

        Constructor ( typeName, typeInputs ) name inputs ->
            ASM.Constructor ( typeName, List.map compileExpression typeInputs )
                name
                (List.map compileExpression inputs)

        Let _ _ ->
            let
                getVariables e =
                    case e of
                        Let ( name, value ) out ->
                            getVariables out
                                |> Tuple.mapFirst
                                    (\xs ->
                                        ( name, compileExpression value ) :: xs
                                    )

                        _ ->
                            ( [], compileExpression e )

                ( variables, output ) =
                    getVariables expression
            in
            ASM.Let (Dict.fromList variables) output

        Load name typ ->
            ASM.Load name (compileType typ)

        Lambda _ _ ->
            let
                getFunction e =
                    case e of
                        Lambda ( name, inputT ) out ->
                            getFunction out
                                |> Tuple.mapFirst
                                    (\ins ->
                                        ( name, compileType inputT ) :: ins
                                    )

                        _ ->
                            ( [], compileExpression e )

                ( inputs, output ) =
                    getFunction expression
            in
            ASM.Function (Dict.fromList inputs) output

        Call function input ->
            Debug.todo "compile expression"

        CaseOf ( input, outputT ) cases ->
            Debug.todo "compile expression"
