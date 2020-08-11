module FVM.Validate exposing
    ( check
    , checkP
    , checkT
    , typeOfP
    , typecheck
    , typecheckP
    , validate
    )

import Dict exposing (Dict)
import FVM exposing (Case(..), Error(..), Expression(..), Package, PackageErrors, Pattern(..), Type(..))
import FVM.Package exposing (letName)
import FVM.Type exposing (typeOf)
import FVM.Util exposing (andThen2, andThen3, andThenDict, andThenList, combinations, zip2)
import Result



---=== MODULE ===---
--
-- VALIDATE MODULE


validate : Package -> Result PackageErrors Package
validate pkg =
    let
        typeErrors =
            validateTypes pkg

        nameErrors =
            validateNames pkg
    in
    if Dict.isEmpty typeErrors && Dict.isEmpty nameErrors then
        Ok pkg

    else
        Err { types = typeErrors, names = nameErrors }


validateTypes : Package -> Dict String Error
validateTypes pkg =
    Dict.map
        (\_ ( typeInputs, ctors ) ->
            Result.map2 (\_ _ -> Nothing)
                (andThenList (\t -> checkT t pkg) typeInputs)
                (andThenDict
                    (\_ inputsT -> andThenList (\t -> checkT t pkg) inputsT)
                    ctors
                )
        )
        pkg.types
        |> collectErrors


validateNames : Package -> Dict String Error
validateNames pkg =
    Dict.map (\_ -> check pkg) pkg.names
        |> collectErrors


collectErrors : Dict comparable (Result error a) -> Dict comparable error
collectErrors dict =
    Dict.toList dict
        |> List.filterMap
            (\( k, result ) ->
                case result of
                    Ok _ ->
                        Nothing

                    Err e ->
                        Just ( k, e )
            )
        |> Dict.fromList



---=== TYPES ===---
--
-- CHECK TYPE


checkT : Type -> Package -> Result Error Type
checkT typ pkg =
    case typ of
        TypeT ->
            Ok typ

        IntT ->
            Ok typ

        NumberT ->
            Ok typ

        NameT typeName typeInputs ->
            Result.map (\_ -> typ)
                (getTypeDefinition ( typeName, typeInputs ) pkg)

        TupleT itemsT ->
            Result.map (\_ -> typ)
                (andThenList (\t -> checkT t pkg) itemsT)

        RecordT itemsT ->
            Result.map (\_ -> typ)
                (andThenDict (\_ t -> checkT t pkg) itemsT)

        LambdaT inputT outputT ->
            Result.map2 (\_ _ -> typ)
                (checkT inputT pkg)
                (checkT outputT pkg)

        GenericT _ ->
            Ok typ

        UnionT types ->
            Result.map (\_ -> typ)
                (andThenList (\t -> checkT t pkg) types)



---=== EXPRESSIONS ===---
--
-- CHECK EXPRESSION


check : Package -> Expression -> Result Error Expression
check pkg expression =
    case expression of
        Type typ ->
            Result.map (\_ -> expression)
                (checkT typ pkg)

        Int _ ->
            Ok expression

        Number _ ->
            Ok expression

        Tuple items ->
            Result.map (\_ -> expression)
                (andThenList (check pkg) items)

        Record items ->
            Result.map (\_ -> expression)
                (andThenDict (\_ -> check pkg) items)

        Constructor namedT name inputs ->
            Result.andThen
                (\ctors ->
                    case Dict.get name ctors of
                        Just inputsT ->
                            if List.map typeOf inputs == inputsT then
                                Ok expression

                            else
                                Err (ConstructorInputsMismatch namedT name { got = List.map typeOf inputs, expected = inputsT })

                        Nothing ->
                            Err (ConstructorNotFound namedT name)
                )
                (getTypeDefinition namedT pkg)

        Let ( name, value ) output ->
            case Dict.get name pkg.names of
                Just existing ->
                    Err (NameAlreadyExists name { got = value, existing = existing })

                Nothing ->
                    Result.map2 (\_ _ -> expression)
                        (check pkg value)
                        (check (letName name value pkg) output)

        Load name typ ->
            Result.map (\_ -> expression)
                (getName name pkg)

        Lambda ( name, inputT ) output ->
            case Dict.get name pkg.names of
                Just existing ->
                    Err (NameAlreadyExists name { got = Load name inputT, existing = existing })

                Nothing ->
                    Result.map2 (\_ _ -> expression)
                        (checkT inputT pkg)
                        (check (letName name (Load name inputT) pkg) output)

        Call _ _ ->
            Result.map (\_ -> expression)
                (checkCall expression Dict.empty pkg)

        CaseOf ( input, outputT ) cases ->
            andThen2
                (\_ _ ->
                    List.foldl
                        (checkCase input outputT pkg)
                        (Ok ( [], [ AnyC (typeOf input) ] ))
                        cases
                        |> Result.andThen
                            (\( _, missing ) ->
                                case typeOf input of
                                    RecordT _ ->
                                        if List.isEmpty cases then
                                            Err (CasesMissing ( input, outputT ) missing)

                                        else
                                            Ok expression

                                    _ ->
                                        if List.isEmpty missing then
                                            Ok expression

                                        else
                                            Err (CasesMissing ( input, outputT ) missing)
                            )
                )
                (check pkg input)
                (checkT outputT pkg)


checkCall : Expression -> Dict String Type -> Package -> Result Error (Dict String Type)
checkCall expression generics pkg =
    case expression of
        Call function input ->
            andThen2
                (\_ gs ->
                    case typeOf function of
                        LambdaT (GenericT name) _ ->
                            case Dict.get name gs of
                                Just t ->
                                    Result.map (\_ -> gs)
                                        (typecheck input t pkg)

                                Nothing ->
                                    Ok (Dict.insert name (typeOf input) gs)

                        LambdaT inputT _ ->
                            Result.map (\_ -> gs)
                                (typecheck input inputT pkg)

                        _ ->
                            Err (CallNonFunction function input)
                )
                (check pkg function)
                (checkCall function generics pkg)

        _ ->
            Ok generics


checkCase :
    Expression
    -> Type
    -> Package
    -> ( Pattern, Expression )
    -> Result Error ( List Pattern, List Case )
    -> Result Error ( List Pattern, List Case )
checkCase input outputT pkg ( pattern, output ) seenAndMissingResult =
    andThen3
        (\( seen, missingCases ) _ patternPkg ->
            andThen2
                (\_ _ ->
                    if isCaseCovered pattern seen then
                        Err (CaseAlreadyCovered ( input, outputT ) ( pattern, output ))

                    else
                        Result.map
                            (\cases -> ( pattern :: seen, cases ))
                            (expandCases pattern missingCases pkg)
                )
                (check patternPkg output)
                (typecheck output outputT patternPkg)
        )
        seenAndMissingResult
        (typecheckP pattern (typeOf input) pkg)
        (withPattern pattern pkg)


isCaseCovered : Pattern -> List Pattern -> Bool
isCaseCovered pattern seenPatterns =
    List.foldl
        (\seen isCovered ->
            case seen of
                AnyP _ ->
                    True

                _ ->
                    isCovered || pattern == seen
        )
        False
        seenPatterns


expandCases : Pattern -> List Case -> Package -> Result Error (List Case)
expandCases pattern cases pkg =
    Result.map List.concat
        (andThenList (\c -> expandCase pattern c pkg) cases)


expandCase : Pattern -> Case -> Package -> Result Error (List Case)
expandCase pattern case_ pkg =
    andThen2
        (\patternType casePattern ->
            if patternType == casePattern then
                case pattern of
                    AnyP _ ->
                        Ok []

                    NameP p _ ->
                        expandCase p case_ pkg

                    TypeP _ ->
                        Ok [ case_ ]

                    IntP _ ->
                        Ok [ case_ ]

                    NumberP _ ->
                        Ok [ case_ ]

                    TupleP itemsP ->
                        case case_ of
                            AnyC (TupleT itemsT) ->
                                expandCase pattern (TupleC (List.map AnyC itemsT)) pkg

                            TupleC itemsC ->
                                Result.map
                                    (\choices -> List.map TupleC (combinations choices))
                                    (andThenList (\( p, c ) -> expandCase p c pkg) (zip2 itemsP itemsC))

                            _ ->
                                Ok [ case_ ]

                    RecordP _ ->
                        Ok []

                    ConstructorP (( typeName, typeInputs ) as namedT) name inputsP ->
                        case case_ of
                            AnyC _ ->
                                Result.andThen
                                    (\ctors ->
                                        Dict.map
                                            (\nameC inputsT -> ConstructorC ( typeName, typeInputs ) nameC (List.map AnyC inputsT))
                                            ctors
                                            |> Dict.values
                                            |> (\cases -> expandCases pattern cases pkg)
                                    )
                                    (getTypeDefinition namedT pkg)

                            ConstructorC namedTC nameC inputsC ->
                                if namedT == namedTC && name == nameC then
                                    Result.map
                                        (\choices -> List.map (ConstructorC namedT nameC) (combinations choices))
                                        (andThenList (\( p, c ) -> expandCase p c pkg) (zip2 inputsP inputsC))

                                else
                                    Ok [ case_ ]

                            _ ->
                                Ok [ case_ ]

            else
                Ok [ case_ ]
        )
        (typeOfP pattern pkg)
        (typeOfP (caseToPattern case_) pkg)


caseToPattern : Case -> Pattern
caseToPattern case_ =
    case case_ of
        AnyC t ->
            AnyP t

        TupleC itemsC ->
            TupleP (List.map caseToPattern itemsC)

        ConstructorC namedT name inputsC ->
            ConstructorP namedT name (List.map caseToPattern inputsC)



-- TYPECHECK EXPRESSION


typecheck : Expression -> Type -> Package -> Result Error Expression
typecheck expression typ pkg =
    andThen2
        (\_ _ ->
            if typeOf expression == typ then
                Ok expression

            else
                Err (TypeMismatch expression typ)
        )
        (check pkg expression)
        (checkT typ pkg)



---=== PATTERNS ===---
--
-- CHECK PATTERN


checkP : Pattern -> Package -> Result Error Pattern
checkP pattern pkg =
    case pattern of
        AnyP t ->
            Result.map (\_ -> pattern) (checkT t pkg)

        NameP p _ ->
            Result.map (\_ -> pattern) (checkP p pkg)

        TypeP t ->
            Result.map (\_ -> pattern) (checkT t pkg)

        IntP _ ->
            Ok pattern

        NumberP _ ->
            Ok pattern

        TupleP itemsP ->
            Result.map (\_ -> pattern)
                (andThenList (\p -> checkP p pkg) itemsP)

        RecordP itemsT ->
            Result.map (\_ -> pattern)
                (checkT (RecordT itemsT) pkg)

        ConstructorP namedT name inputsP ->
            Result.andThen
                (\ctors ->
                    case Dict.get name ctors of
                        Just inputsT ->
                            Result.andThen
                                (\gotT ->
                                    if gotT == inputsT then
                                        Ok pattern

                                    else
                                        Err (ConstructorInputsMismatch namedT name { got = gotT, expected = inputsT })
                                )
                                (andThenList (\p -> typeOfP p pkg) inputsP)

                        Nothing ->
                            Err (ConstructorNotFound namedT name)
                )
                (getTypeDefinition namedT pkg)



-- TYPE OF PATTERN


typeOfP : Pattern -> Package -> Result Error Type
typeOfP pattern pkg =
    Result.andThen
        (\_ ->
            case pattern of
                AnyP t ->
                    Ok t

                NameP p _ ->
                    typeOfP p pkg

                TypeP _ ->
                    Ok TypeT

                IntP _ ->
                    Ok IntT

                NumberP _ ->
                    Ok NumberT

                TupleP itemsP ->
                    Result.map TupleT (andThenList (\p -> typeOfP p pkg) itemsP)

                RecordP itemsT ->
                    Ok (RecordT itemsT)

                ConstructorP ( typeName, typeInputs ) _ _ ->
                    Ok (NameT typeName typeInputs)
        )
        (checkP pattern pkg)



-- TYPECHECK PATTERN


typecheckP : Pattern -> Type -> Package -> Result Error Pattern
typecheckP pattern typ pkg =
    andThen2
        (\patternType _ ->
            if patternType == typ then
                Ok pattern

            else
                case ( pattern, typ ) of
                    ( RecordP itemsPT, RecordT itemsT ) ->
                        Dict.foldl
                            (\name pt ->
                                Result.andThen
                                    (\_ ->
                                        case Dict.get name itemsT of
                                            Just t ->
                                                if pt == t then
                                                    Ok pattern

                                                else
                                                    Err (PatternMismatch pattern typ)

                                            Nothing ->
                                                Err (PatternMismatch pattern typ)
                                    )
                            )
                            (Ok pattern)
                            itemsPT

                    _ ->
                        Err (PatternMismatch pattern typ)
        )
        (typeOfP pattern pkg)
        (checkT typ pkg)



---=== MODULE ===---
--
-- GET TYPE DEFINITION


getTypeDefinition : ( String, List Expression ) -> Package -> Result Error (Dict String (List Type))
getTypeDefinition ( typeName, typeInputs ) pkg =
    case Dict.get typeName pkg.types of
        Just ( typeInputsT, ctors ) ->
            if List.map typeOf typeInputs == typeInputsT then
                Ok ctors

            else
                Err (TypeInputsMismatch typeName { got = List.map typeOf typeInputs, expected = typeInputsT })

        Nothing ->
            Err (TypeNotFound typeName)


getName : String -> Package -> Result Error Expression
getName name pkg =
    case Dict.get name pkg.names of
        Just value ->
            Ok value

        Nothing ->
            Err (NameNotFound name)


withPattern : Pattern -> Package -> Result Error Package
withPattern pattern pkg =
    Result.andThen
        (\typ ->
            case pattern of
                AnyP _ ->
                    Ok pkg

                NameP p name ->
                    Result.map (letName name (Load name typ))
                        (withPattern p pkg)

                TypeP _ ->
                    Ok pkg

                IntP _ ->
                    Ok pkg

                NumberP _ ->
                    Ok pkg

                TupleP itemsP ->
                    List.foldl (\p -> Result.andThen (withPattern p))
                        (Ok pkg)
                        itemsP

                RecordP _ ->
                    Ok pkg

                ConstructorP _ _ inputsP ->
                    List.foldl (\p -> Result.andThen (withPattern p))
                        (Ok pkg)
                        inputsP
        )
        (typeOfP pattern pkg)
