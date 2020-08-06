module FVM.Validate exposing
    ( check
    , checkP
    , checkT
    , typeOf
    , typeOfP
    , typecheck
    , typecheckP
    , validate
    )

import Dict exposing (Dict)
import FVM exposing (Case(..), Error(..), Expression(..), Package, PackageErrors, Pattern(..), Type(..))
import FVM.Package exposing (letName)
import FVM.Util exposing (andThen2, andThen3, andThenDict, andThenList, combinations, zip2)
import Result



---=== MODULE ===---
--
-- VALIDATE MODULE


validate : Package -> Result PackageErrors Package
validate m =
    let
        typeErrors =
            validateTypes m

        nameErrors =
            validateNames m
    in
    if Dict.isEmpty typeErrors && Dict.isEmpty nameErrors then
        Ok m

    else
        Err { types = typeErrors, names = nameErrors }


validateTypes : Package -> Dict String Error
validateTypes m =
    Dict.map
        (\_ ( typeInputs, ctors ) ->
            Result.map2 (\_ _ -> Nothing)
                (andThenList (\t -> checkT t m) typeInputs)
                (andThenDict
                    (\_ inputsT -> andThenList (\t -> checkT t m) inputsT)
                    ctors
                )
        )
        m.types
        |> collectErrors


validateNames : Package -> Dict String Error
validateNames m =
    Dict.map
        (\_ value -> check value m)
        m.names
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
checkT typ m =
    case typ of
        TypeT ->
            Ok typ

        IntT ->
            Ok typ

        NumberT ->
            Ok typ

        NameT typeName typeInputs ->
            Result.map (\_ -> typ)
                (getTypeDefinition ( typeName, typeInputs ) m)

        TupleT itemsT ->
            Result.map (\_ -> typ)
                (andThenList (\t -> checkT t m) itemsT)

        RecordT itemsT ->
            Result.map (\_ -> typ)
                (andThenDict (\_ t -> checkT t m) itemsT)

        LambdaT inputT outputT ->
            Result.map2 (\_ _ -> typ)
                (checkT inputT m)
                (checkT outputT m)

        GenericT _ ->
            Ok typ

        UnionT types ->
            Result.map (\_ -> typ)
                (andThenList (\t -> checkT t m) types)



---=== EXPRESSIONS ===---
--
-- CHECK EXPRESSION


check : Expression -> Package -> Result Error Expression
check expression m =
    case expression of
        Type typ ->
            Result.map (\_ -> expression)
                (checkT typ m)

        Int _ ->
            Ok expression

        Number _ ->
            Ok expression

        Tuple items ->
            Result.map (\_ -> expression)
                (andThenList (\x -> check x m) items)

        Record items ->
            Result.map (\_ -> expression)
                (andThenDict (\_ x -> check x m) items)

        Constructor namedT name inputs ->
            Result.andThen
                (\ctors ->
                    case Dict.get name ctors of
                        Just inputsT ->
                            Result.andThen
                                (\gotT ->
                                    if gotT == inputsT then
                                        Ok expression

                                    else
                                        Err (ConstructorInputsMismatch namedT name { got = gotT, expected = inputsT })
                                )
                                (andThenList (\x -> typeOf x m) inputs)

                        Nothing ->
                            Err (ConstructorNotFound namedT name)
                )
                (getTypeDefinition namedT m)

        Input typ ->
            Result.map (\_ -> expression)
                (checkT typ m)

        Let ( name, value ) output ->
            case Dict.get name m.names of
                Just existing ->
                    Err (NameAlreadyExists name { got = value, existing = existing })

                Nothing ->
                    Result.map2 (\_ _ -> expression)
                        (check value m)
                        (check output (letName name value m))

        Load name ->
            Result.map (\_ -> expression)
                (getName name m)

        Lambda ( name, inputT ) output ->
            case Dict.get name m.names of
                Just existing ->
                    Err (NameAlreadyExists name { got = Input inputT, existing = existing })

                Nothing ->
                    Result.map2 (\_ _ -> expression)
                        (checkT inputT m)
                        (check output (letName name (Input inputT) m))

        Call _ _ ->
            Result.map (\_ -> expression)
                (checkCall expression Dict.empty m)

        CaseOf ( input, outputT ) cases ->
            andThen2
                (\inputT _ ->
                    List.foldl
                        (checkCase ( input, inputT ) outputT m)
                        (Ok ( [], [ AnyC inputT ] ))
                        cases
                        |> Result.andThen
                            (\( _, missing ) ->
                                case inputT of
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
                (typeOf input m)
                (checkT outputT m)


checkCall : Expression -> Dict String Type -> Package -> Result Error (Dict String Type)
checkCall expression generics m =
    case expression of
        Call function input ->
            Result.andThen
                (\gs ->
                    case typeOf function m of
                        Ok (LambdaT (GenericT name) _) ->
                            case Dict.get name gs of
                                Just t ->
                                    Result.map (\_ -> gs)
                                        (typecheck input t m)

                                Nothing ->
                                    Result.map (\t -> Dict.insert name t gs)
                                        (typeOf input m)

                        Ok (LambdaT inputT _) ->
                            Result.map (\_ -> gs)
                                (typecheck input inputT m)

                        Ok _ ->
                            Err (CallNonFunction function input)

                        Err e ->
                            Err e
                )
                (checkCall function generics m)

        _ ->
            Ok generics


checkCase :
    ( Expression, Type )
    -> Type
    -> Package
    -> ( Pattern, Expression )
    -> Result Error ( List Pattern, List Case )
    -> Result Error ( List Pattern, List Case )
checkCase ( input, inputT ) outputT m ( pattern, output ) seenAndMissingResult =
    andThen3
        (\( seen, missingCases ) _ mod ->
            Result.andThen
                (\_ ->
                    if isCaseCovered pattern seen then
                        Err (CaseAlreadyCovered ( input, outputT ) ( pattern, output ))

                    else
                        Result.map
                            (\cases -> ( pattern :: seen, cases ))
                            (expandCases pattern missingCases m)
                )
                (typecheck output outputT mod)
        )
        seenAndMissingResult
        (typecheckP pattern inputT m)
        (withPattern pattern m)


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
expandCases pattern cases m =
    Result.map List.concat
        (andThenList (\c -> expandCase pattern c m) cases)


expandCase : Pattern -> Case -> Package -> Result Error (List Case)
expandCase pattern case_ m =
    andThen2
        (\patternType casePattern ->
            if patternType == casePattern then
                case pattern of
                    AnyP _ ->
                        Ok []

                    NameP p _ ->
                        expandCase p case_ m

                    TypeP _ ->
                        Ok [ case_ ]

                    IntP _ ->
                        Ok [ case_ ]

                    NumberP _ ->
                        Ok [ case_ ]

                    TupleP itemsP ->
                        case case_ of
                            AnyC (TupleT itemsT) ->
                                expandCase pattern (TupleC (List.map AnyC itemsT)) m

                            TupleC itemsC ->
                                Result.map
                                    (\choices -> List.map TupleC (combinations choices))
                                    (andThenList (\( p, c ) -> expandCase p c m) (zip2 itemsP itemsC))

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
                                            |> (\cases -> expandCases pattern cases m)
                                    )
                                    (getTypeDefinition namedT m)

                            ConstructorC namedTC nameC inputsC ->
                                if namedT == namedTC && name == nameC then
                                    Result.map
                                        (\choices -> List.map (ConstructorC namedT nameC) (combinations choices))
                                        (andThenList (\( p, c ) -> expandCase p c m) (zip2 inputsP inputsC))

                                else
                                    Ok [ case_ ]

                            _ ->
                                Ok [ case_ ]

            else
                Ok [ case_ ]
        )
        (typeOfP pattern m)
        (typeOfP (caseToPattern case_) m)


caseToPattern : Case -> Pattern
caseToPattern case_ =
    case case_ of
        AnyC t ->
            AnyP t

        TupleC itemsC ->
            TupleP (List.map caseToPattern itemsC)

        ConstructorC namedT name inputsC ->
            ConstructorP namedT name (List.map caseToPattern inputsC)



-- TYPE OF EXPRESSION


typeOf : Expression -> Package -> Result Error Type
typeOf expression m =
    Result.andThen
        (\_ ->
            case expression of
                Type _ ->
                    Ok TypeT

                Int _ ->
                    Ok IntT

                Number _ ->
                    Ok NumberT

                Tuple items ->
                    Result.map TupleT
                        (andThenList (\x -> typeOf x m) items)

                Record items ->
                    Result.map RecordT
                        (andThenDict (\_ x -> typeOf x m) items)

                Constructor ( typeName, typeInputs ) _ _ ->
                    Ok (NameT typeName typeInputs)

                Input typ ->
                    Ok typ

                Let ( name, value ) output ->
                    typeOf output (letName name value m)

                Load name ->
                    Result.andThen (\e -> typeOf e m)
                        (getName name m)

                Lambda ( inputName, inputT ) output ->
                    Result.map (LambdaT inputT)
                        (typeOf output (letName inputName (Input inputT) m))

                Call function input ->
                    case typeOf function m of
                        Ok (LambdaT _ outputT) ->
                            Ok outputT

                        Ok _ ->
                            Err (CallNonFunction function input)

                        Err e ->
                            Err e

                CaseOf ( _, outputT ) _ ->
                    Ok outputT
        )
        (check expression m)



-- TYPECHECK EXPRESSION


typecheck : Expression -> Type -> Package -> Result Error Expression
typecheck expression typ m =
    andThen2
        (\exprType _ ->
            if exprType == typ then
                Ok expression

            else
                Err (TypeMismatch expression typ)
        )
        (typeOf expression m)
        (checkT typ m)



---=== PATTERNS ===---
--
-- CHECK PATTERN


checkP : Pattern -> Package -> Result Error Pattern
checkP pattern m =
    case pattern of
        AnyP t ->
            Result.map (\_ -> pattern) (checkT t m)

        NameP p _ ->
            Result.map (\_ -> pattern) (checkP p m)

        TypeP t ->
            Result.map (\_ -> pattern) (checkT t m)

        IntP _ ->
            Ok pattern

        NumberP _ ->
            Ok pattern

        TupleP itemsP ->
            Result.map (\_ -> pattern)
                (andThenList (\p -> checkP p m) itemsP)

        RecordP itemsT ->
            Result.map (\_ -> pattern)
                (checkT (RecordT itemsT) m)

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
                                (andThenList (\p -> typeOfP p m) inputsP)

                        Nothing ->
                            Err (ConstructorNotFound namedT name)
                )
                (getTypeDefinition namedT m)



-- TYPE OF PATTERN


typeOfP : Pattern -> Package -> Result Error Type
typeOfP pattern m =
    Result.andThen
        (\_ ->
            case pattern of
                AnyP t ->
                    Ok t

                NameP p _ ->
                    typeOfP p m

                TypeP _ ->
                    Ok TypeT

                IntP _ ->
                    Ok IntT

                NumberP _ ->
                    Ok NumberT

                TupleP itemsP ->
                    Result.map TupleT (andThenList (\p -> typeOfP p m) itemsP)

                RecordP itemsT ->
                    Ok (RecordT itemsT)

                ConstructorP ( typeName, typeInputs ) _ _ ->
                    Ok (NameT typeName typeInputs)
        )
        (checkP pattern m)



-- TYPECHECK PATTERN


typecheckP : Pattern -> Type -> Package -> Result Error Pattern
typecheckP pattern typ m =
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
        (typeOfP pattern m)
        (checkT typ m)



---=== MODULE ===---
--
-- GET TYPE DEFINITION


getTypeDefinition : ( String, List Expression ) -> Package -> Result Error (Dict String (List Type))
getTypeDefinition ( typeName, typeInputs ) m =
    case Dict.get typeName m.types of
        Just ( typeInputsT, ctors ) ->
            Result.andThen
                (\gotT ->
                    if gotT == typeInputsT then
                        Ok ctors

                    else
                        Err (TypeInputsMismatch typeName { got = gotT, expected = typeInputsT })
                )
                (andThenList (\x -> typeOf x m) typeInputs)

        Nothing ->
            Err (TypeNotFound typeName)


getName : String -> Package -> Result Error Expression
getName name m =
    case Dict.get name m.names of
        Just value ->
            Ok value

        Nothing ->
            Err (NameNotFound name)


withPattern : Pattern -> Package -> Result Error Package
withPattern pattern m =
    Result.andThen
        (\typ ->
            case pattern of
                AnyP _ ->
                    Ok m

                NameP p name ->
                    Result.map (letName name (Input typ))
                        (withPattern p m)

                TypeP _ ->
                    Ok m

                IntP _ ->
                    Ok m

                NumberP _ ->
                    Ok m

                TupleP itemsP ->
                    List.foldl (\p -> Result.andThen (withPattern p))
                        (Ok m)
                        itemsP

                RecordP _ ->
                    Ok m

                ConstructorP _ _ inputsP ->
                    List.foldl (\p -> Result.andThen (withPattern p))
                        (Ok m)
                        inputsP
        )
        (typeOfP pattern m)
