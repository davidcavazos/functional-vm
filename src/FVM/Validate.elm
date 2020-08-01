module FVM.Validate exposing
    ( check
    , checkP
    , checkT
    , expandCase
    , expandCases
    , typeOf
    , typeOfP
    , typecheck
    , typecheckP
    )

import Dict
import FVM exposing (Case(..), Error(..), Expression(..), Module, Pattern(..), Type(..))
import FVM.Module exposing (addName, getName, getTypeDefinition)
import FVM.Util exposing (andThen2, andThen3, andThenDict, andThenList, combinations, zip2)
import Result



---=== TYPES ===---
--
-- CHECK TYPE


checkT : Type -> Module -> Result Error Type
checkT typ m =
    case typ of
        TypeT ->
            Ok typ

        IntT ->
            Ok typ

        NumberT ->
            Ok typ

        NameT typeName typeInputs ->
            Result.andThen
                (\( typeInputsT, _ ) ->
                    Result.andThen
                        (\gotT ->
                            if gotT == typeInputsT then
                                Ok typ

                            else
                                Err (TypeInputsMismatch typeName { got = gotT, expected = typeInputsT })
                        )
                        (andThenList (\x -> typeOf x m) typeInputs)
                )
                (getTypeDefinition typeName m)

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

        GenericT name ->
            case Dict.get name m.generics of
                Just t ->
                    Ok t

                Nothing ->
                    Ok (GenericT name)

        UnionT types ->
            Result.map (\_ -> typ)
                (andThenList (\t -> checkT t m) types)



---=== EXPRESSIONS ===---
--
-- CHECK EXPRESSION


check : Expression -> Module -> Result Error Expression
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

        Constructor ( typeName, typeInputs ) name inputs ->
            andThen2
                (\_ ( _, ctors ) ->
                    case Dict.get name ctors of
                        Just inputsT ->
                            Result.andThen
                                (\gotT ->
                                    if gotT == inputsT then
                                        Ok expression

                                    else
                                        Err (ConstructorInputsMismatch typeName name { got = gotT, expected = inputsT })
                                )
                                (andThenList (\x -> typeOf x m) inputs)

                        Nothing ->
                            Err (ConstructorNotFound typeName name)
                )
                (checkT (NameT typeName typeInputs) m)
                (getTypeDefinition typeName m)

        Input typ ->
            Result.map (\_ -> expression)
                (checkT typ m)

        Load name ->
            Result.map (\_ -> expression)
                (getName name m)

        Lambda ( inputName, inputT ) output ->
            Result.map2 (\_ _ -> expression)
                (checkT inputT m)
                (Result.andThen (check output)
                    (addName inputName (Input inputT) m)
                )

        Call function input ->
            andThen2
                (\_ _ ->
                    case ( function, typeOf function m ) of
                        ( Lambda ( inputName, inputT ) output, _ ) ->
                            Result.map2 (\_ _ -> expression)
                                (typecheck input inputT m)
                                (Result.andThen (check output)
                                    (addName inputName input m)
                                )

                        ( _, Ok (LambdaT inputT _) ) ->
                            Result.map (\_ -> expression)
                                (typecheck input inputT m)

                        ( _, Ok _ ) ->
                            Err (CallNonFunction function input)

                        ( _, Err e ) ->
                            Err e
                )
                (check function m)
                (check input m)

        CaseOf ( input, outputT ) cases ->
            andThen2
                (\inputT _ ->
                    List.foldl
                        (checkCase input inputT outputT m)
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


checkCase : Expression -> Type -> Type -> Module -> ( Pattern, Expression ) -> Result Error ( List Pattern, List Case ) -> Result Error ( List Pattern, List Case )
checkCase input inputT outputT m ( pattern, output ) seenAndMissingResult =
    andThen3
        (\( seen, missingCases ) _ moduleWithPattern ->
            Result.andThen
                (\_ ->
                    if isCaseCovered pattern seen then
                        Err (CaseAlreadyCovered ( input, outputT ) ( pattern, output ))

                    else
                        Result.map
                            (\cases -> ( pattern :: seen, cases ))
                            (expandCases pattern missingCases m)
                )
                (typecheck output outputT moduleWithPattern)
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


withPattern : Pattern -> Module -> Result Error Module
withPattern pattern m =
    Result.andThen
        (\typ ->
            case pattern of
                AnyP _ ->
                    Ok m

                NameP p name ->
                    Result.andThen (addName name (Input typ))
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



-- TYPE OF EXPRESSION


typeOf : Expression -> Module -> Result Error Type
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

                Input typ ->
                    Ok typ

                Load name ->
                    Result.andThen (\e -> typeOf e m)
                        (getName name m)

                Lambda ( inputName, inputT ) output ->
                    Result.andThen
                        (\mod -> Result.map (LambdaT inputT) (typeOf output mod))
                        (addName inputName (Input inputT) m)

                Tuple items ->
                    Result.map TupleT
                        (andThenList (\x -> typeOf x m) items)

                Record items ->
                    Result.map RecordT
                        (andThenDict (\_ x -> typeOf x m) items)

                Constructor ( typeName, typeInputs ) _ _ ->
                    checkT (NameT typeName typeInputs) m

                Call function input ->
                    case typeOf function m of
                        Ok (LambdaT inputT outputT) ->
                            Result.map3 (\_ _ t -> t)
                                (checkT inputT m)
                                (typecheck input inputT m)
                                (checkT outputT m)

                        Ok _ ->
                            Err (CallNonFunction function input)

                        Err e ->
                            Err e

                CaseOf ( input, outputT ) cases ->
                    Result.andThen
                        (\inputT ->
                            List.foldl
                                (\( pattern, output ) prevType ->
                                    Result.map3 (\t _ _ -> t)
                                        prevType
                                        (typecheckP pattern inputT m)
                                        (typecheck output outputT m)
                                )
                                (checkT outputT m)
                                cases
                        )
                        (typeOf input m)
        )
        (check expression m)



-- TYPECHECK EXPRESSION


typecheck : Expression -> Type -> Module -> Result Error Expression
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


checkP : Pattern -> Module -> Result Error Pattern
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

        ConstructorP ( typeName, typeInputs ) name inputsP ->
            andThen2
                (\_ ( _, ctors ) ->
                    case Dict.get name ctors of
                        Just inputsT ->
                            Result.andThen
                                (\gotT ->
                                    if gotT == inputsT then
                                        Ok pattern

                                    else
                                        Err (ConstructorInputsMismatch typeName name { got = gotT, expected = inputsT })
                                )
                                (andThenList (\p -> typeOfP p m) inputsP)

                        Nothing ->
                            Err (ConstructorNotFound typeName name)
                )
                (checkT (NameT typeName typeInputs) m)
                (getTypeDefinition typeName m)



-- TYPE OF PATTERN


typeOfP : Pattern -> Module -> Result Error Type
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


typecheckP : Pattern -> Type -> Module -> Result Error Pattern
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



---=== CASES ===---
--
-- EXPAND CASES


expandCases : Pattern -> List Case -> Module -> Result Error (List Case)
expandCases pattern cases m =
    Result.map List.concat
        (andThenList (\c -> expandCase pattern c m) cases)



-- EXPAND CASE


expandCase : Pattern -> Case -> Module -> Result Error (List Case)
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
                                    (\( _, ctors ) ->
                                        Dict.map
                                            (\nameC inputsT -> ConstructorC ( typeName, typeInputs ) nameC (List.map AnyC inputsT))
                                            ctors
                                            |> Dict.values
                                            |> (\cases -> expandCases pattern cases m)
                                    )
                                    (getTypeDefinition typeName m)

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

        ConstructorC namedType name inputsC ->
            ConstructorP namedType name (List.map caseToPattern inputsC)
