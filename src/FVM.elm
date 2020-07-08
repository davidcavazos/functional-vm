module FVM exposing
    ( Context
    , Error(..)
    , Expression(..)
    , NameDefinition(..)
    , Pattern(..)
    , Type(..)
    , TypeDefinition(..)
    , andThen
    , call
    , constructor
    , evaluate
    , evaluateMany
    , fail
    , followedBy
    , function
    , getType
    , ifNoResult
    , ifThen
    , int
    , lambda
    , load
    , matchInto
    , new
    , number
    , orElse
    , record
    , saveInput
    , saveMaybeName
    , saveName
    , saveTaggedUnionType
    , succeed
    , tuple
    , typecheck
    , withPattern
    , withResult
    , withoutResult
    )

import Dict exposing (Dict)



-- TYPES


type TypeDefinition
    = TypeAlias Type
    | BoundGeneric Type
    | TaggedUnionType (List Type) (Dict String (List Type))


type NameDefinition
    = Input Type
    | Value Expression


type alias Context =
    { types : Dict String TypeDefinition
    , names : Dict String NameDefinition
    , result : Result Error Expression
    }


type Expression
    = Type Type -- Int
    | Integer Int -- 42
    | Number Float -- 3.14
    | Tuple (List Expression) -- (42, 3.14)
    | Record (Dict String Expression) -- (x = 1, y = 3.14)
    | Constructor ( String, List Expression ) String (List Expression) -- (Maybe a).Just 42
    | Lambda ( String, Type ) Expression -- (x : Int) -> x
    | Load String Type -- (x : Int)



-- | Call String Expression Expression -- ((x : Int) -> x) 42
-- | Match String (List ( Pattern, Context -> Context )) Type


type Type
    = TypeType -- the type of any type
    | IntType -- Int
    | NumberType -- Number
    | TupleType (List Type) -- (Int, Number)
    | RecordType (Dict String Type) -- (x: Int, y: Number)
    | LambdaType Type Type -- a -> b
    | GenericType String -- a
    | NamedType String (List Expression) -- Vector 0 Int


type Pattern
    = TypePattern Type -- Int
    | IntPattern Int -- 42
    | NumberPattern Float -- 3.14
    | TuplePattern (List Pattern) (Maybe String) -- (x, y, z) as t
    | RecordPattern (Dict String Pattern) (Maybe String) -- {x = 1, y = y, z = _} as r
    | ConstructorPattern ( String, List Expression ) String (List Pattern) (Maybe String) -- ((Maybbe Int).Just x) as c
    | AnythingPattern (Maybe String) -- _ x


type Error
    = NoResult
    | CallNonFunction Expression
    | CallTooManyInputs Expression (List Expression)
    | ConstructorInputsMismatch String String { got : List Expression, expected : List Type }
    | ConstructorNotFound String String
    | ConstructorNotOfTaggedUnionType String TypeDefinition
    | MatchCaseAlreadyCovered Pattern
    | MatchConstructorNotFound ( String, List Expression ) String
    | MatchMissingCases (List Pattern)
    | MatchPatternTypeMismatch Expression Pattern
    | NameAlreadyExists String
    | NameNotFound String
    | TypeAlreadyExists String TypeDefinition
    | TypeInputsMismatch String { got : List Expression, expected : List Type }
    | TypeMismatch Expression Type
    | TypeNotFound String



-- CONTROL FLOW


new : Context
new =
    { types = Dict.empty
    , names = Dict.empty
    , result = Err NoResult
    }


withResult : Result Error Expression -> Context -> Context
withResult result context =
    { types = context.types
    , names = context.names
    , result = result
    }


withoutResult : Context -> Context
withoutResult context =
    withResult (Err NoResult) context


andThen : (Expression -> Context -> Context) -> Context -> Context
andThen f context =
    case context.result of
        Ok result ->
            f result context

        _ ->
            context


followedBy : (Context -> Context) -> Context -> Context
followedBy f context =
    case context.result of
        Ok _ ->
            f context

        Err NoResult ->
            f context

        Err e ->
            fail e context


ifThen : Bool -> (Context -> Context) -> Context -> Context
ifThen condition thenDo context =
    if condition then
        thenDo context

    else
        context


ifNoResult : (Context -> Context) -> Context -> Context
ifNoResult thenDo context =
    case context.result of
        Err NoResult ->
            thenDo context

        _ ->
            context


orElse : (Context -> Context) -> Context -> Context
orElse fallback context =
    case context.result of
        Ok _ ->
            context

        Err NoResult ->
            context

        Err _ ->
            fallback context


succeed : Expression -> Context -> Context
succeed expression context =
    followedBy (withResult (Ok expression)) context


fail : Error -> Context -> Context
fail error context =
    withResult (Err error) context



-- SAVE NAME


saveName : String -> Expression -> Context -> Context
saveName name value context =
    case Dict.get name context.names of
        Just _ ->
            fail (NameAlreadyExists name) context

        Nothing ->
            { context
                | names = Dict.insert name (Value value) context.names
            }



-- SAVE MAYBE NAME


saveMaybeName : Maybe String -> Expression -> Context -> Context
saveMaybeName maybeName value context =
    case maybeName of
        Just name ->
            saveName name value context

        Nothing ->
            context



-- SAVE INPUT


saveInput : String -> Type -> Context -> Context
saveInput name typ context =
    case Dict.get name context.names of
        Just _ ->
            fail (NameAlreadyExists name) context

        Nothing ->
            { context
                | names = Dict.insert name (Input typ) context.names
            }



-- SAVE TAGGED UNION TYPE


saveTaggedUnionType : ( String, List Type ) -> Dict String ( List ( String, Type ), List (Context -> Context) ) -> Context -> Context
saveTaggedUnionType ( typeName, typeInputTypes ) constructors context =
    case Dict.get typeName context.types of
        Just tdef ->
            fail (TypeAlreadyExists typeName tdef) context

        Nothing ->
            let
                ctors =
                    Dict.foldl
                        (\name ( inputs, _ ) prevCtors ->
                            Dict.insert name (List.map Tuple.second inputs) prevCtors
                        )
                        Dict.empty
                        constructors
            in
            Dict.foldl
                (\name ( inputs, typeInputs ) ->
                    saveConstructor ( typeName, typeInputs ) name inputs
                )
                { context
                    | types =
                        Dict.insert typeName
                            (TaggedUnionType typeInputTypes ctors)
                            context.types
                }
                constructors
                |> orElse (\ctx -> withResult ctx.result context)


saveConstructor : ( String, List (Context -> Context) ) -> String -> List ( String, Type ) -> Context -> Context
saveConstructor namedType name inputs context =
    context
        |> function inputs
            (constructor namedType
                name
                (List.map (\( inputName, _ ) -> load inputName) inputs)
            )
        |> andThen (saveName name)
        |> followedBy (withResult context.result)



-- WITH PATTERN


withPattern : Pattern -> Expression -> Context -> Context
withPattern pattern input context =
    case ( pattern, input ) of
        ( TypePattern pType, Type typ ) ->
            evaluate input context
                |> ifThen (pType /= typ) withoutResult

        ( IntPattern pValue, Integer value ) ->
            evaluate input context
                |> ifThen (pValue /= value) withoutResult

        ( NumberPattern pValue, Number value ) ->
            evaluate input context
                |> ifThen (pValue /= value) withoutResult

        ( TuplePattern pItems maybeName, Tuple items ) ->
            if List.length pItems /= List.length items then
                fail (MatchPatternTypeMismatch input pattern) context

            else
                List.foldl
                    (\( pItem, item ) prevCtx ->
                        prevCtx
                            |> andThen (\_ -> withPattern pItem item)
                            |> andThen (\got -> ifThen (got /= item) withoutResult)
                    )
                    (tuple [] context)
                    (zip pItems items)
                    |> andThen (\_ -> evaluate input)
                    |> andThen (saveMaybeName maybeName)

        ( RecordPattern pItems maybeName, Record items ) ->
            Dict.foldl
                (\pName pItem prevCtx ->
                    case Dict.get pName items of
                        Just item ->
                            withPattern pItem item prevCtx
                                |> andThen (saveName pName)

                        Nothing ->
                            fail (MatchPatternTypeMismatch input pattern) prevCtx
                )
                (record Dict.empty context)
                pItems
                |> andThen (\_ -> evaluate input)
                |> andThen (saveMaybeName maybeName)

        ( ConstructorPattern pNamedType pName pInputs maybeName, Constructor namedType name inputs ) ->
            if pNamedType /= namedType || pName /= name || List.length pInputs /= List.length inputs then
                fail (MatchPatternTypeMismatch input pattern) context

            else
                evaluate input context

        ( AnythingPattern maybeName, _ ) ->
            context
                |> saveMaybeName maybeName input
                |> evaluate input

        _ ->
            fail (MatchPatternTypeMismatch input pattern) context



-- LOAD


load : String -> Context -> Context
load name context =
    case Dict.get name context.names of
        Just (Input typ) ->
            succeed (Load name typ) context

        Just (Value value) ->
            succeed value context

        Nothing ->
            fail (NameNotFound name) context



-- EVALUATE


evaluate : Expression -> Context -> Context
evaluate expression context =
    case expression of
        Type _ ->
            -- TODO: make a wrapper function for Type
            succeed expression context

        Integer value ->
            int value context

        Number value ->
            number value context

        Tuple items ->
            tuple (List.map evaluate items) context

        Record namedItems ->
            record (Dict.map (\_ -> evaluate) namedItems) context

        Constructor ( typeName, typeInputs ) name inputs ->
            constructor
                ( typeName, List.map evaluate typeInputs )
                name
                (List.map evaluate inputs)
                context

        Lambda input output ->
            lambda input (evaluate output) context

        Load name typ ->
            load name context |> andThen (typecheck typ)



-- EVALUATE MANY


evaluateMany : List (Context -> Context) -> (List Expression -> Context -> Context) -> Context -> Context
evaluateMany items f context =
    let
        ( resultItems, resultContext ) =
            List.foldl
                (\item ( prevItems, prevCtx ) ->
                    let
                        ctx =
                            item prevCtx
                    in
                    case ctx.result of
                        Ok result ->
                            ( prevItems ++ [ result ], ctx )

                        Err _ ->
                            ( [], ctx )
                )
                ( [], context )
                items
    in
    f resultItems resultContext



-- EXPRESSION - INT


int : Int -> Context -> Context
int value context =
    succeed (Integer value) context



-- EXPRESSION - NUMBER


number : Float -> Context -> Context
number value context =
    succeed (Number value) context



-- EXPRESSION - TUPLE


tuple : List (Context -> Context) -> Context -> Context
tuple items context =
    evaluateMany items
        (\resultItems -> succeed (Tuple resultItems))
        context



-- EXPRESSION - RECORD


record : Dict String (Context -> Context) -> Context -> Context
record items context =
    -- TODO: use evaluateMany
    let
        ( resultItems, resultContext ) =
            Dict.foldl
                (\name item ( prevItems, prevCtx ) ->
                    let
                        ctx =
                            item prevCtx
                    in
                    case ctx.result of
                        Ok result ->
                            ( Dict.insert name result prevItems, ctx )

                        Err _ ->
                            ( Dict.empty, ctx )
                )
                ( Dict.empty, context )
                items
    in
    succeed (Record resultItems) resultContext



-- EXPRESSION - LAMBDA


lambda : ( String, Type ) -> (Context -> Context) -> Context -> Context
lambda ( name, typ ) body context =
    context
        |> saveInput name typ
        |> followedBy body
        |> andThen (\result _ -> succeed (Lambda ( name, typ ) result) context)



-- EXPRESSION - FUNCTION


function : List ( String, Type ) -> (Context -> Context) -> Context -> Context
function inputs body context =
    List.foldr
        (\input ->
            andThen (\output _ -> succeed (Lambda input output) context)
        )
        (Dict.foldl saveInput context (Dict.fromList inputs) |> body)
        inputs



-- EXPRESSION - CONSTRUCTOR


constructor : ( String, List (Context -> Context) ) -> String -> List (Context -> Context) -> Context -> Context
constructor ( typeName, typeInputs ) name inputs context =
    evaluateMany typeInputs
        (\evalTypeInputs ->
            evaluateMany inputs
                (validatedConstructor ( typeName, evalTypeInputs ) name)
        )
        context


validatedConstructor : ( String, List Expression ) -> String -> List Expression -> Context -> Context
validatedConstructor ( typeName, typeInputs ) name inputs context =
    case Dict.get typeName context.types of
        Just (TaggedUnionType _ ctors) ->
            case Dict.get name ctors of
                Just inputTypes ->
                    if List.length inputTypes /= List.length inputs then
                        fail (ConstructorInputsMismatch typeName name { got = inputs, expected = inputTypes }) context

                    else
                        List.foldl
                            (\( typ, input ) -> typecheck typ input)
                            context
                            (zip inputTypes inputs)
                            |> succeed (Constructor ( typeName, typeInputs ) name inputs)
                            |> andThen (typecheck (NamedType typeName typeInputs))

                Nothing ->
                    fail (ConstructorNotFound typeName name) context

        Just tdef ->
            fail (ConstructorNotOfTaggedUnionType typeName tdef) context

        Nothing ->
            fail (TypeNotFound typeName) context



-- EXPRESSION - CALL


call : List (Context -> Context) -> Expression -> Context -> Context
call inputs expression context =
    List.foldl
        (\input -> andThen (callLambda input))
        (succeed expression context)
        inputs
        |> andThen (\result _ -> succeed result context)
        -- User experience: better error messages
        |> orElse
            (\ctx ->
                case ( expression, ctx.result ) of
                    ( Lambda _ _, Err (CallNonFunction _) ) ->
                        evaluateMany inputs
                            (\args -> fail (CallTooManyInputs expression args))
                            ctx

                    _ ->
                        ctx
            )


callLambda : (Context -> Context) -> Expression -> Context -> Context
callLambda input expression context =
    case expression of
        Lambda ( inputName, inputType ) output ->
            input context
                |> andThen (typecheck inputType)
                |> andThen (saveName inputName)
                |> followedBy (evaluate output)

        _ ->
            fail (CallNonFunction expression) context



-- MATCH INTO (PATTERN MATCHING)


matchInto : Type -> List ( Pattern, Context -> Context ) -> Expression -> Context -> Context
matchInto outputType cases input context =
    List.foldl
        (matchCase outputType input)
        (withoutResult context)
        cases
        |> ifNoResult (fail (MatchMissingCases (List.map Tuple.first cases)))


matchCase : Type -> Expression -> ( Pattern, Context -> Context ) -> Context -> Context
matchCase outputType input ( pattern, do ) context =
    context
        |> withPattern pattern input
        |> (\ctx ->
                case ( context.result, ctx.result ) of
                    -- If there was already a result and we got one,
                    -- that case was already covered by a previous pattern.
                    ( Ok _, Ok _ ) ->
                        fail (MatchCaseAlreadyCovered pattern) ctx

                    -- If we got a result, evaluate the body and keep the result.
                    ( _, Ok _ ) ->
                        do ctx |> andThen (typecheck outputType)

                    -- If we didn't get a result, evalauate the body but discard it.
                    ( _, Err NoResult ) ->
                        do ctx
                            |> andThen (typecheck outputType)
                            |> andThen (\_ -> withoutResult)

                    -- If we got an error, carry it over.
                    ( _, Err _ ) ->
                        ctx
           )
        |> andThen (\result _ -> succeed result context)



-- TYPECHECK


typecheck : Type -> Expression -> Context -> Context
typecheck typ expression context =
    case ( typ, expression ) of
        ( GenericType name, _ ) ->
            case Dict.get name context.types of
                Just (TypeAlias t) ->
                    Debug.todo "typecheck TypeAlias"

                Just (BoundGeneric t) ->
                    typecheck t expression context

                Just (TaggedUnionType _ _) ->
                    Debug.todo "typecheck TaggedUnionType"

                Nothing ->
                    { context
                        | types =
                            Dict.insert name
                                (BoundGeneric (getType expression))
                                context.types
                    }

        ( NamedType typeName typeInputs, Constructor _ _ _ ) ->
            if getType expression /= typ then
                fail (TypeMismatch expression typ) context

            else
                typecheckNamedType typeName typeInputs context

        _ ->
            if getType expression /= typ then
                fail (TypeMismatch expression typ) context

            else
                context


typecheckNamedType : String -> List Expression -> Context -> Context
typecheckNamedType typeName typeInputs context =
    case Dict.get typeName context.types of
        Just (TypeAlias _) ->
            Debug.todo "typecheckNamedType TypeAlias"

        Just (BoundGeneric _) ->
            Debug.todo "typecheckNamedType BoundGeneric"

        Just (TaggedUnionType typeInputTypes _) ->
            if List.length typeInputs /= List.length typeInputTypes then
                fail (TypeInputsMismatch typeName { got = typeInputs, expected = typeInputTypes }) context

            else
                List.foldl
                    (\( t, e ) -> typecheck t e)
                    context
                    (zip typeInputTypes typeInputs)
                    |> orElse (fail (TypeInputsMismatch typeName { got = typeInputs, expected = typeInputTypes }))

        Nothing ->
            fail (TypeNotFound typeName) context



-- GET TYPE


getType : Expression -> Type
getType expression =
    case expression of
        Type _ ->
            TypeType

        Integer _ ->
            IntType

        Number _ ->
            NumberType

        Tuple items ->
            TupleType (List.map getType items)

        Record namedItems ->
            RecordType (Dict.map (\_ -> getType) namedItems)

        Constructor ( typeName, typeInputs ) _ _ ->
            NamedType typeName typeInputs

        Lambda ( _, inputType ) output ->
            LambdaType inputType (getType output)

        Load _ typ ->
            typ



-- HELPER


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.map2 (\x y -> ( x, y )) xs ys
