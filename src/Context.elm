module Context exposing
    ( Context
    , andThen
    , call
    , caseOf
    , constructor
    , evaluate
    , evaluateMany
    , fail
    , followedBy
    , function
    , getType
    , int
    , lambda
    , load
    , new
    , number
    , orElse
    , record
    , succeed
    , tuple
    , typecheck
    , withInput
    , withName
    , withResult
    , withType
    , withTypeConstructor
    , withoutResult
    )

import AST exposing (..)
import Dict exposing (Dict)


type alias Context =
    { types : Dict String ( List Type, List String )
    , names : Dict String Expression
    , inputs : Dict String Type
    , generics : Dict String Type
    , result : Result Error Expression
    }


new : Context
new =
    { types = Dict.empty
    , names = Dict.empty
    , inputs = Dict.empty
    , generics = Dict.empty
    , result = Err NoResult
    }


withResult : Result Error Expression -> Context -> Context
withResult result context =
    { types = context.types
    , names = context.names
    , inputs = context.inputs
    , generics = context.generics
    , result = result
    }


withoutResult : Context -> Context
withoutResult context =
    withResult (Err NoResult) context



-- CONTROL FLOW


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


orElse : (Context -> Context) -> Context -> Context
orElse fallback context =
    case context.result of
        Ok _ ->
            context

        Err NoResult ->
            context

        _ ->
            fallback context


succeed : Expression -> Context -> Context
succeed expression context =
    followedBy (withResult (Ok expression)) context


fail : Error -> Context -> Context
fail error context =
    withResult (Err error) context



-- WITH NAME


withName : String -> Expression -> Context -> Context
withName name value context =
    case Dict.get name context.names of
        Just _ ->
            fail (NameAlreadyExists name) context

        Nothing ->
            case Dict.get name context.inputs of
                Just _ ->
                    fail (NameAlreadyExists name) context

                Nothing ->
                    { context
                        | result = context.result
                        , names = Dict.insert name value context.names
                    }



-- WITH INPUT


withInput : String -> Type -> Context -> Context
withInput name typ context =
    case Dict.get name context.inputs of
        Just _ ->
            fail (NameAlreadyExists name) context

        Nothing ->
            case Dict.get name context.names of
                Just _ ->
                    fail (NameAlreadyExists name) context

                Nothing ->
                    { context | inputs = Dict.insert name typ context.inputs }



-- WITH TYPE


withType : String -> List Type -> Context -> Context
withType name inputTypes context =
    if Dict.member name context.types then
        fail (TypeAlreadyExists name) context

    else
        { context | types = Dict.insert name ( inputTypes, [] ) context.types }



-- WITH TYPE CONSTRUCTOR


withTypeConstructor : ( String, List (Context -> Context) ) -> String -> List ( String, Type ) -> Context -> Context
withTypeConstructor ( typeName, typeInputs ) name inputs context =
    context
        |> constructor ( typeName, typeInputs )
            name
            (List.map (\( n, _ ) -> load n) inputs)
        |> andThen (withName name)
        |> followedBy
            (\ctx ->
                case Dict.get typeName ctx.types of
                    Just ( inputTypes, constructors ) ->
                        { ctx
                            | result = context.result
                            , types =
                                Dict.insert typeName
                                    ( inputTypes, constructors ++ [ name ] )
                                    ctx.types
                        }

                    Nothing ->
                        -- Unreachable since it was already type checked at `constructor`
                        fail (TypeNotFound typeName) ctx
            )



-- LOAD


load : String -> Context -> Context
load name context =
    case Dict.get name context.names of
        Just value ->
            succeed value context

        Nothing ->
            case Dict.get name context.inputs of
                Just typ ->
                    succeed (Input name typ) context

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
            record (Dict.toList (Dict.map (\_ -> evaluate) namedItems)) context

        Input name typ ->
            load name context |> andThen (typecheck typ)

        Lambda input output ->
            lambda input (evaluate output) context

        Constructor ( typeName, typeInputs ) name inputs ->
            constructor
                ( typeName, List.map evaluate typeInputs )
                name
                (List.map evaluate inputs)
                context



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



-- INT


int : Int -> Context -> Context
int value context =
    succeed (Integer value) context



-- NUMBER


number : Float -> Context -> Context
number value context =
    succeed (Number value) context



-- TUPLE


tuple : List (Context -> Context) -> Context -> Context
tuple items context =
    evaluateMany items
        (\resultItems -> succeed (Tuple resultItems))
        context



-- RECORD


record : List ( String, Context -> Context ) -> Context -> Context
record namedItems context =
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
                (Dict.fromList namedItems)
    in
    succeed (Record resultItems) resultContext



-- LAMBDA


lambda : ( String, Type ) -> (Context -> Context) -> Context -> Context
lambda ( name, typ ) body context =
    context
        |> withInput name typ
        |> followedBy body
        |> andThen (\result _ -> succeed (Lambda ( name, typ ) result) context)



-- FUNCTION


function : List ( String, Type ) -> (Context -> Context) -> Context -> Context
function inputs body context =
    List.foldr
        (\input ->
            andThen (\output _ -> succeed (Lambda input output) context)
        )
        (Dict.foldl withInput context (Dict.fromList inputs) |> body)
        inputs



-- CONSTRUCTOR


constructor : ( String, List (Context -> Context) ) -> String -> List (Context -> Context) -> Context -> Context
constructor ( typeName, typeInputs ) name inputs context =
    evaluateMany typeInputs
        (\evalTypeInputs ->
            evaluateMany inputs
                (\evalInputs ctx ->
                    ctx
                        |> succeed (Constructor ( typeName, evalTypeInputs ) name evalInputs)
                        |> andThen (typecheck (NamedType typeName evalTypeInputs))
                )
        )
        context



-- CALL


call : List (Context -> Context) -> Expression -> Context -> Context
call inputs expression context =
    List.foldl
        (\input ->
            andThen
                (\expr ctx ->
                    case expr of
                        Lambda ( inputName, inputType ) output ->
                            input ctx
                                |> andThen (typecheck inputType)
                                |> andThen (withName inputName)
                                |> followedBy (evaluate output)

                        _ ->
                            evaluateMany inputs
                                (\xs -> fail (CallTooManyInputs expression xs))
                                ctx
                )
        )
        context
        inputs
        |> andThen (\result _ -> withResult (Ok result) context)



-- CASE OF (PATTERN MATCHING)


caseOf : List ( Pattern, Context -> Context ) -> Expression -> Context -> Context
caseOf cases expression context =
    if List.isEmpty cases then
        fail (CaseWithoutPatterns expression) context

    else
        Debug.todo "caseOf"



-- TYPECHECK


typecheck : Type -> Expression -> Context -> Context
typecheck typ expression context =
    case ( typ, expression ) of
        ( GenericType name, _ ) ->
            case Dict.get name context.generics of
                Just t ->
                    typecheck t expression context

                Nothing ->
                    { context
                        | generics =
                            Dict.insert name
                                (getType expression)
                                context.generics
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
        Just ( typeInputTypes, _ ) ->
            if List.length typeInputs /= List.length typeInputTypes then
                fail (TypeInputsMismatch typeName { got = typeInputs, expected = typeInputTypes }) context

            else
                List.foldl
                    (\( t, e ) -> typecheck t e)
                    context
                    (List.map2 (\t e -> ( t, e )) typeInputTypes typeInputs)
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

        Input _ typ ->
            typ

        Lambda ( _, inputType ) output ->
            LambdaType inputType (getType output)

        Constructor ( typeName, typeInputs ) _ _ ->
            NamedType typeName typeInputs
