module ASM exposing
    ( Accessor(..)
    , Condition(..)
    , Expr(..)
    , Type(..)
    , typeOf
    )

import Dict exposing (Dict)


type Type
    = TypeT
    | IntT
    | NumberT
    | TupleT (List Type)
    | RecordT (Dict String Type)
    | NameT String (List Expr)
    | FunctionT (List Type) Type


type Expr
    = Type Type
    | Int Int
    | Number Float
    | Tuple (List Expr)
    | Record (Dict String Expr)
    | Constructor ( String, List Expr ) String (List Expr)
    | Let (Dict String Expr) Expr
    | Load String Type
    | Function (Dict String Type) Expr
    | Call Expr (List Expr)
    | CaseOf ( Expr, Type ) (List ( List Condition, Dict String Accessor, Expr )) ( Dict String Accessor, Expr )


type Condition
    = EqualsType Accessor Type
    | EqualsInt Accessor Int
    | EqualsNumber Accessor Float
    | EqualsConstructor Accessor ( String, List Expr ) String


type Accessor
    = Self



-- | TupleItem Int
-- | ConstructorInput String
-- TYPE OF EXPRESSION


typeOf : Expr -> Type
typeOf expr =
    case expr of
        Type _ ->
            TypeT

        Int _ ->
            IntT

        Number _ ->
            NumberT

        Tuple items ->
            TupleT (List.map typeOf items)

        Record items ->
            RecordT (Dict.map (\_ -> typeOf) items)

        Constructor ( typeName, typeInputs ) _ _ ->
            NameT typeName typeInputs

        Let _ output ->
            typeOf output

        Load _ typ ->
            typ

        Function inputs output ->
            FunctionT (Dict.values inputs) (typeOf output)

        Call function _ ->
            case typeOf function of
                FunctionT _ outputT ->
                    outputT

                typ ->
                    typ

        CaseOf ( _, outputT ) _ _ ->
            outputT
