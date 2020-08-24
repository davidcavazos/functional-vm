module ASM exposing
    ( Accessor(..)
    , Condition(..)
    , Expression(..)
    , Package
    , Type(..)
    , typeOf
    )

import Dict exposing (Dict)


type alias Package =
    { types : Dict String ( List Type, Dict String (List Type) )
    , names : Dict String Expression
    }


type Type
    = TypeT
    | IntT
    | NumberT
    | TupleT (List Type)
    | RecordT (Dict String Type)
    | NameT String (List Expression)
    | FunctionT (List Type) Type
    | GenericT String
    | UnionT (List Type)


type Expression
    = Type Type
    | Int Int
    | Number Float
    | Tuple (List Expression)
    | Record (Dict String Expression)
    | Constructor ( String, List Expression ) String (List Expression)
    | Let (Dict String Expression) Expression
    | Load String Type
    | Function (Dict String Type) Expression
    | Call Expression (List Expression)
    | CaseOf ( Expression, Type ) (List ( List Condition, Dict String Accessor, Expression )) ( Dict String Accessor, Expression )


type Condition
    = EqualsType Accessor Type
    | EqualsInt Accessor Int
    | EqualsNumber Accessor Float
    | EqualsConstructor Accessor ( String, List Expression ) String


type Accessor
    = Self
    | TupleItem Int
    | RecordField String
    | ConstructorInput Int



-- TYPE OF EXPRESSION


typeOf : Expression -> Type
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
