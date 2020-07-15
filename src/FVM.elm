module FVM exposing
    ( Context
    , Error(..)
    , Expression(..)
    , Module
    , Pattern(..)
    , Program
    , Type(..)
    , TypeDefinition
    )

import Dict exposing (Dict)



-- TYPES


type alias Program =
    { modules : Dict String Module
    , main : Maybe Expression
    }


type alias Module =
    { types : Dict String (Result Error TypeDefinition)
    , values : Dict String (Result Error Expression)
    }


type alias Context =
    { generics : Dict String Type
    , variables : Dict String Type
    }


type alias TypeDefinition =
    ( List Type, Dict String (List Type) )


type Expression
    = Type Type -- Int
    | Integer Int -- 42
    | Number Float -- 3.14
    | Variable String Type -- (x : Int)
    | Lambda ( String, Type ) Expression -- (x : Int) -> x
    | Tuple (List Expression) -- (42, 3.14)
    | Record (Dict String Expression) -- (x = 1, y = 3.14)
    | Constructor ( String, List Expression ) String (List Expression) -- (Maybe a).Just 42
    | Call Expression Expression -- (f : Int -> Int) 1
    | Match Expression (List ( Pattern, Expression )) Type -- case x of 1 -> True; _ -> False


type Type
    = TypeType -- the type of any type
    | IntType -- Int
    | NumberType -- Number
    | NamedType ( String, List Expression ) -- Vector 0 Int
    | LambdaType Type Type -- a -> b
    | TupleType (List Type) -- (Int, Number)
    | RecordType (Dict String Type) -- (x: Int, y: Number)
    | GenericType String -- a
    | UnionType (List Type) -- Int | Number


type Pattern
    = TypePattern Type -- Int
    | IntPattern Int -- 42
    | NumberPattern Float -- 3.14
    | TuplePattern (List Pattern) (Maybe String) -- (x, y, z) as t
    | RecordPattern (Dict String Pattern) (Maybe String) -- {x = 1, y = y, z = _} as r
    | ConstructorPattern ( String, List Expression ) String (List Pattern) (Maybe String) -- ((Maybbe Int).Just x) as c
    | AnythingPattern (Maybe String) -- _ x


type Error
    = CallNonFunction Expression Expression
    | ConstructorInputsMismatch ( String, List Expression ) String { got : List Expression, expected : List Type }
    | ConstructorNotFound ( String, List Expression ) String
    | GenericTypeIsAlreadyBound String
    | NameAlreadyExists String { got : Expression, existing : Expression }
    | NameNotFound String
    | TypeAlreadyExists String { got : TypeDefinition, existing : TypeDefinition }
    | TypeInputsMismatch String { got : List Expression, expected : List Type }
    | TypeMismatch Expression Type
    | TypeNotFound String
