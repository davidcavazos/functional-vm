module FVM exposing
    ( Case(..)
    , Error(..)
    , Expression(..)
    , Module
    , Pattern(..)
    , Program
    , Type(..)
    , TypeDefinition
    , new
    )

import Dict exposing (Dict)



-- TYPES


type alias Program =
    { modules : Dict String Module
    , main : Maybe Expression
    }


type alias Module =
    { types : Dict String TypeDefinition
    , generics : Dict String Type
    , names : Dict String Expression
    }


type alias NamedType =
    ( String, List Expression )


type alias TypeDefinition =
    ( List Type, Dict String (List Type) )


type Expression
    = Type Type -- Int
    | Int Int -- 42
    | Number Float -- 3.14
    | Tuple (List Expression) -- (42, 3.14)
    | Record (Dict String Expression) -- (x = 1, y = 3.14)
    | Constructor ( String, List Expression ) String (List Expression) -- (Maybe a).Just 42
    | Input Type -- input for Lambda
    | Load String -- x
    | Lambda ( String, Type ) Expression -- (x : Int) -> x
    | Let (Dict String Expression) Expression -- let x = 1 in x + 1
    | Call Expression Expression -- (f : Int -> Int) 1
    | CaseOf ( Expression, Type ) (List ( Pattern, Expression )) -- case x -> Bool of 1 -> True; _ -> False


type Type
    = TypeT -- the type of any type
    | IntT -- Int
    | NumberT -- Number
    | NameT String (List Expression) -- Vector 0 Int
    | TupleT (List Type) -- (Int, Number)
    | RecordT (Dict String Type) -- (x: Int, y: Number)
    | LambdaT Type Type -- a -> b
    | GenericT String -- a
    | UnionT (List Type) -- Int | Number


type Pattern
    = AnyP Type -- _ : Int
    | NameP Pattern String -- _ as x
    | TypeP Type -- Int
    | IntP Int -- 42
    | NumberP Float -- 3.14
    | TupleP (List Pattern) -- (x, y, z)
    | RecordP (Dict String Type) -- {x : Int, y : Int, z : Int}
    | ConstructorP ( String, List Expression ) String (List Pattern) -- ((Maybe a).Just x)


type Case
    = AnyC Type
    | TupleC (List Case)
    | ConstructorC ( String, List Expression ) String (List Case)


type Error
    = CallNonFunction Expression Expression
    | CaseAlreadyCovered ( Expression, Type ) ( Pattern, Expression )
    | CasesMissing ( Expression, Type ) (List Case)
    | ConstructorInputsMismatch NamedType String { got : List Type, expected : List Type }
    | ConstructorNotFound NamedType String
    | GenericTIsAlreadyBound String
    | NameAlreadyExists String { got : Expression, existing : Expression }
    | NameNotFound String
    | PatternMismatch Pattern Type
    | TypeAlreadyExists String { got : TypeDefinition, existing : TypeDefinition }
    | TypeInputsMismatch String { got : List Type, expected : List Type }
    | TypeMismatch Expression Type
    | TypeNotFound String
    | VariableMismatch String Type Expression



-- NEW


new : Module
new =
    { types = Dict.empty
    , generics = Dict.empty
    , names = Dict.empty
    }
