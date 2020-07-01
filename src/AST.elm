module AST exposing
    ( Error(..)
    , Expression(..)
    , Pattern(..)
    , Type(..)
    )

import Dict exposing (Dict)


type Expression
    = Type Type -- Int
    | Integer Int -- 42
    | Number Float -- 3.14
      -- | Text String Type -- 'hello'
      -- | Array Type Int -- [Int: 42]
    | Tuple (List Expression) -- (42, 3.14)
    | Record (Dict String Expression) -- (x = 1, y = 3.14)
    | Input String Type -- (x : Int)
    | Lambda ( String, Type ) Expression -- (x : Int) -> x
    | Constructor ( String, List Expression ) String (List Expression) -- (Maybe a).Just 42


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
    = Anything -- _
    | Name String -- x
    | TuplePattern (List Pattern) (Maybe String) -- (x, (y, z) as yz) as xyz
    | RecordPattern (List ( Pattern, Maybe String )) (Maybe String) -- {x as x, {y as y, z as z} as yz} as xyz
    | ConstructorPattern ( String, List Expression ) String (List Pattern) -- (Maybe Int).Just x


type Error
    = NoResult
    | CallTooManyInputs Expression (List Expression)
    | CaseWithoutPatterns Expression
    | NameAlreadyExists String
    | NameNotFound String
    | TypeAlreadyExists String
    | TypeInputsMismatch String { got : List Expression, expected : List Type }
    | TypeMismatch Expression Type
    | TypeNotFound String



-- type Builtin
--     = IntAdd Int Int -- 1 + 2
--     | IntSub Int Int -- 1 - 2
--     | IntMul Int Int -- 1 * 2
--     | IntDiv Int Int -- 1 / 2
--     | IntRem Int Int -- 1 % 2
--     | IntShl Int Int -- 1 << 2
--     | IntShr Int Int -- 1 >> 2
--     | IntAnd Int Int -- 1 & 2
--     | IntOr Int Int -- 1 | 2
--     | IntXor Int Int -- 1 ^ 2
--     | NumberNeg Float -- -1.0
--     | NumberAdd Float Float -- 1.0 + 2.0
--     | NumberSub Float Float -- 1.0 - 2.0
--     | NumberMul Float Float -- 1.0 * 2.0
--     | NumberDiv Float Float -- 1.0 / 2.0
--     | NumberRem Float Float -- 1.0 % 2.0
