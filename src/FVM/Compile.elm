module FVM.Compile exposing
    ( checkTypeDefinition
      -- , compile
    )

import FVM exposing (Error(..), Expression(..), Module, Type(..), TypeDefinition)
import FVM.Util exposing (andThenDict, andThenList)
import FVM.Validate exposing (checkT)
import Result



-- COMPILE
-- compile : Module -> Module
-- compile mod =
--     let
--         check : (a -> Module -> Result Error a) -> Module -> String -> Result Error a -> Result Error a
--         check f m name result =
--             Result.andThen (\x -> f x m) result
--     in
--     mod
--         |> (\m -> { m | types = Dict.map (check checkTDefinition m) m.types })
--         |> (\m -> { m | names = Dict.map (check evaluate m) m.names })
-- CHECK TYPE DEFINITION


checkTypeDefinition : TypeDefinition -> Module -> Result Error TypeDefinition
checkTypeDefinition ( typeInputTypes, constructors ) m =
    Result.map2 (\typeInputs ctors -> ( typeInputs, ctors ))
        (andThenList (\t -> checkT t m) typeInputTypes)
        (andThenDict (\_ -> andThenList (\t -> checkT t m)) constructors)
