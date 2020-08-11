module FVM.Type exposing (typeOf)

import Dict
import FVM exposing (Error(..), Expression(..), Type(..))



-- TYPE OF EXPRESSION


typeOf : Expression -> Type
typeOf expression =
    case expression of
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

        Lambda ( _, inputT ) output ->
            LambdaT inputT (typeOf output)

        Call function _ ->
            case typeOf function of
                LambdaT _ outputT ->
                    outputT

                typ ->
                    typ

        CaseOf ( _, outputT ) _ ->
            outputT
