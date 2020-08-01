module FVM.Util exposing
    ( andThen2
    , andThen3
    , andThenDict
    , andThenList
    , combinations
    , zip2
    )

import Dict exposing (Dict)
import Result


andThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
andThen2 f ra rb =
    Result.andThen (\a -> Result.andThen (\b -> f a b) rb) ra


andThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
andThen3 f ra rb rc =
    Result.andThen (\a -> andThen2 (\b c -> f a b c) rb rc) ra


andThenList : (a -> Result error b) -> List a -> Result error (List b)
andThenList f xs =
    List.foldl
        (\x prev -> Result.map2 (\ys y -> ys ++ [ y ]) prev (f x))
        (Ok [])
        xs


andThenDict : (comparable -> a -> Result error b) -> Dict comparable a -> Result error (Dict comparable b)
andThenDict f xs =
    Dict.foldl
        (\k x prev -> Result.map2 (\ys y -> Dict.insert k y ys) prev (f k x))
        (Ok Dict.empty)
        xs


zip2 : List a -> List b -> List ( a, b )
zip2 xs ys =
    List.map2 (\x y -> ( x, y )) xs ys


combinations : List (List a) -> List (List a)
combinations choices =
    case choices of
        [] ->
            []

        [ xs ] ->
            List.map (\x -> [ x ]) xs

        xs :: xss ->
            List.concatMap
                (\x ->
                    List.map (List.append [ x ])
                        (combinations xss)
                )
                xs
