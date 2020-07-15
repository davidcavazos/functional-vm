module FVM.Util exposing
    ( andThen2
    , mapDict
    , mapList
    , zip2
    )

import Dict exposing (Dict)
import FVM exposing (Context, Module)
import Result


mapDict : (comparable -> a -> Context -> Module -> Result error b) -> Dict comparable a -> Context -> Module -> Result error (Dict comparable b)
mapDict f xs ctx m =
    Dict.foldl
        (\k x prev -> Result.map2 (\ys y -> Dict.insert k y ys) prev (f k x ctx m))
        (Ok Dict.empty)
        xs


mapList : (a -> Context -> Module -> Result error b) -> List a -> Context -> Module -> Result error (List b)
mapList f xs ctx m =
    List.foldl
        (\x prev -> Result.map2 (\ys y -> ys ++ [ y ]) prev (f x ctx m))
        (Ok [])
        xs


andThen2 : (a -> b -> Result error c) -> Result error a -> Result error b -> Result error c
andThen2 f rx ry =
    Result.andThen (\x -> Result.andThen (\y -> f x y) ry) rx


zip2 : List a -> List b -> List ( a, b )
zip2 xs ys =
    List.map2 (\x y -> ( x, y )) xs ys
