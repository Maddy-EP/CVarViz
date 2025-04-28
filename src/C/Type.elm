module C.Type exposing (Type(..), pretty, view)

import Html exposing (..)
import Parser exposing (..)


type Type
    = IntT
    | PointerT Type


pretty : Type -> String
pretty ct =
    case ct of
        IntT ->
            "int"

        PointerT ct_ ->
            pretty ct_ ++ "*"


view : Type -> Html msg
view t =
    text <| pretty t
