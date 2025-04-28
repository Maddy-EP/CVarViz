module C.Val exposing (Val(..), pretty, view)

import Html exposing (..)
import String


type Val
    = I Int
    | P (Maybe Int)


pretty : Val -> String
pretty v =
    case v of
        I i ->
            String.fromInt i

        P Nothing ->
            "null"

        P (Just i) ->
            String.fromInt i


view : Val -> Html msg
view v =
    text <| pretty v
