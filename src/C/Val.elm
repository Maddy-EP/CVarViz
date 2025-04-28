module C.Val exposing (Val(..), view)

import Html exposing (..)
import String


type Val
    = I Int
    | P (Maybe Int)


view : Val -> Html msg
view v =
    case v of
        I i ->
            text <| String.fromInt i

        P Nothing ->
            text "null"

        P (Just i) ->
            text <| String.fromInt i
