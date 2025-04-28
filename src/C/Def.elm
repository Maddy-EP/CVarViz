module C.Def exposing (Def)

import C.Env exposing (Env)
import C.Exp exposing (Exp)
import C.Type exposing (Type)
import Parser exposing (..)


type alias Def =
    { typ : Type
    , name : String
    , rhs : Exp
    }
