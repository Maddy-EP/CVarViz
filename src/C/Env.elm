module C.Env exposing (Env, emptyEnv, insert, lookupAddr, lookupType, lookupValue, view)

import C.Type exposing (Type)
import C.Val exposing (Val)
import Dict as D exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (hidden)
import List as L
import Result as R
import String as S


type alias Env =
    { types : Dict String Type
    , addrs : Dict String Int
    , store : Dict Int Val
    , nextAddr : Int
    }


emptyEnv : Env
emptyEnv =
    Env D.empty D.empty D.empty 0


insert : Env -> Type -> String -> Val -> Result String Env
insert env t n v =
    case D.get n env.types of
        Just t_ ->
            if t_ /= t then
                Err <| "type mismatch: the variable '" ++ n ++ "' is already defined to have type " ++ C.Type.pretty t_ ++ ", but this definition declares it to have type " ++ C.Type.pretty t

            else
                lookupAddr env n
                    |> R.andThen
                        (\a ->
                            Ok { env | store = D.insert a v env.store }
                        )

        Nothing ->
            Ok
                { env
                    | types = D.insert n t env.types
                    , addrs = D.insert n env.nextAddr env.addrs
                    , store = D.insert env.nextAddr v env.store
                    , nextAddr = env.nextAddr + 1
                }


lookupType : Env -> String -> Result String Type
lookupType env name =
    D.get name env.types
        |> Result.fromMaybe
            ("'" ++ name ++ "' has not been defined")


lookupAddr : Env -> String -> Result String Int
lookupAddr env name =
    D.get name env.addrs
        |> Result.fromMaybe
            ("'" ++ name ++ "' has not been defined")


lookupValue : Env -> String -> Result String Val
lookupValue env name =
    lookupAddr env name
        |> Result.andThen
            (\addr -> D.get addr env.store |> Result.fromMaybe "")


view : Env -> Html msg
view env =
    let
        rows =
            L.reverse <|
                D.merge
                    (\_ _ rs -> rs)
                    (\n t a rs -> tr [] [ td [] [ text n ], td [] [ text <| S.fromInt a ], td [] <| [ C.Type.view t ], td [] [ R.withDefault (text "???") (lookupValue env n |> R.andThen (\v -> Ok <| C.Val.view v)) ] ] :: rs)
                    (\_ _ rs -> rs)
                    env.types
                    env.addrs
                    []
    in
    table [ hidden <| L.isEmpty rows ] <|
        [ thead
            []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Address" ]
                , th [] [ text "Type" ]
                , th [] [ text "Value" ]
                ]
            ]
        ]
            ++ rows
