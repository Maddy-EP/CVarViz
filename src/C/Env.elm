module C.Env exposing (Env, emptyEnv, insert, lookupAddr, lookupType, lookupValue, lookupValueAtAddr, toGraphDot, updateAtAddr, view)

import C.Type exposing (Type)
import C.Val exposing (Val)
import Dict as D exposing (Dict)
import Graph as G exposing (Edge, Graph, Node)
import Graph.DOT as GDOT
import Html exposing (..)
import Html.Attributes exposing (hidden)
import List as L
import Maybe as M
import Result as R
import String as S


type alias Env =
    { types : Dict String Type
    , addrs : Dict String Int
    , store : Dict Int Val
    , nextAddr : Int
    }


envNodes : Env -> List (Node String)
envNodes env =
    D.merge
        (\_ _ rs -> rs)
        (\n t a rs ->
            Node a
                ("Addr "
                    ++ S.fromInt a
                    ++ ": "
                    ++ n
                    ++ "[ "
                    ++ C.Type.pretty t
                    ++ " ]"
                    ++ " = "
                    ++ R.withDefault "???" (lookupValue env n |> R.andThen (\v -> Ok <| C.Val.pretty v))
                )
                :: rs
        )
        (\_ _ rs -> rs)
        env.types
        env.addrs
        []


envEdges : Env -> List (Edge String)
envEdges env =
    D.merge
        (\_ _ rs -> rs)
        (\n t a rs ->
            case t of
                C.Type.IntT ->
                    rs

                C.Type.PointerT _ ->
                    case R.withDefault (C.Val.P Nothing) (lookupValue env n) of
                        C.Val.P Nothing ->
                            rs

                        C.Val.P (Just i) ->
                            Edge a i "" :: rs

                        C.Val.I _ ->
                            rs
        )
        (\_ _ rs -> rs)
        env.types
        env.addrs
        []


toGraph : Env -> Graph String String
toGraph env =
    G.fromNodesAndEdges (envNodes env) (envEdges env)


toGraphDot : Env -> String
toGraphDot env =
    GDOT.output Just Just (toGraph env)


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


updateAtAddr : Env -> Int -> Val -> Result String Env
updateAtAddr env a v =
    if D.member a env.store then
        Ok { env | store = D.update a (M.map (always v)) env.store }

    else
        Err <| "address " ++ S.fromInt a ++ " is not in use"


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


lookupValueAtAddr : Env -> Int -> Result String Val
lookupValueAtAddr env a =
    D.get a env.store |> Result.fromMaybe ""


view : Env -> Html msg
view env =
    let
        rows =
            L.reverse <|
                D.merge
                    (\_ _ rs -> rs)
                    (\n t a rs ->
                        tr []
                            [ td [] [ text n ]
                            , td [] [ text <| S.fromInt a ]
                            , td [] <| [ C.Type.view t ]
                            , td [] [ R.withDefault (text "???") (lookupValue env n |> R.andThen (\v -> Ok <| C.Val.view v)) ]
                            ]
                            :: rs
                    )
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
