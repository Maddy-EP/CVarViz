module C.Eval exposing (elabDefStr)

import C.Def exposing (Def)
import C.Env exposing (Env)
import C.Exp exposing (Exp(..))
import C.Parse as CP
import C.Type exposing (Type(..))
import C.Val exposing (Val(..))
import Dict as D
import Parser exposing (..)
import Parser.Extra as PE
import Result as R


eval : Env -> Exp -> Result String Val
eval env e =
    case e of
        Null ->
            Ok (P Nothing)

        Lit i ->
            Ok (I i)

        Var n ->
            C.Env.lookupValue env n

        Addr n ->
            C.Env.lookupAddr env n
                |> R.map (P << Just)

        Deref e_ ->
            eval env e_
                |> R.andThen
                    (\v ->
                        case v of
                            P Nothing ->
                                Err "tried to dereference null"

                            P (Just i) ->
                                D.get i env.store
                                    |> R.fromMaybe "got a dangling reference"

                            I _ ->
                                Err "tried to dereference a plain integer"
                    )


elabDefStr : Env -> String -> Result String Env
elabDefStr env s =
    run (CP.parseDef env) s
        |> R.mapError PE.deadEndsToString
        |> R.andThen (elabDef env)


elabDef : Env -> Def -> Result String Env
elabDef env d =
    typecheck env d.typ d.rhs
        |> R.andThen (\_ -> eval env d.rhs)
        |> R.andThen (\v -> C.Env.insert env d.typ d.name v)


typecheck : Env -> Type -> Exp -> Result String ()
typecheck env t e =
    case ( t, e ) of
        ( PointerT _, Null ) ->
            Ok ()

        ( IntT, Lit i ) ->
            Ok ()

        ( _, Var n ) ->
            C.Env.lookupType env n
                |> Result.andThen
                    (\t_ ->
                        if t_ /= t then
                            Err <| "expected '" ++ n ++ "' to have type " ++ C.Type.pretty t ++ ", but got " ++ C.Type.pretty t_

                        else
                            Ok ()
                    )

        ( PointerT t1, Addr n ) ->
            C.Env.lookupType env n
                |> Result.andThen
                    (\t_ ->
                        if t_ /= t1 then
                            Err <| "expected '" ++ n ++ "' to have type " ++ C.Type.pretty t1 ++ ", but got " ++ C.Type.pretty t_

                        else
                            Ok ()
                    )

        ( _, Deref e_ ) ->
            typecheck env (PointerT t) e_

        ( IntT, Null ) ->
            Err "type error: null is a value of pointer type"

        ( IntT, Addr _ ) ->
            Err "type error: taking the address of something results in a value of pointer type"

        ( PointerT _, Lit _ ) ->
            Err "type error: the only pointer literal is null"
