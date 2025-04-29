module C.Eval exposing (elabDefStr)

import C.Def exposing (Def)
import C.Env exposing (Env)
import C.Exp exposing (Exp(..))
import C.Parse as CP
import C.Stmt exposing (LValue(..), Stmt(..))
import C.Type exposing (Type(..))
import C.Val exposing (Val(..))
import Dict as D
import Parser exposing (..)
import Parser.Extra as PE
import Result as R


evalExp : Env -> Exp -> Result String Val
evalExp env e =
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
            evalExp env e_
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
    run CP.parseDef s
        |> R.mapError PE.deadEndsToString
        |> R.andThen (elabDef env)
addrOfLValue : Env -> LValue -> Result String Int
addrOfLValue env lv =
    case lv of
        LVar n ->
            C.Env.lookupAddr env n

        LDeref lv_ ->
            case addrOfLValue env lv_ |> R.andThen (\a -> C.Env.lookupValueAtAddr env a) |> R.toMaybe of
                Nothing ->
                    Err "could not resolve lvalue to a value"

                Just (P Nothing) ->
                    Err "lvalue cannot be null"

                Just (P (Just i)) ->
                    Ok i

                Just (I _) ->
                    Err "lvalue cannot be a plain integer"


runStmt : Env -> Stmt -> Result String Env
runStmt env s =
    case s of
        Assign lv e ->
            lvalueType env lv
                |> R.andThen
                    (\t ->
                        typecheck env t e
                            |> R.andThen (\_ -> evalExp env e)
                            |> R.andThen
                                (\v ->
                                    addrOfLValue env lv
                                        |> R.andThen (\a -> C.Env.updateAtAddr env a v)
                                )
                    )


elabDef : Env -> Def -> Result String Env
elabDef env d =
    typecheck env d.typ d.rhs
        |> R.andThen (\_ -> evalExp env d.rhs)
        |> R.andThen (\v -> C.Env.insert env d.typ d.name v)


lvalueName : LValue -> String
lvalueName lv =
    case lv of
        LVar s ->
            s

        LDeref lv_ ->
            lvalueName lv_


lvalueType : Env -> LValue -> Result String Type
lvalueType env lv =
    case lv of
        LVar n ->
            C.Env.lookupType env n

        LDeref lv_ ->
            lvalueType env lv_
                |> R.andThen
                    (\t ->
                        case t of
                            PointerT t_ ->
                                Ok t_

                            IntT ->
                                Err "type error: cannot dereference an lvalue of integer type"
                    )


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
