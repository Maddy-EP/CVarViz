module C.Exp exposing (Exp(..))


type Exp
    = Null
    | Lit Int
    | Var String
    | Addr String
    | Deref Exp
