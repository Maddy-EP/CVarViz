module C.Stmt exposing (LValue(..), Stmt(..))

import C.Exp exposing (Exp)


type LValue
    = LVar String
    | LDeref LValue


type Stmt
    = Assign LValue Exp
