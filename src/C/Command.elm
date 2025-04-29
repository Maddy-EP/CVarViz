module C.Command exposing (Command(..))

import C.Def exposing (Def)
import C.Stmt exposing (Stmt)


type Command
    = CDef Def
    | CStmt Stmt
