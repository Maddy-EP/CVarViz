module C.Parse exposing (parseCommand)

import C.Command exposing (Command(..))
import C.Def exposing (Def)
import C.Env exposing (Env)
import C.Exp exposing (Exp(..))
import C.Stmt exposing (LValue(..), Stmt(..))
import C.Type exposing (Type(..))
import Char as C
import Parser exposing (..)
import Set as S


parseType : Parser Type
parseType =
    succeed identity
        |. keyword "int"
        |. spaces
        |= loop identity parseStars
        |= succeed IntT


parseStars : (Type -> Type) -> Parser (Step (Type -> Type) (Type -> Type))
parseStars appPointerT =
    oneOf
        [ succeed (Loop (PointerT << appPointerT)) |. symbol "*"
        , succeed (Done appPointerT)
        ]


parseVarName : Parser String
parseVarName =
    variable
        { start = C.isAlpha
        , inner = \c -> C.isAlphaNum c || c == '_'
        , reserved = S.fromList [ "int", "null" ]
        }


parseDef : Parser Def
parseDef =
    succeed Def
        |= parseType
        |. spaces
        |= parseVarName
        |. spaces
        |. symbol "="
        |. spaces
        |= parseExp
        |. spaces
        |. symbol ";"


parseExp : Parser Exp
parseExp =
    oneOf
        [ map Lit int
        , map (always Null) (keyword "null")
        , succeed Addr
            |. symbol "&"
            |= parseVarName
        , succeed Deref
            |. symbol "*"
            |= lazy (\_ -> parseExp)
        , succeed Var
            |= parseVarName
        ]


parseLValue : Parser LValue
parseLValue =
    oneOf
        [ succeed LDeref
            |. symbol "*"
            |= lazy (\_ -> parseLValue)
        , succeed LVar
            |= parseVarName
        ]


parseStmt : Parser Stmt
parseStmt =
    succeed Assign
        |= parseLValue
        |. spaces
        |. symbol "="
        |. spaces
        |= parseExp
        |. spaces
        |. symbol ";"


parseCommand : Parser Command
parseCommand =
    succeed identity
        |. spaces
        |= oneOf
            [ map CDef parseDef
            , map CStmt parseStmt
            ]
        |. end
