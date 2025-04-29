port module Main exposing (init, main)

import Browser
import C.Env as CE exposing (Env)
import C.Eval as CEv
import Html exposing (..)
import Html.Attributes exposing (attribute, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- PORTS


port sendDot : String -> Cmd msg



-- MODEL


type alias Model =
    { env : Env
    , varString : String
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model CE.emptyEnv "" "", Cmd.none )



-- UPDATE


type Msg
    = AddVar
    | ChangeCVarInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddVar ->
            let
                res =
                    CEv.runCommandStr model.env model.varString
            in
            case res of
                Ok env_ ->
                    ( { model | env = env_, varString = "", error = "" }, sendDot <| CE.toGraphDot env_ )

                Err e ->
                    ( { model | error = e }, Cmd.none )

        ChangeCVarInput newContent ->
            ( { model | varString = newContent }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    article [ id "app" ]
        [ h2 [] [ text "Visualizer" ]
        , aside []
            [ label [] [ text "C definition:" ]
            , input [ placeholder "int x = 0;", value model.varString, onInput ChangeCVarInput, type_ "input" ] []
            , button [ onClick AddVar, type_ "button" ] [ text "Add variable" ]
            ]
        , aside []
            [ text <|
                if model.error /= "" then
                    "Error: " ++ model.error

                else
                    ""
            ]
        , details []
            [ summary [] [ text "Current variable environment" ]
            , p [] [ CE.view model.env ]
            ]
        ]
