module Main exposing (..)

import Dict
import Navigation exposing (Location)
import Html exposing (Html, text, div, img)
import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = TopRoute
    | LoginRoute (Maybe Route)
    | NotFoundRoute


type alias Model =
    { route : Route
    }

parseLocation : Location -> Route
parseLocation location =
    case (parse matchers location.pathname (parseParams location.search)) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map TopRoute top
        , map LoginRoute ((s "login") <?> (urlParam "redirect"))
        ]


urlParam : String -> QueryParser (Maybe Route -> a) a
urlParam name =
    customParam name parsePath


parsePath : Maybe String -> Maybe Route
parsePath pathname =
    case pathname of
        Just path ->
            parse matchers path Dict.empty

        Nothing ->
            Nothing


toPath : Route -> String
toPath route =
    case route of
        TopRoute ->
            "/"

        NotFoundRoute ->
            "/404"

        LoginRoute redirect ->
            case redirect of
                Just redirect ->
                    "/login?redirect=" ++ toPath redirect

                Nothing ->
                    "/login"


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute = parseLocation location
    in
        ( { route = currentRoute }, Cmd.none )


type Msg
    = OnLocationChange Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ div [] [ text (toPath model.route) ] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
