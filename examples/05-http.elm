module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { topic : String
    , gif : Result Http.Error String
    }


init : String -> ( Model, Cmd Msg )
init topic =
    ( Model topic (Ok "waiting.gif")
    , getRandomGif topic
    )



-- UPDATE


type Msg
    = MorePlease
    | Topic String
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        Topic topic ->
            ( { model | topic = topic }, Cmd.none )

        NewGif gif ->
            ( { model | gif = gif }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.form [ onSubmit MorePlease ]
        [ input
            [ onInput Topic
            , value model.topic
            ]
            []
        , button [] [ text "More Please!" ]
        , br [] []
        , case model.gif of
            Ok url ->
                img [ src url ] []

            Err error ->
                div [] [ error |> errorMessage |> text ]
        ]


errorMessage error =
    case error of
        Http.BadUrl url ->
            "invalid URL" ++ url

        Http.Timeout ->
            "response timed out"

        Http.NetworkError ->
            "no network connectivity"

        Http.BadStatus { status } ->
            "bad status "
                ++ toString status.code
                ++ """

                 """
                ++ status.message

        Http.BadPayload code _ ->
            """
            bad payload
            """
                ++ "status "
                ++ code



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string
