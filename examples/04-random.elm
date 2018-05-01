module Main exposing (..)

import Array
import Char
import Html exposing (..)
import Html.Events exposing (..)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace0 : Int
    , dieFace1 : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1 1, Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)) )

        NewFace ( newFace0, newFace1 ) ->
            ( Model newFace0 newFace1, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


die =
    Array.fromList
        [ '⚀'
        , '⚁'
        , '⚂'
        , '⚃'
        , '⚄'
        , '⚅'
        ]


toDieFace dieFace =
    die |> Array.get (dieFace - 1) |> Maybe.withDefault '�' |> String.fromChar


view : Model -> Html Msg
view model =
    Html.form [ onSubmit Roll ]
        [ h1 []
            [ (model.dieFace0 |> toDieFace) ++ (model.dieFace1 |> toDieFace) |> text
            ]
        , button [] [ text "Roll" ]
        ]
