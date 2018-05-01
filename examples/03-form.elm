module Main exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Maybe


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : Result String Int
    , validity : Result String Bool
    }


model : Model
model =
    Model "" "" "" (Err "Empty") (Ok True)



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age (Result String Int)
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Age age ->
            { model | age = age }

        Submit ->
            { model | validity = updateValidity model }


updateValidity : Model -> Result String Bool
updateValidity model =
    let
        passwordTest test =
            .password >> String.any test >> not
    in
    if model.password /= model.passwordAgain then
        Err "Passwords do not match!"
    else if String.length model.password < 8 then
        Err "Password less than 8 characters long"
    else if passwordTest Char.isUpper model then
        Err "Password lacks upper case letter"
    else if passwordTest Char.isLower model then
        Err "Password lacks lower case letter"
    else if passwordTest Char.isDigit model then
        Err "Password lacks digit"
    else
        case model.age of
            Err _ ->
                Err "Age is not an integer"

            _ ->
                Ok True



-- VIEW


view : Model -> Html Msg
view model =
    Html.form [ onSubmit Submit ]
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "number", placeholder "Age", onInput (String.toInt >> Age) ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , button [] [ text "Submit" ]
        , viewValidation model
        ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message ) =
            case model.validity of
                Ok _ ->
                    ( "green", "OK" )

                Err message ->
                    ( "red", message )
    in
    div [ style [ ( "color", color ) ] ] [ text message ]
