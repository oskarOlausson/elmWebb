module Main exposing (..)

import Html exposing (Html, aside, button, div, form, h1, h2, header, img, input, li, main_, nav, p, span, text, ul)
import Html.Attributes exposing (class, id, placeholder, src, title, type_)
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Lazy exposing (..)


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


outputString : Form -> Result Errors String
outputString form =
    case findErrors form of
        Nothing ->
            Ok <|
                "På "
                    ++ form.name
                    ++ " strävar vi efter perfektion. "
                    ++ "Med vår "
                    ++ form.adjective
                    ++ " design så tror vi att vi har skapat den "
                    ++ form.adjective2
                    ++ " tjänsten i världen."
                    ++ "Har du någonsin stått inför problemet med att behöva "
                    ++ form.verb
                    ++ " i timmar i sträck. Med "
                    ++ form.name
                    ++ " så behöver du inte "
                    ++ form.verb
                    ++ " längre. "
                    ++ form.name
                    ++ "'s team tror på perfektion för oss alla"
                    ++ " och vi vill dela den upplevelsen med dig."
                    ++ "Så hoppa ombord och upplev framtiden med "
                    ++ form.name
                    ++ ", "
                    ++ form.slogan
                    ++ "."

        Just errorMessage ->
            Err errorMessage


findErrors : Form -> Maybe Errors
findErrors form =
    let
        list =
            [ ( form.name, Name )
            , ( form.slogan, Slogan )
            , ( form.adjective, Adjective )
            , ( form.adjective2, Adjective2 )
            , ( form.verb, Verb )
            ]

        --/* gets all the errors in the fields */--
        errorList =
            List.map (\( a, b ) -> b) <| List.filter (\( a, b ) -> String.isEmpty a) list
    in
    if List.isEmpty errorList then
        if String.right 1 form.adjective /= "a" then
            Just <| Errors "adjektivet måste sluta på bokstaven a" errorList
        else
            Nothing
    else
        Just <| Errors "All fields are not filled out" errorList



---- MODEL ----


type alias Model =
    { site : Site
    , formContent : Form
    , outputString : String
    , show : Bool
    , errors : Maybe Errors
    }


type alias Errors =
    { feedback : String
    , errors : List ChangeType
    }


type alias Form =
    { name : String
    , slogan : String
    , adjective : String
    , adjective2 : String
    , verb : String
    }


emptyForm : Form
emptyForm =
    Form "" "" "" "" ""


init : ( Model, Cmd Msg )
init =
    ( Model Main emptyForm "" False Nothing, Cmd.none )


type Site
    = Main
    | Help


type ChangeType
    = Name
    | Slogan
    | Adjective
    | Adjective2
    | Verb



---- UPDATE ----


type Msg
    = ChangeSite Site
    | FormChange ChangeType String
    | UpdateOutput
    | RemoveError ChangeType
    | RemoveAllErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSite s ->
            ( swapSite model s, Cmd.none )

        FormChange t s ->
            let
                newForm =
                    handleFormChange model.formContent t s
            in
            ( { model | formContent = newForm }, Cmd.none )

        UpdateOutput ->
            case outputString model.formContent of
                Ok newOutput ->
                    ( { model | outputString = newOutput, errors = Nothing, show = True }, Cmd.none )

                Err errors ->
                    ( { model | errors = Just errors }, Cmd.none )

        RemoveError changeType ->
            case model.errors of
                Just e ->
                    let
                        list =
                            e.errors

                        errors =
                            { e
                                | errors =
                                    List.filter
                                        (\x -> not <| x == changeType)
                                        list
                            }
                    in
                    ( { model | errors = Just errors }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RemoveAllErrors ->
            ( { model | errors = Nothing }, Cmd.none )


handleFormChange : Form -> ChangeType -> String -> Form
handleFormChange form changeType string =
    case changeType of
        Name ->
            { form | name = string }

        Slogan ->
            { form | slogan = string }

        Adjective ->
            { form | adjective = string }

        Adjective2 ->
            { form | adjective2 = string }

        Verb ->
            { form | verb = string }


swapSite : Model -> Site -> Model
swapSite currentModel newSite =
    { currentModel | site = newSite }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ header []
                [ h1 [] [ span [ id "panic" ] [ text "Panik " ], text "Pitch" ]
                , h2 [] [ text "Skapa ett elevator-pitch på nolltid" ]
                , nav []
                    [ div [ selected model.site Main, onClick <| ChangeSite Main ] [ text "Pitch" ]
                    , div [ selected model.site Help, onClick <| ChangeSite Help ] [ text "Hjälp" ]
                    ]
                ]
            , pageContent model
            ]
        ]


selected : a -> a -> Html.Attribute msg
selected current this =
    if current == this then
        class "selected"
    else
        class "notSelected"


pageContent : Model -> Html Msg
pageContent model =
    case model.site of
        Main ->
            mainPage model

        Help ->
            helpPage


mainPage : Model -> Html Msg
mainPage model =
    let
        errorClass =
            case model.errors of
                Nothing ->
                    "valid"

                Just _ ->
                    "error"

        errorDiv =
            case model.errors of
                Nothing ->
                    []

                Just e ->
                    [ div [ id "errorDiv", onClick RemoveAllErrors ]
                        [ span [] [ text e.feedback ]
                        , div [ id "errorButton" ] [ text "🞮" ]
                        ]
                    ]
    in
    main_ []
        [ div [ class "gridPart" ] <|
            [ h2 [ class "description" ]
                [ text "Fyll i fälten nedanför och klicka på skapa pitch så genererar vi ett pitch åt dig" ]
            , form [ id "allTheInputs" ]
                [ myInput model.errors "Namn på din tjänst" "Panik Pitch" Name
                , myInput model.errors "Slogan för din tjänst" "För en bättre värld" Slogan
                , myInput model.errors "Adjektiv som slutar på a" "coola, bästa" Adjective
                , myInput model.errors "Adjektiv i superlativ" "roligaste" Adjective2
                , myInput model.errors "Verb folk ogillar att utföra" "städa" Verb
                ]
            , if model.show then
                aside [ class errorClass ] [ lazy text model.outputString ]
              else
                aside [] []
            , button [ id "generateButton", onClick UpdateOutput ]
                [ text "Generera pitch" ]
            ]
                ++ errorDiv
        ]


myInput : Maybe Errors -> String -> String -> ChangeType -> Html Msg
myInput errors labelText hint formType =
    let
        attr =
            case errors of
                Nothing ->
                    []

                Just e ->
                    if List.member formType e.errors then
                        [ class "errorField", title e.feedback ]
                    else
                        [ title "" ]
    in
    div [ class "input-field" ]
        [ text labelText
        , input
            (attr
                ++ [ type_ "text"
                   , placeholder <| "ex) " ++ hint
                   , onInput <| FormChange formType
                   , onFocus <| RemoveError formType
                   ]
            )
            []
        ]


helpPage : Html Msg
helpPage =
    let
        list =
            [ ( "Vad är en elevator-pitch?"
              , "Det är ett kort sammanfattning att din tjänst som hjälper till att sälja"
                    ++ "produkten. Namnet elevator-pitch kommer från att det ska vara så kort så man hinner säga"
                    ++ "det till någon medans man åker hiss. Cirka 30 sek till en minut"
              )
            , ( "Kan man ändra längd på den genererade pitchen?", "Nej" )
            , ( "Min pitch blev jättedålig?!"
              , "Pitchen genereras med en mad-lib model, "
                    ++ "det finns alltså ingen AI "
                    ++ "bakom som förstår meningen med pitchen"
              )
            ]
    in
    ul [ id "faq" ] <|
        List.map
            (\( hej, san ) ->
                li []
                    [ div [ class "question" ]
                        [ text hej ]
                    , div
                        [ class "answer" ]
                        [ text san ]
                    ]
            )
            list
