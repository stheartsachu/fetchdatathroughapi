module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (button, div, h1, li, table, td, text, th, tr, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP exposing (required)


type Model
    = Ready
    | Loading
    | PeopleLoaded People
    | PeopleLoadFailed


init : () -> ( Model, Cmd Msg )
init _ =
    ( Ready, Cmd.none )


type Msg
    = Noop
    | GetPeople
    | GotPeople (Result Http.Error People)


type alias People =
    List Person


type alias Person =
    { id : Int
    , name : String
    , tagline : String
    , first_brewed : String
    , description : String
    , image_url : String
    , volumeValue : Int
    , volumeUnit : String
    , contributed_by : String
    , ingredients_yeast : String
    }


type alias Amount =
    { value : Float
    , unit : String
    }


type alias Malt =
    { name : String
    , amount : Amount
    }



-- type alias IngredientsMalt =
--     List Malt


ingredientsMaltAmountDecoder : JD.Decoder Amount
ingredientsMaltAmountDecoder =
    JD.succeed Amount
        |> JDP.required "value" JD.float
        |> JDP.required "unit" JD.string


ingredientsMaltDecoder : JD.Decoder Malt
ingredientsMaltDecoder =
    JD.succeed Malt
        |> JDP.required "name" JD.string
        |> JDP.required "amount" ingredientsMaltAmountDecoder



-- ingredientsMaltDecoderParser : JD.Decoder IngredientsMalt
-- ingredientsMaltDecoderParser =
--     JD.list ingredientsMaltDecoder
-----------------------------------------


personParser : JD.Decoder Person
personParser =
    JD.succeed Person
        |> JDP.required "id" JD.int
        |> JDP.required "name" JD.string
        |> JDP.required "tagline" JD.string
        |> JDP.required "first_brewed" JD.string
        |> JDP.required "description" JD.string
        |> JDP.required "image_url" JD.string
        |> JDP.requiredAt [ "volume", "value" ] JD.int
        |> JDP.requiredAt [ "volume", "unit" ] JD.string
        |> JDP.required "contributed_by" JD.string
        |> JDP.requiredAt [ "ingredients", "yeast" ] JD.string



--


peopleParser : JD.Decoder People
peopleParser =
    JD.list personParser


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GetPeople ->
            ( Loading, loadPeople )

        GotPeople result ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "check" err
                    in
                    ( PeopleLoadFailed, Cmd.none )

                Ok people ->
                    ( PeopleLoaded people, Cmd.none )


loadPeople =
    Http.get
        { url = "https://api.punkapi.com/v2/beers"
        , expect = Http.expectJson GotPeople peopleParser
        }


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Use the api and display the information" ]
        , button [ onClick GetPeople ] [ text "Reload" ]
        , peopleTable model
        ]


peopleTable model =
    case model of
        Ready ->
            div [] [ text "Click Here to fetch the Brewer Data" ]

        Loading ->
            div [] [ text "Loading.." ]

        PeopleLoaded people ->
            table []
                (newFunction people)

        PeopleLoadFailed ->
            div [] [ text "Failed" ]


newFunction2 malt =
    List.map (\m -> li [] [ text m.name ]) malt


newFunction : People -> List (Html.Html msg)
newFunction people =
    [ tr []
        [ th [] [ text "Brewer-Id" ]
        , th [] [ text "Name" ]
        , th [] [ text "Tagline" ]
        , th [] [ text "First Brewed" ]
        , th [] [ text "Description" ]
        , th [] [ text "Image url" ]
        , th [] [ text "Volume" ]
        , th [] [ text "Contributed by" ]
        , th [] [ text "Ingredients" ]
        ]
    ]
        ++ List.map
            (\person ->
                tr []
                    [ td [] [ text (String.fromInt person.id) ]
                    , td [] [ text person.name ]
                    , td [] [ text person.tagline ]
                    , td [] [ text person.first_brewed ]
                    , td [] [ text person.description ]
                    , td [] [ text person.image_url ]
                    , tr []
                        [ td [] [ text ("Unit:" ++ person.volumeUnit) ]
                        , td [] [ text ("Value:" ++ String.fromInt person.volumeValue) ]
                        ]
                    , td [] [ text person.contributed_by ]
                    , td []
                        [ ul []
                            [ li [] [ text person.ingredients_yeast ]
                            ]
                        ]
                    ]
            )
            people


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
