module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (b, button, div, h1, img, li, table, td, text, th, tr, ul)
import Html.Attributes exposing (height, src, style, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type Model
    = Ready
    | Loading
    | BrewerDetails (List Brewer)
    | BrewerDetailsLoadFailed


init : () -> ( Model, Cmd Msg )
init _ =
    ( Ready, Cmd.none )


type Msg
    = Noop
    | LoadBrewerData
    | BrewersDataGetOrNot (Result Http.Error (List Brewer))


type alias Brewer =
    { id : Int
    , name : String
    , tagline : String
    , firstBrewed : String
    , description : String
    , imageUrl : String
    , volumeValue : Int
    , volumeUnit : String
    , contributedBy : String
    , ingredients : Ingredients
    }


type alias Amount =
    { value : Float
    , unit : String
    }


type alias Malt =
    { name : String
    , amount : Amount
    }


type alias Hops =
    { name : String
    , amount : Amount
    , add : String
    , attribute : String
    }


type alias Ingredients =
    { malts : List Malt
    , hops : List Hops
    , yeast : String
    }


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


ingredientsHopsDecoder : JD.Decoder Hops
ingredientsHopsDecoder =
    JD.succeed Hops
        |> JDP.required "name" JD.string
        |> JDP.required "amount" ingredientsMaltAmountDecoder
        |> JDP.required "add" JD.string
        |> JDP.required "attribute" JD.string


ingredientsDecoder : JD.Decoder Ingredients
ingredientsDecoder =
    JD.succeed Ingredients
        |> JDP.required "malt" (JD.list ingredientsMaltDecoder)
        |> JDP.required "hops" (JD.list ingredientsHopsDecoder)
        |> JDP.required "yeast" JD.string


brewerParser : JD.Decoder Brewer
brewerParser =
    JD.succeed Brewer
        |> JDP.required "id" JD.int
        |> JDP.required "name" JD.string
        |> JDP.required "tagline" JD.string
        |> JDP.required "first_brewed" JD.string
        |> JDP.required "description" JD.string
        |> JDP.required "image_url" JD.string
        |> JDP.requiredAt [ "volume", "value" ] JD.int
        |> JDP.requiredAt [ "volume", "unit" ] JD.string
        |> JDP.required "contributed_by" JD.string
        |> JDP.required "ingredients" ingredientsDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        LoadBrewerData ->
            ( Loading, loadBrewer )

        BrewersDataGetOrNot result ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "check" err
                    in
                    ( BrewerDetailsLoadFailed, Cmd.none )

                Ok brewer ->
                    ( BrewerDetails brewer, Cmd.none )


loadBrewer =
    Http.get
        { url = "https://api.punkapi.com/v2/beers"
        , expect = Http.expectJson BrewersDataGetOrNot (JD.list brewerParser)
        }


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Use the api and display the information" ]
        , button [ onClick LoadBrewerData ] [ text "Reload" ]
        , brewerTable model
        ]


brewerTable model =
    case model of
        Ready ->
            div [] [ text "Click Here to fetch the Brewer Data" ]

        Loading ->
            div [] [ text "Loading.." ]

        BrewerDetails brewer ->
            table []
                (newFunction brewer)

        BrewerDetailsLoadFailed ->
            div [] [ text "Failed" ]


newFunction : List Brewer -> List (Html.Html msg)
newFunction brewers =
    [ tr []
        [ th [] [ text "Brewer-Id" ]
        , th [] [ text "Name" ]
        , th [] [ text "Tagline" ]
        , th [] [ text "First Brewed" ]
        , th [] [ text "Description" ]
        , th [] [ text "Image" ]
        , th [] [ text "Volume" ]
        , th [] [ text "Contributed by" ]
        , th [] [ text "Ingredients: Malts" ]
        , th [] [ text "Ingredients: Hops" ]
        , th [] [ text "Ingredients: Yeast" ]
        ]
    ]
        ++ List.map
            (\brewer ->
                tr []
                    [ td [] [ text (String.fromInt brewer.id) ]
                    , td [] [ text brewer.name ]
                    , td [] [ text brewer.tagline ]
                    , td [] [ text brewer.firstBrewed ]
                    , td [] [ text brewer.description ]
                    , td [ style "height" "50px" ] [ img [ src brewer.imageUrl, height 100, width 50 ] [] ]
                    , td []
                        [ td [] [ text ("Unit:" ++ brewer.volumeUnit) ]
                        , td [] [ text ("Value:" ++ String.fromInt brewer.volumeValue) ]
                        ]
                    , td [] [ text brewer.contributedBy ]
                    , td [] [ maltrender brewer.ingredients.malts ]
                    , td [] [ hopsrendser brewer.ingredients.hops ]
                    , td [] [ text brewer.ingredients.yeast ]
                    ]
            )
            brewers


hopsrendser : List Hops -> Html.Html msg
hopsrendser lst =
    ul []
        (List.map
            (\element ->
                li []
                    [ li [] [ text ("name : " ++ element.name) ]
                    , li [] [ text ("add :  " ++ element.add) ]
                    , li [] [ text ("attribute : " ++ element.attribute) ]
                    , li [] [ text ("amount Unit : " ++ element.amount.unit) ]
                    , li [] [ text ("amount value : " ++ toString element.amount.value) ]
                    , li [ style "listStyle" "none" ] [ b [] [ text "New Ingredients: Hops" ] ]
                    ]
            )
            lst
        )


maltrender : List Malt -> Html.Html msg
maltrender lst =
    ul []
        (List.map
            (\element ->
                li []
                    [ li [] [ text ("name : " ++ element.name) ]
                    , li [] [ text ("amount Unit : " ++ element.amount.unit) ]
                    , li [] [ text ("amount value : " ++ toString element.amount.value) ]
                    , li [ style "listStyle" "none" ] [ b [] [ text "New Ingredients: Malts---" ] ]
                    ]
            )
            lst
        )


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
