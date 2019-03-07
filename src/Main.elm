module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, list, string)



-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Article =
    { title : String
    , url : String
    , id : String
    , image : String
    }


type alias Model =
    { articles : List Article
    , isError : Bool
    , page : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { articles = []
      , isError = False
      , page = 1
      }
    , getLatestArticles 1
    )



-- Update


type Msg
    = GotArticles (Result Http.Error (List Article))
    | GetNextPage
    | GetPrevPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotArticles (Ok hoge) ->
            ( { model | articles = hoge, isError = False }, Cmd.none )

        GotArticles (Err error) ->
            ( { model | isError = True }, Cmd.none )

        GetNextPage ->
            let
                next =
                    model.page + 1
            in
            ( { model | page = next }, getLatestArticles next )

        GetPrevPage ->
            if model.page > 1 then
                let
                    prev =
                        model.page - 1
                in
                ( { model | page = prev }, getLatestArticles prev )

            else
                ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "flex bg-orange fixed pin-t pin-x h-16" ]
            [ div [ class "text-white font-bold text-3xl flex items-center mx-8" ] [ text "Article list" ]
            ]
        , div [ class "container mx-auto pb-8" ]
            [ div [ class "flex flex-col items-center my-16" ] (viewArticles model)
            , viewButtons model
            ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    if model.page == 1 then
        div [ class "flex justify-around" ]
            [ button [ class "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded", onClick GetNextPage ] [ text "next" ]
            ]

    else
        div [ class "flex justify-around" ]
            [ button [ class "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded", onClick GetPrevPage ] [ text "prev" ]
            , div [] [ text ("page:" ++ String.fromInt model.page) ]
            , button [ class "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded", onClick GetNextPage ] [ text "next" ]
            ]


viewArticles : Model -> List (Html Msg)
viewArticles model =
    if model.isError == False then
        let
            el article =
                div [ class "border-b border-gray-dark py-2 flex w-3/5" ]
                    [ img [ class "w-12 h-12 rounded-lg my-2", src article.image ] []
                    , div [ class "flex flex-col ml-4" ]
                        [ a [ class "text-blue text-xl font-bold py-2", href article.url, target "_blank" ] [ text article.title ]
                        , span []
                            [ text "posted by: "
                            , a [ class "text-blue", href ("https://qiita.com/" ++ article.id), target "_blank" ] [ text article.id ]
                            ]
                        ]
                    ]
        in
        List.map el model.articles

    else
        [ div [ class "bg-red-lightest border border-red-light text-red-dark px-4 py-3 rounded rerative m-12" ]
            [ strong [ class "font-bold" ] [ text "Error: " ]
            , span [ class "block sm:inline" ] [ text "Something happened" ]
            ]
        ]



-- HTTP


baseUrl : String
baseUrl =
    "https://qiita.com/api/v2/items"


getLatestArticles : Int -> Cmd Msg
getLatestArticles page =
    if page == 1 then
        Http.get { url = baseUrl, expect = Http.expectJson GotArticles articlesDecoder }

    else
        Http.get { url = baseUrl ++ "?page=" ++ String.fromInt page, expect = Http.expectJson GotArticles articlesDecoder }


articleDecoder : Decoder Article
articleDecoder =
    Json.Decode.map4 Article
        (field "title" string)
        (field "url" string)
        (field "user" (field "id" string))
        (field "user" (field "profile_image_url" string))


articlesDecoder : Decoder (List Article)
articlesDecoder =
    list articleDecoder
