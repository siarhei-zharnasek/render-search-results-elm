import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (map2, map7)
import Url.Builder as Url


-- MAIN

main =
  Browser.element
    {
      init = init,
      update = update,
      subscriptions = subscriptions,
      view = view
    }


-- MODEL

type alias Model =
  {
    query : String,
    response : ResponseItem,
    error : String,
    searchEntity : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    Model "" ( ResponseItem 0 [] ) "" "Citation",
    Cmd.none
  )


-- UPDATE

type Msg
  = GetResponse (Result Http.Error ResponseItem)
  | InputHandler String
  | SubmitHandler
  | SearchEntityChange String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetResponse result ->
      case result of
        Ok response ->
          (
            { model | response = response, error = "" },
            Cmd.none
          )

        Err error ->
          (
            { model | error = "Error on request" },
            Cmd.none
          )

    InputHandler query ->
      (
        { model | query = query },
        Cmd.none
      )

    SubmitHandler ->
      (
        model,
        getResponse model.query
      )

    SearchEntityChange searchEntity ->
      (
        {model | searchEntity = searchEntity},
        Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [
      select [onInput SearchEntityChange] [
        option [value "Citation"] [text "Citation"],
        option [value "Substance"] [text "Substance"]
      ],
      input [ type_ "text", value model.query, onInput InputHandler, autofocus True ] [],
      viewValidation model,
      button [ onClick SubmitHandler ] [ text "Get results" ],
      br [] [],
      div [] [ text ("I found " ++ (String.fromInt model.response.totalHits) ++ " results") ],
      div [] (List.map renderEntities model.response.entities)
    ]

listStringRender : String -> Html msg
listStringRender word =
  div [] [
    text word
  ]

renderEntities : Item -> Html Msg
renderEntities item =
  div [] [
    h2 [] [text item.title],
    h3 [] [text "Abstract"],
    div [] [text item.abstract],
    h3 [] [text "Keywords"],
    div [] (List.map listStringRender (Maybe.withDefault [] item.keywords)),
    h3 [] [text "Journal Title"],
    div [] [text item.publicationDetail.publicationname],
    h3 [] [text "Year"],
    div [] [text (String.fromInt item.publicationDetail.hasPublicationYear)],
    h3 [] [text "Authors"],
    div [] (List.map listStringRender (Maybe.withDefault [] item.authors)),
    h3 [] [text "Source"],
    div [] [text item.source],
    h3 [] [text "PUI"],
    div [] [text item.pui],
    hr [] []
  ]

viewValidation : Model -> Html Msg
viewValidation model =
  if String.length model.error > 0
  then div [ style "color" "red" ] [ text model.error ]
  else div [] []


-- HTTP

getResponse : String -> Cmd Msg
getResponse query =
  Http.send GetResponse (Http.get (prepareQuery query) responseDecoder)

prepareQuery : String -> String
prepareQuery query =
  Url.crossOrigin "http://localhost:4200" ["search/", "citation", "search"]
    [
      Url.int "_from" 0,
      Url.int "_size" 20,
      Url.string "abstract" query,
      Url.string "keywords" query,
      Url.string "title" query
    ]

type alias Item =
  {
    title : String,
    abstract : String,
    keywords : Maybe (List String),
    publicationDetail : PublicationDetails,
    authors : Maybe (List String),
    source : String,
    pui : String
  }

keywordsDecode : Decode.Decoder (List String)
keywordsDecode =
  Decode.list Decode.string

type alias PublicationDetails = { publicationname : String, hasPublicationYear : Int }

publicationDetailDecode : Decode.Decoder PublicationDetails
publicationDetailDecode =
  map2 PublicationDetails
    (Decode.field "publicationname" Decode.string)
    (Decode.field "hasPublicationYear" Decode.int)

creatorDecode : Decode.Decoder (List String)
creatorDecode =
  Decode.list (Decode.field "name" Decode.string)

itemDecode : Decode.Decoder Item
itemDecode =
  map7 Item
    (Decode.field "title" Decode.string)
    (Decode.field "abstract" Decode.string)
    (Decode.maybe (Decode.field "keywords" keywordsDecode))
    (Decode.field "publicationDetail" publicationDetailDecode)
    (Decode.maybe (Decode.field "creator" creatorDecode))
    (Decode.field "provenance" (Decode.field "supplier" (Decode.field "name" Decode.string)))
    (Decode.field "pui" Decode.string)

entitiesDecoder : Decode.Decoder (List Item)
entitiesDecoder =
  Decode.list itemDecode

type alias ResponseItem = { totalHits : Int, entities : List Item }

responseDecoder : Decode.Decoder ResponseItem
responseDecoder =
  map2 ResponseItem
    (Decode.field "totalHits" Decode.int)
    (Decode.field "entities" entitiesDecoder)
    