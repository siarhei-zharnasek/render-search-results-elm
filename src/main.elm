import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (map2, map3)
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
    error : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    Model "" ( ResponseItem 0 [] ) "",
    Cmd.none
  )


-- UPDATE

type Msg
  = GetResponse (Result Http.Error ResponseItem)
  | InputHandler String
  | SubmitHandler

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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [
      input [ type_ "text", value model.query, onInput InputHandler, autofocus True ] [],
      viewValidation model,
      button [ onClick SubmitHandler ] [ text "Get results" ],
      br [] [],
      div [] [ text (String.fromInt model.response.totalHits) ],
      div [] (List.map renderEntities model.response.entities)
    ]

renderKeywords : String -> Html msg
renderKeywords word =
  div [] [
    text word
  ]

renderEntities : Item -> Html Msg
renderEntities item =
  div [] [
    div [] [text item.title],
    br [] [],
    div [] [text item.abstract],
    br [] [],
    div [] (List.map renderKeywords (Maybe.withDefault [] item.keywords)),
    hr [] [],
    br [] []
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

type alias Item = { title : String, abstract : String, keywords : Maybe (List String) }

keywordsDecode : Decode.Decoder (List String)
keywordsDecode =
  Decode.list Decode.string

itemDecode : Decode.Decoder Item
itemDecode =
  map3 Item
    (Decode.field "title" Decode.string)
    (Decode.field "abstract" Decode.string)
    (Decode.maybe (Decode.field "keywords" keywordsDecode))

entitiesDecoder : Decode.Decoder (List Item)
entitiesDecoder =
  Decode.list itemDecode

type alias ResponseItem = { totalHits : Int, entities : List Item }

responseDecoder : Decode.Decoder ResponseItem
responseDecoder =
  map2 ResponseItem
    (Decode.field "totalHits" Decode.int)
    (Decode.field "entities" entitiesDecoder)
    