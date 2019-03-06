module SimpleWebFront exposing (Msg(..), main, update, view)

import Base64
import Basics
import BinaryFileReader exposing (Error(..), FileRef, NativeFile, parseSelectedFiles, postImageFileTask, readAsArrayBuffer, readAsDataUrl)
import Date exposing (Date)
import Debug exposing (log)
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Http exposing (Body, emptyBody, expectJson, multipartBody, stringBody, stringPart)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe
import MimeType
import String
import Task
import Tuple
import Update.Extra exposing (addCmd, sequence)
import Update.Extra.Infix exposing ((:>))


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { file : Maybe NativeFile
    , dataUrl : Maybe String
    , result_data : Maybe ResultData
    , error : Maybe String
    , isUploading : Bool
    }


type alias ResultData =
    { sides : List BoxSide
    }


resultDataToValue =
    Decode.map
        ResultData
        (Decode.field "box_side_length" <| Decode.list boxSideToValue)


type alias BoxSide =
    { len : Float
    }


boxSideToValue =
    Decode.map
        BoxSide
        Decode.float


type Msg
    = NoOp
    | SetError String
    | Upload
    | SetResult ResultData
    | SelectFile (List NativeFile)
    | ReadFile NativeFile ( Date, String )


init : ( Model, Cmd Msg )
init =
    { file = Nothing
    , dataUrl = Nothing
    , result_data = Nothing
    , error = Nothing
    , isUploading = False
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetError err ->
            { model
                | error = Just err
                , isUploading = False
            }
                ! []

        Upload ->
            { model | isUploading = True }
                ! [ Task.attempt
                        (\res ->
                            case log "res" res of
                                Ok r ->
                                    SetResult r

                                _ ->
                                    NoOp
                        )
                    <|
                        postImageFileTask regularCargoSpaceSizeingRequests
                  ]

        SetResult res ->
            { model
                | result_data = Just res
                , isUploading = False
            }
                ! []

        SelectFile files ->
            { model | isUploading = True }
                ! (case List.head files of
                    Just file ->
                        [ readArrayBuffer file ]

                    Nothing ->
                        []
                  )

        ReadFile fileRef ( date, dataUrl ) ->
            { model
                | file = Just fileRef
                , dataUrl = Just dataUrl
            }
                ! []


view : Model -> Html Msg
view model =
    div []
        [ select_view model
        , case ( model.file, model.dataUrl ) of
            ( Just f, Just durl ) ->
                file_info_view model f durl

            _ ->
                div [] []
        , case model.result_data of
            Just res ->
                result_view model res

            Nothing ->
                div [] []
        , case model.error of
            Just err ->
                div [] [ text <| "Error : " ++ err ]

            Nothing ->
                div [] []
        ]


select_view : Model -> Html Msg
select_view model =
    div []
        [ text "" -- "Select File"
        , input
            [ type_ "file"
            , multiple False
            , on "change"
                (parseSelectedFiles
                    |> Decode.andThen (Decode.succeed << SelectFile)
                )
            ]
            []
        ]


file_info_view : Model -> NativeFile -> String -> Html Msg
file_info_view model file durl =
    div []
        [ div [] [ text file.name ]
        , div [] []
        , image_view model file durl
        ]


image_view model file durl =
    div [] <|
        case file.mimeType of
            Just (MimeType.Image _) ->
                [ img [ Html.Attributes.src durl ] []
                , upload_view model file
                ]

            _ ->
                [ text file.name ]


upload_view model file =
    div []
        [ button
            [ onClick <| Upload
            , disabled <| not model.isUploading
            ]
            [ text "Upload" ]
        ]


result_view : Model -> ResultData -> Html Msg
result_view model res =
    div [] <|
        List.map
            (\side ->
                div []
                    [ text <|
                        toString side.len
                    ]
            )
            res.sides


subscriptions model =
    Sub.none



--{-


regularCargoSpaceSizeingRequests =
    { method = "POST"
    , headers = []
    , url = "https://xxxxxxxxxx.execute-api.ap-northeast-1.amazonaws.com/demo/"
    , body = emptyBody
    , expect = expectJson resultDataToValue
    , timeout = Nothing
    , withCredentials = False
    }
--}



{-


   regularCargoSpaceSizeingRequests =
       Http.post
           "https://xxxxxxxxxx.execute-api.ap-northeast-1.amazonaws.com/demo/"
           emptyBody
           resultDataToValue



   --
   -
-}


readArrayBuffer : NativeFile -> Cmd Msg
readArrayBuffer fileRef =
    Task.attempt
        (\x ->
            case x of
                Ok ( d, dataUrl ) ->
                    ReadFile fileRef ( d, dataUrl )

                Err e ->
                    SetError <| toString e
        )
        (readAsDataUrl fileRef.blob
            |> Task.andThen
                (\mDataUrl ->
                    readAsArrayBuffer fileRef.blob
                        |> Task.andThen
                            (\_ ->
                                Task.succeed mDataUrl
                            )
                )
        )
