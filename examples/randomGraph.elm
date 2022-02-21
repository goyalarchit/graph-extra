module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text,input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)
import Random as R
import Random.List as RL
import Graph as G
import Graph.DOT as GD
import Random.Graph as RG 


type alias Model =
    { nodes : List Int 
    , edges : List (Int,Int)
    , nodeCnt : Int
    , edgeCnt : Int
    }


initialModel : () -> (Model,Cmd Msg)
initialModel _ =
    (
    { nodes = [1,2,3,4,5]
    , edges = []
    , nodeCnt = 2
    , edgeCnt = 3
    }
    , R.generate Got (RG.simpleConnectedGraph 2 3)
    )


type Msg
    = Generate
    | Got (List Int, List (Int,Int))
    | ChangeNodeCnt String
    | ChangeEdgeCnt String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, R.generate Got (RG.simpleConnectedGraph model.nodeCnt model.edgeCnt))
        Got (m,n) ->
            ({model | nodes = m, edges = n},Cmd.none)
        ChangeNodeCnt n ->
            ({model | nodeCnt = Maybe.withDefault 1 (String.toInt n)},Cmd.none)
        ChangeEdgeCnt e ->
            ({model | edgeCnt = Maybe.withDefault 1 (String.toInt e)},Cmd.none)
             


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| String.join ", " (List.map (String.fromInt) model.nodes ) ]
        , div [] [ text <| String.join ", " (List.map (\(n1,n2) -> "(" ++ String.fromInt n1 ++", " ++ String.fromInt n2 ++ ")") model.edges ) ]
        , div [] [ text <| viewGraph model]
        , input [onInput ChangeNodeCnt, placeholder "Enter Number of Nodes" ] []
        , input [onInput ChangeEdgeCnt, placeholder "Enter Number of Edges" ] []
        , button [ onClick Generate ] [ text "Generate" ]
        ]

viewGraph : Model -> String
viewGraph model =
    let
        nodeList = List.map (\n -> G.Node n (String.fromInt n)) model.nodes
        edgeList = List.map (\(n1,n2) -> G.Edge n1 n2 ()) model.edges
       
    
    in
    
    G.fromNodesAndEdges nodeList edgeList
    |> GD.output (\_ -> Nothing) (\_ -> Nothing) 



main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
