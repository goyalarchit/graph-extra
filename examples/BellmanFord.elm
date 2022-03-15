module BellmanFord exposing (..)
import Browser 
import Graph.Extra as GE
import Graph as G
import Dict as D
import Html exposing (text)

main =
    text bellmanExample

sampleGraph1 : G.Graph () Float
sampleGraph1 =
    G.fromNodesAndEdges
        [ G.Node 0 ()
        , G.Node 1 ()
        , G.Node 2 ()
        , G.Node 3 ()
        , G.Node 4 ()
        ]
        [ G.Edge 0 1 1
        , G.Edge 0 2 4
        , G.Edge 1 2 -2
        , G.Edge 1 3 2
        , G.Edge 2 3 3
        , G.Edge 3 4 4
        , G.Edge 4 1 -3
        ]


sampleGraph2 : G.Graph () Float
sampleGraph2 =
    G.fromNodesAndEdges
        [ G.Node 0 ()
        , G.Node 1 ()
        , G.Node 2 ()
        ]
        [ G.Edge 0 1 2
        , G.Edge 1 2 3
        , G.Edge 2 0 -3
        ]

bellmanExample : String
bellmanExample =
    case GE.bellmanFord 0 (.label) sampleGraph2 of
        Nothing ->
            "Negative Weight Cycle Detected"
        Just dpd ->
            D.toList dpd 
            |> List.map (\(n,(d,p)) -> String.fromInt n ++ " : " ++ String.fromFloat d )
            |> String.join " |#| "