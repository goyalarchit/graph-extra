module Kruskal exposing (..)
import Browser 
import Graph.Extra as GE
import Graph as G
import Dict as D
import Html exposing (text)
import Graph.DOT as GD

main =
    text kruskalExample

sampleGraph1 : G.Graph String Float
sampleGraph1 =
    G.fromNodesAndEdges
        [ G.Node 0 "a"
        , G.Node 1 "b"
        , G.Node 2 "c"
        , G.Node 3 "d"
        , G.Node 4 "e"
        , G.Node 5 "f"
        , G.Node 6 "g"
        , G.Node 7 "h"
        , G.Node 8 "i"
        ]
        [ G.Edge 0 1 4      -- (a, b, 4)    
        , G.Edge 0 7 8      -- (a, h, 8)    
        , G.Edge 1 2 8      -- (b, c, 8)    
        , G.Edge 1 7 11     -- (b, h, 11)   
        , G.Edge 2 3 7      -- (c, d, 7)    
        , G.Edge 2 5 4      -- (c, f, 4)    
        , G.Edge 2 8 2      -- (c, i, 2)    
        , G.Edge 3 4 9      -- (d, e, 9)    
        , G.Edge 3 5 14     -- (d, f, 14)   
        , G.Edge 4 5 10     -- (e, f, 10)   
        , G.Edge 5 6 2      -- (f, g, 2)   
        , G.Edge 6 7 1      -- (g, h, 1)    
        , G.Edge 6 8 6      -- (g, i, 6)    
        , G.Edge 7 8 7      -- (h, i, 7)    
        ]


kruskalExample : String
kruskalExample =
    GE.kruskal (.label) sampleGraph1
    |> GD.output (identity >> Just) (\e -> Just (String.fromFloat e)) 