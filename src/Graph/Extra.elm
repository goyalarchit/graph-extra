module Graph.Extra exposing (..)

import Dict
import Graph
import List.Extra as LE


{-| Utility

1.  adjacentNodes : both incoming and outgoing edges

Algorithms

1.  Dijkstra
2.  Kruskals

-}
adjacentNodes : Graph.NeighborSelector n e
adjacentNodes nodeCtx =
    List.append (Graph.alongIncomingEdges nodeCtx) (Graph.alongOutgoingEdges nodeCtx)
        |> LE.unique


dijkstra : Graph.NodeContext n e -> (e -> number) -> Bool -> Graph.Graph n e -> Dict.Dict Graph.NodeId ( number, List Graph.NodeId )
dijkstra source weightFn directed g =
    let
        weightedGraph =
            Graph.mapEdges (\e -> weightFn e) g

        distancePathDict =
            Dict.singleton source.node.id ( 0, [] )
    in
    Debug.todo "implement dijkstras"


kruskal : (Graph.Edge e -> Float) -> Graph.Graph n e -> Graph.Graph n e
kruskal weightFn g =
    Debug.todo "implement kruskal"



-- mapEdges using the weightFn
-- take all edges and sort them in order of their weights
-- start picking edges till all nodes are added
-- return
