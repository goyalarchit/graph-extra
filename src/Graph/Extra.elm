module Graph.Extra exposing (alongAnyEdges, bellmanFord, kruskal)

import Dict
import Graph
import List.Extra as LE


{-| Utility

1.  adjacentNodes : both incoming and outgoing edges

Algorithms

1.  Bellman Ford
2.  Kruskals

-}
alongAnyEdges : Graph.NeighborSelector n e
alongAnyEdges nodeCtx =
    List.append (Graph.alongIncomingEdges nodeCtx) (Graph.alongOutgoingEdges nodeCtx)
        |> LE.unique


bellmanFord : Graph.NodeId -> (Graph.Edge e -> Float) -> Graph.Graph n e -> Maybe (Dict.Dict Graph.NodeId ( Float, List Graph.NodeId ))
bellmanFord sourceId weightFn g =
    let
        weightedEdges =
            List.map (\e -> ( e.from, e.to, weightFn e )) (Graph.edges g)

        -- INITIALIZE -SINGLE -SOURCE (G, s)
        distancePathDict =
            Dict.singleton sourceId ( 0, [] )

        shortestDistPathDict =
            runBellmanRelax (Graph.size g - 1) weightedEdges distancePathDict

        ( _, dictUpdated ) =
            List.foldl bellmanRelax ( shortestDistPathDict, False ) weightedEdges
    in
    if dictUpdated then
        Nothing

    else
        Just shortestDistPathDict


runBellmanRelax : Int -> List ( Graph.NodeId, Graph.NodeId, Float ) -> Dict.Dict Graph.NodeId ( Float, List Graph.NodeId ) -> Dict.Dict Graph.NodeId ( Float, List Graph.NodeId )
runBellmanRelax count weightedEdges distancePathDict =
    if count > 0 then
        let
            ( newDistancePathDict, dictUpdated ) =
                List.foldl bellmanRelax ( distancePathDict, False ) weightedEdges
        in
        if dictUpdated then
            runBellmanRelax (count - 1) weightedEdges newDistancePathDict

        else
            distancePathDict

    else
        distancePathDict


bellmanRelax : ( Graph.NodeId, Graph.NodeId, Float ) -> ( Dict.Dict Graph.NodeId ( Float, List Graph.NodeId ), Bool ) -> ( Dict.Dict Graph.NodeId ( Float, List Graph.NodeId ), Bool )
bellmanRelax ( u, v, w ) ( distancePathDict, updated ) =
    case Dict.get u distancePathDict of
        Nothing ->
            ( distancePathDict, updated )

        Just ( d_u, p_u ) ->
            case Dict.get v distancePathDict of
                Nothing ->
                    ( Dict.insert v ( d_u + w, List.append p_u [ u ] ) distancePathDict, True )

                Just ( d_v, _ ) ->
                    if d_v > (d_u + w) then
                        ( Dict.insert v ( d_u + w, List.append p_u [ u ] ) distancePathDict, True )

                    else
                        ( distancePathDict, updated )


kruskal : (Graph.Edge e -> Float) -> Graph.Graph n e -> Graph.Graph n e
kruskal weightFn g =
    Debug.todo "implement kruskal"



-- mapEdges using the weightFn
-- take all edges and sort them in order of their weights
-- start picking edges till all nodes are added
-- return
