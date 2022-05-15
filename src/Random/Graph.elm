module Random.Graph exposing
    ( simpleConnectedGraph
    , simpleConnectedGraphWithRandomNodeEdgeLabels
    )

{-| This module is useful for generating a random graph with given number of edges and nodes.

#Generators

@docs simpleConnectedGraph

@docs simpleConnectedGraphWithRandomNodeEdgeLabels

-}

import Graph as G
import Random as R
import Random.List as RL


{-| Similar to simpleConnectedGraph, also generates edge labels and node labels using the given generator.
-}
simpleConnectedGraphWithRandomNodeEdgeLabels : Int -> Int -> R.Generator n -> R.Generator e -> R.Generator (G.Graph n e)
simpleConnectedGraphWithRandomNodeEdgeLabels nodesCnt edgesCnt nodeLabelGenerator edgeLabelGenerator =
    simpleConnectedGraph nodesCnt edgesCnt
        |> R.andThen
            (\( nodeIds, edgePairs ) ->
                R.map2
                    (\nodeLabels edgeLabels ->
                        G.fromNodesAndEdges
                            (List.map2 G.Node nodeIds nodeLabels)
                            (List.map2 (\( f, t ) eLabels -> G.Edge f t eLabels) edgePairs edgeLabels)
                    )
                    (R.list
                        (List.length nodeIds)
                        nodeLabelGenerator
                    )
                    (R.list
                        (List.length edgePairs)
                        edgeLabelGenerator
                    )
            )


{-| This function generates a random graph that is connected if number of edges >= number of nodes - 1

_Note_: You can also generate random trees, just pass edge count as n-1.

-}
simpleConnectedGraph : Int -> Int -> R.Generator ( List Int, List ( Int, Int ) )
simpleConnectedGraph nodeCnt edgeCnt =
    if nodeCnt > 0 then
        let
            nodeIds =
                List.range 0 (nodeCnt - 1)

            edgeCnt_ =
                min edgeCnt (nodeCnt ^ 2)
        in
        R.pair (R.constant nodeIds)
            (generateEdge edgeCnt_ ( [], nodeIds ) [])

    else
        R.constant ( [], [] )


generateEdge : Int -> ( List Int, List Int ) -> List ( Int, Int ) -> R.Generator (List ( Int, Int ))
generateEdge eCnt ( selNodes, unSelNodes ) edges =
    if eCnt <= 0 then
        R.constant edges

    else if selNodes == [] && edges == [] then
        RL.choices 2 unSelNodes
            |> R.andThen
                (\( l1, l2 ) ->
                    case l1 of
                        [] ->
                            generateEdge eCnt ( selNodes, unSelNodes ) edges

                        [ n ] ->
                            generateEdge eCnt ( List.append selNodes l1, l2 ) edges

                        [ n1, n2 ] ->
                            generateEdge (eCnt - 1) ( List.append selNodes l1, l2 ) (List.append edges [ ( n1, n2 ) ])

                        n :: ns ->
                            let
                                newEdges =
                                    List.map (\n2 -> ( n, n2 )) ns
                            in
                            generateEdge (eCnt - List.length newEdges) ( List.append selNodes l1, l2 ) (List.append edges newEdges)
                )

    else
        RL.choose selNodes
            |> R.andThen
                (\( n, l ) ->
                    case n of
                        Just n1 ->
                            if List.length unSelNodes == 0 then
                                RL.choose selNodes
                                    |> R.andThen
                                        (\( n2_, l2 ) ->
                                            case n2_ of
                                                Just n2 ->
                                                    if List.member ( n1, n2 ) edges then
                                                        generateEdge eCnt ( selNodes, unSelNodes ) edges

                                                    else
                                                        generateEdge (eCnt - 1) ( selNodes, unSelNodes ) (( n1, n2 ) :: edges)

                                                Nothing ->
                                                    generateEdge eCnt ( selNodes, unSelNodes ) edges
                                        )

                            else
                                RL.choose unSelNodes
                                    |> R.andThen
                                        (\( n2_, l2 ) ->
                                            case n2_ of
                                                Just n2 ->
                                                    generateEdge (eCnt - 1) ( n2 :: selNodes, l2 ) (( n1, n2 ) :: edges)

                                                Nothing ->
                                                    generateEdge eCnt ( selNodes, unSelNodes ) edges
                                        )

                        Nothing ->
                            generateEdge eCnt ( selNodes, unSelNodes ) edges
                )
