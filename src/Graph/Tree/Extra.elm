module Graph.Tree.Extra exposing (..)

import Fifo exposing (..)
import Graph
import Graph.Tree exposing (Tree, root)



{- This function takes a elm-community/Graph.Tree tree, converts the
   Tree into a elm-community/Graph Graph, which can then easily visualised using
   the Render module we wrote earlier
-}


toGraph : Graph.Tree.Tree label -> Graph.Graph label ()
toGraph tree =
    let
        ( nodes, edges ) =
            levelOrderAdv toGraphHelper ( [], [] ) tree
    in
    Graph.fromNodesAndEdges nodes edges


toGraphHelper : ( Int, label, Maybe Int ) -> Graph.Tree.Forest label -> ( List (Graph.Node label), List (Graph.Edge ()) ) -> ( List (Graph.Node label), List (Graph.Edge ()) )
toGraphHelper ( nId, lbl, pId ) _ ( nodes, edges ) =
    let
        newNode =
            Graph.Node nId lbl
    in
    case pId of
        Nothing ->
            ( newNode :: nodes, edges )

        Just pId_ ->
            ( newNode :: nodes, Graph.Edge pId_ nId () :: edges )


pushMany : List a -> Fifo a -> Fifo a
pushMany vals queue =
    List.foldl Fifo.insert queue vals


levelOrderAdv : (( Int, label, Maybe Int ) -> Graph.Tree.Forest label -> acc -> acc) -> acc -> Tree label -> acc
levelOrderAdv visit acc tree =
    let
        go acc_ toVisit nodeId =
            case Fifo.remove toVisit of
                ( Nothing, _ ) ->
                    acc_

                ( Just ( tree_, parentId ), othersToVisit ) ->
                    case root tree_ of
                        Nothing ->
                            go acc_ othersToVisit nodeId

                        Just ( label, children ) ->
                            go (visit ( nodeId, label, parentId ) children acc_) (pushMany (List.map (\c -> ( c, Just nodeId )) children) othersToVisit) (nodeId + 1)
    in
    go acc (Fifo.empty |> Fifo.insert ( tree, Nothing )) 0
