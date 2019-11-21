{-# LANGUAGE DeriveGeneric     #-}

module NimbleGraph.NimbleGraph where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import qualified Data.SortedList as SortList

data Edge a = Edge {
    edgeId :: String,
    edgeValue :: a,
    labels :: [String],
    source :: Vertex a,
    target :: Vertex a
} deriving (Show, Eq, Generic)

data Vertex a = Vertex {
    vertexId :: String,
    vertexValue :: a,
    inComingEdges :: SortList.SortedList String,
    outGoingEdges :: SortList.SortedList String
} deriving (Show, Eq, Generic)

data NimbleGraph a = NimbleGraph {
    vertices :: HashMap String (Vertex a),
    edges :: HashMap String (Edge a)
} deriving (Show, Eq, Generic)

addEdge :: Edge a -> NimbleGraph a -> NimbleGraph a
addEdge edge graph = NimbleGraph (vertices graph) (insert (edgeId edge) edge (edges graph))

addVertex :: Vertex a -> NimbleGraph a -> NimbleGraph a
addVertex vertex graph = NimbleGraph (insert (vertexId vertex) vertex (vertices graph)) (edges graph)

mkGraphFromTuples :: [(String, (String, a), (String, a), [String], a)] -> NimbleGraph a
mkGraphFromTuples [] = NimbleGraph empty empty
mkGraphFromTuples (x:xs) = let (id, source, target, labels, value) = x in
    let tailGraph = mkGraphFromTuples xs in
        if member id (edges $ tailGraph) 
            then tailGraph 
            else let (sourceId, sourceValue) = source in
                let (targetId, targetValue) = target in
                    if member sourceId (vertices tailGraph) && member targetId (vertices tailGraph)
                        then let oldSourceVertex = (vertices tailGraph) ! sourceId in 
                                let newSourceVertex = Vertex (vertexId oldSourceVertex) (vertexValue oldSourceVertex) (inComingEdges oldSourceVertex) (SortList.insert id (outGoingEdges oldSourceVertex)) in
                                    let oldTargetVertex = (vertices tailGraph) ! targetId in 
                                        let newTargetVertex = Vertex (vertexId oldTargetVertex) (vertexValue oldTargetVertex) (SortList.insert id (inComingEdges oldTargetVertex)) (outGoingEdges oldTargetVertex) in
                                            addEdge (Edge id value labels newSourceVertex newTargetVertex) (addVertex newTargetVertex (addVertex newSourceVertex tailGraph))
                        else if member targetId (vertices tailGraph) 
                            then let oldTargetVertex = (vertices tailGraph) ! targetId in 
                                    let newTargetVertex = Vertex (vertexId oldTargetVertex) (vertexValue oldTargetVertex) (SortList.insert id (inComingEdges oldTargetVertex)) (outGoingEdges oldTargetVertex) in
                                        addEdge (Edge id value labels (Vertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) newTargetVertex) 
                                            (addVertex (Vertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) 
                                                (addVertex newTargetVertex tailGraph))
                            else if member sourceId (vertices tailGraph)
                                then let oldSourceVertex = (vertices tailGraph) ! sourceId in 
                                        let newSourceVertex = Vertex (vertexId oldSourceVertex) (vertexValue oldSourceVertex) (inComingEdges oldSourceVertex) (SortList.insert id (outGoingEdges oldSourceVertex)) in
                                            addEdge (Edge id value labels newSourceVertex (Vertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList []))) 
                                                (addVertex (Vertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList [])) 
                                                    (addVertex newSourceVertex tailGraph))
                            else addEdge (Edge id value labels (Vertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) (Vertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList []))) 
                                            (addVertex (Vertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) 
                                                (addVertex (Vertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList [])) tailGraph))