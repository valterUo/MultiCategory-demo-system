{-# LANGUAGE DeriveGeneric     #-}

module NimbleGraph.NimbleGraph where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import Data.Hashable
import qualified Data.SortedList as SortList

data NimbleEdge a b = NimbleEdge {
    edgeId :: String,
    edgeValue :: b,
    labels :: [String],
    source :: NimbleVertex a,
    target :: NimbleVertex a
} deriving (Show, Eq, Generic)

data NimbleVertex a = NimbleVertex {
    vertexId :: String,
    vertexValue :: a,
    inComingEdges :: SortList.SortedList String,
    outGoingEdges :: SortList.SortedList String
} deriving (Show, Eq, Generic)

data NimbleGraph a b = NimbleGraph {
    vertices :: HashMap String (NimbleVertex a),
    edges :: HashMap String (NimbleEdge a b)
} deriving (Show, Eq, Generic)

-- NimbleGraph creation

emptyNimbleGraph :: NimbleGraph a b
emptyNimbleGraph = NimbleGraph empty empty

singleNimbleGraph :: Hashable a => a -> b -> NimbleGraph a b
singleNimbleGraph vertex edge = addVertex (NimbleVertex (show(hash vertex)) vertex (SortList.toSortedList []) (SortList.toSortedList [])) emptyNimbleGraph

addEdge :: NimbleEdge a b -> NimbleGraph a b -> NimbleGraph a b
addEdge edge graph = NimbleGraph (vertices graph) (insert (edgeId edge) edge (edges graph))

addVertex :: NimbleVertex a -> NimbleGraph a b -> NimbleGraph a b
addVertex vertex graph = NimbleGraph (insert (vertexId vertex) vertex (vertices graph)) (edges graph)

mkGraphFromTuples :: [(String, (String, a), (String, a), [String], b)] -> NimbleGraph a b
mkGraphFromTuples [] = NimbleGraph empty empty
mkGraphFromTuples (x:xs) = let (id, source, target, labels, value) = x in
    let tailGraph = mkGraphFromTuples xs in
        if member id (edges $ tailGraph) 
            then tailGraph 
            else let (sourceId, sourceValue) = source in
                let (targetId, targetValue) = target in
                    if member sourceId (vertices tailGraph) && member targetId (vertices tailGraph)
                        then let oldSourceVertex = (vertices tailGraph) ! sourceId in 
                                let newSourceVertex = NimbleVertex (vertexId oldSourceVertex) (vertexValue oldSourceVertex) (inComingEdges oldSourceVertex) (SortList.insert id (outGoingEdges oldSourceVertex)) in
                                    let oldTargetVertex = (vertices tailGraph) ! targetId in 
                                        let newTargetVertex = NimbleVertex (vertexId oldTargetVertex) (vertexValue oldTargetVertex) (SortList.insert id (inComingEdges oldTargetVertex)) (outGoingEdges oldTargetVertex) in
                                            addEdge (NimbleEdge id value labels newSourceVertex newTargetVertex) (addVertex newTargetVertex (addVertex newSourceVertex tailGraph))
                        else if member targetId (vertices tailGraph) 
                            then let oldTargetVertex = (vertices tailGraph) ! targetId in 
                                    let newTargetVertex = NimbleVertex (vertexId oldTargetVertex) (vertexValue oldTargetVertex) (SortList.insert id (inComingEdges oldTargetVertex)) (outGoingEdges oldTargetVertex) in
                                        addEdge (NimbleEdge id value labels (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) newTargetVertex) 
                                            (addVertex (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) 
                                                (addVertex newTargetVertex tailGraph))
                            else if member sourceId (vertices tailGraph)
                                then let oldSourceVertex = (vertices tailGraph) ! sourceId in 
                                        let newSourceVertex = NimbleVertex (vertexId oldSourceVertex) (vertexValue oldSourceVertex) (inComingEdges oldSourceVertex) (SortList.insert id (outGoingEdges oldSourceVertex)) in
                                            addEdge (NimbleEdge id value labels newSourceVertex (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList []))) 
                                                (addVertex (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList [])) 
                                                    (addVertex newSourceVertex tailGraph))
                            else addEdge (NimbleEdge id value labels (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList []))) 
                                            (addVertex (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) 
                                                (addVertex (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList [])) tailGraph))

-- NimbleGraph querying

isEmpty :: NimbleGraph a b -> Bool
isEmpty graph = (Data.HashMap.Strict.null $ vertices graph) && (Data.HashMap.Strict.null $ edges graph)

lookupVertexById :: String -> NimbleGraph a b -> Maybe (NimbleVertex a)
lookupVertexById key graph = Data.HashMap.Strict.lookup key (vertices graph)

lookupEdgeById :: String -> NimbleGraph a b -> Maybe (NimbleEdge a b)
lookupEdgeById key graph = Data.HashMap.Strict.lookup key (edges graph)

foldNimble :: (NimbleVertex a -> c -> c) -> (NimbleEdge a b -> c -> c) -> c -> NimbleGraph a b -> c
foldNimble vertexFunction edgeFunction structure graph = if isEmpty graph then structure else
    let foldedVertices = Data.HashMap.Strict.foldr vertexFunction structure (vertices graph) in
        Data.HashMap.Strict.foldr edgeFunction foldedVertices (edges graph)

-- NimbleGraph manipulation

nimbleGraphUnion :: NimbleGraph a b -> NimbleGraph a b -> NimbleGraph a b
nimbleGraphUnion graph1 graph2 = NimbleGraph (union (vertices graph1) (vertices graph2)) (union (edges graph1) (edges graph2))