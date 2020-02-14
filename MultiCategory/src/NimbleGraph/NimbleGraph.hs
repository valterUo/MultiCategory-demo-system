{-# LANGUAGE DeriveGeneric     #-}

module NimbleGraph.NimbleGraph where

import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as Map
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
    vertices :: Map.HashMap String (NimbleVertex a),
    edges :: Map.HashMap String (NimbleEdge a b)
} deriving (Show, Eq, Generic)

-- NimbleGraph creation

unwrapEitherVertex :: NimbleVertex (Either a b) -> Either (NimbleVertex a) (NimbleVertex b)
unwrapEitherVertex vertex = case vertexValue vertex of
    Left(y) -> Left $ NimbleVertex (vertexId vertex) y (inComingEdges vertex) (outGoingEdges vertex)
    Right(x) -> Right $ NimbleVertex (vertexId vertex) x (inComingEdges vertex) (outGoingEdges vertex)

emptyNimbleGraph :: NimbleGraph a b
emptyNimbleGraph = NimbleGraph Map.empty Map.empty

singleNimbleGraph :: Hashable a => a -> b -> NimbleGraph a b
singleNimbleGraph vertex edge = addVertex (NimbleVertex (show(hash vertex)) vertex (SortList.toSortedList []) (SortList.toSortedList [])) emptyNimbleGraph

addEdge :: NimbleEdge a b -> NimbleGraph a b -> NimbleGraph a b
addEdge edge graph = NimbleGraph (vertices graph) (Map.insert (edgeId edge) edge (edges graph))

addVertex :: NimbleVertex a -> NimbleGraph a b -> NimbleGraph a b
addVertex vertex graph = NimbleGraph (Map.insert (vertexId vertex) vertex (vertices graph)) (edges graph)

mkGraphFromTuples :: [(String, (String, a), (String, a), [String], b)] -> NimbleGraph a b
mkGraphFromTuples [] = NimbleGraph Map.empty Map.empty
mkGraphFromTuples (x:xs) = let (id, source, target, labels, value) = x in
    let tailGraph = mkGraphFromTuples xs in
        if Map.member id (edges $ tailGraph) 
            then tailGraph 
            else let (sourceId, sourceValue) = source in
                let (targetId, targetValue) = target in
                    if Map.member sourceId (vertices tailGraph) && Map.member targetId (vertices tailGraph)
                        then let oldSourceVertex = (vertices tailGraph) Map.! sourceId in 
                                let newSourceVertex = NimbleVertex (vertexId oldSourceVertex) (vertexValue oldSourceVertex) (inComingEdges oldSourceVertex) (SortList.insert id (outGoingEdges oldSourceVertex)) in
                                    let oldTargetVertex = (vertices tailGraph) Map.! targetId in 
                                        let newTargetVertex = NimbleVertex (vertexId oldTargetVertex) (vertexValue oldTargetVertex) (SortList.insert id (inComingEdges oldTargetVertex)) (outGoingEdges oldTargetVertex) in
                                            addEdge (NimbleEdge id value labels newSourceVertex newTargetVertex) (addVertex newTargetVertex (addVertex newSourceVertex tailGraph))
                        else if Map.member targetId (vertices tailGraph) 
                            then let oldTargetVertex = (vertices tailGraph) Map.! targetId in 
                                    let newTargetVertex = NimbleVertex (vertexId oldTargetVertex) (vertexValue oldTargetVertex) (SortList.insert id (inComingEdges oldTargetVertex)) (outGoingEdges oldTargetVertex) in
                                        addEdge (NimbleEdge id value labels (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) newTargetVertex) 
                                            (addVertex (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) 
                                                (addVertex newTargetVertex tailGraph))
                            else if Map.member sourceId (vertices tailGraph)
                                then let oldSourceVertex = (vertices tailGraph) Map.! sourceId in 
                                        let newSourceVertex = NimbleVertex (vertexId oldSourceVertex) (vertexValue oldSourceVertex) (inComingEdges oldSourceVertex) (SortList.insert id (outGoingEdges oldSourceVertex)) in
                                            addEdge (NimbleEdge id value labels newSourceVertex (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList []))) 
                                                (addVertex (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList [])) 
                                                    (addVertex newSourceVertex tailGraph))
                            else addEdge (NimbleEdge id value labels (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList []))) 
                                            (addVertex (NimbleVertex sourceId sourceValue (SortList.toSortedList []) (SortList.toSortedList [id])) 
                                                (addVertex (NimbleVertex targetId targetValue (SortList.toSortedList [id]) (SortList.toSortedList [])) tailGraph))

-- NimbleGraph querying

isEmpty :: NimbleGraph a b -> Bool
isEmpty graph = (Map.null $ vertices graph) && (Map.null $ edges graph)

vertexSize :: NimbleGraph a b -> Int
vertexSize graph = Map.size $ vertices graph

edgeSize :: NimbleGraph a b -> Int
edgeSize graph = Map.size $ edges graph

size :: NimbleGraph a b -> Int
size graph = vertexSize graph + edgeSize graph

prettyPrintSize :: NimbleGraph a b -> String
prettyPrintSize graph = "Vertices: " ++ (show $ vertexSize graph) ++ " Edges: " ++ (show $ edgeSize graph)

lookupVertexById :: String -> NimbleGraph a b -> Maybe (NimbleVertex a)
lookupVertexById key graph = Map.lookup key (vertices graph)

lookupEdgeById :: String -> NimbleGraph a b -> Maybe (NimbleEdge a b)
lookupEdgeById key graph = Map.lookup key (edges graph)

foldNimble :: (NimbleVertex a -> c -> c) -> (NimbleEdge a b -> c -> c) -> c -> NimbleGraph a b -> c
foldNimble vertexFunction edgeFunction structure graph = if isEmpty graph then structure else
    let foldedVertices = Map.foldr vertexFunction structure (vertices graph) in
        Map.foldr edgeFunction foldedVertices (edges graph)

inComingNeighbors :: NimbleVertex a -> NimbleGraph a b -> NimbleGraph a b
inComingNeighbors vertex graph = let newEdges = foldr (\edgeKey xs -> case Map.lookup edgeKey (edges graph) of
                                                        Nothing -> xs
                                                        Just(e) -> Map.insert edgeKey e xs) Map.empty (SortList.fromSortedList $ inComingEdges vertex) in
                                    let newVertices = Map.foldr (\e xs -> Map.insert (vertexId $ source e) (source e) xs) (Map.singleton (vertexId vertex) vertex) newEdges in 
                                        NimbleGraph newVertices newEdges

outGoingNeighbors :: NimbleVertex a -> NimbleGraph a b -> NimbleGraph a b
outGoingNeighbors vertex graph = let newEdges = foldr (\edgeKey xs -> case Map.lookup edgeKey (edges graph) of
                                                        Nothing -> xs
                                                        Just(e) -> Map.insert edgeKey e xs) Map.empty (SortList.fromSortedList $ outGoingEdges vertex) in
                                    let newVertices = Map.foldr (\e xs -> Map.insert (vertexId $ target e) (target e) xs) (Map.singleton (vertexId vertex) vertex) newEdges in 
                                        NimbleGraph newVertices newEdges

-- NimbleGraph manipulation

nimbleGraphUnion :: NimbleGraph a b -> NimbleGraph a b -> NimbleGraph a b
nimbleGraphUnion graph1 graph2 = NimbleGraph (Map.union (vertices graph1) (vertices graph2)) (Map.union (edges graph1) (edges graph2))