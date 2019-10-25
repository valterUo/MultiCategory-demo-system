module GraphFunctions where

import Algebra.Graph
import Data.List
import Debug.Trace

patternSource :: Eq a => a -> Graph a -> Graph a -> Graph a
patternSource x (Vertex z) (Vertex y) = if x == y then Vertex z else empty
patternSource _ _ _ = empty

patternTarget :: Eq a => a -> Graph a -> Graph a -> Graph a
patternTarget x (Vertex y) (Vertex z) = if x == y then Vertex z else empty
patternTarget _ _ _ = empty

patternOverlay :: Eq a => a -> Graph a -> Graph a -> Graph a
patternOverlay main (Vertex x) (Vertex y) = if x == main && y == main then Overlay (Vertex x) (Vertex y) else if x == main then Vertex x else if y == main then Vertex y else empty
patternOverlay main (Overlay t k) (Vertex y) = if y == main then Overlay (Overlay t k) (Vertex y) else Overlay t k
patternOverlay main (Vertex x) (Overlay t k) = if x == main then Overlay (Vertex x) (Overlay t k) else Overlay t k
patternOverlay main (Overlay x y) (Overlay k t) = Overlay (Overlay x y) (Overlay k t) 
patternOverlay main (Connect x y) (Vertex z) = if z == main then Overlay (Connect x y) (Vertex z) else Connect x y
patternOverlay main (Vertex z) (Connect x y) = if z == main then Overlay (Vertex z) (Connect x y) else Connect x y
patternOverlay main (Connect x y) (Overlay k t) = Overlay (Connect x y) (Overlay k t)
patternOverlay main (Overlay k t) (Connect x y) = Overlay (Overlay k t) (Connect x y)
patternOverlay main (Connect x y) (Connect k t) = Overlay (Connect x y) (Connect k t)
patternOverlay main x y = Overlay x y

findTargetNeighborsWithEmptyNodes :: Eq a => a -> Graph a -> Graph a
findTargetNeighborsWithEmptyNodes x graph = let newgraph = foldg empty vertex (\y z -> patternOverlay x y z) connect graph in 
    foldg empty vertex overlay (\y z -> patternTarget x y z) newgraph

findTargetNeighbors :: Eq a => a -> Graph a -> Graph a
findTargetNeighbors x graph = let neighbors = findTargetNeighborsWithEmptyNodes x graph in
    let accepted = foldg [] (\x -> [x]) (++) (++) neighbors in
        let neighbors2 = induce (\x -> elem x accepted) neighbors in
            Connect (Vertex x) neighbors2

findSourceNeighborsWithEmptyNodes :: Eq a => a -> Graph a -> Graph a
findSourceNeighborsWithEmptyNodes x graph = let newgraph = foldg empty vertex (\y z -> patternOverlay x y z) connect graph in 
    foldg empty vertex overlay (\y z -> patternSource x y z) newgraph

findSourceNeighbors :: Eq a => a -> Graph a -> Graph a
findSourceNeighbors x graph = let neighbors = findSourceNeighborsWithEmptyNodes x graph in
    let accepted = foldg [] (\x -> [x]) (++) (++) neighbors in
        let neighbors2 = induce (\x -> elem x accepted) neighbors in
            Connect neighbors2 (Vertex x)

reachable :: Eq a => a -> a -> Graph a -> Bool
reachable x y graphLeft = 
    let neighbors = removeVertex x (findTargetNeighbors x graphLeft) in
        if isEmpty(graphLeft) || isEmpty(neighbors) then False else 
                if hasVertex y neighbors then True else
                    let targetList = nub $ (foldg [] (\x -> [x]) (++) (++) neighbors) in
                        let newGraph = removeVertex x graphLeft in
                            if isEmpty(newGraph) then False else
                                let booleanList = map (\z -> reachable z y newGraph) targetList in
                                    elem True booleanList