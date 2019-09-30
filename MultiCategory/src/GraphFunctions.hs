module GraphFunctions where

import Algebra.Graph
import Data.List
import Debug.Trace

patternSource :: Eq a => a -> Graph a -> Graph a -> Graph a
patternSource x (Vertex y) z = if x == y then z else empty
patternSource _ _ _ = empty

patternTarget :: Eq a => a -> Graph a -> Graph a -> Graph a
patternTarget x z (Vertex y) = if x == y then z else empty
patternTarget _ _ _ = empty

findTargetNeighborsWithEmptyNodes :: Eq a => a -> Graph a -> Graph a
findTargetNeighborsWithEmptyNodes x graph = foldg empty vertex overlay (\y z -> patternSource x y z) graph

findTargetNeighbors :: Eq a => a -> Graph a -> Graph a
findTargetNeighbors customer graph = let neighbors = findTargetNeighborsWithEmptyNodes customer graph in
    let accepted = (foldg [] (\x -> [x]) (++) (++) (neighbors)) in
        induce (\x -> elem x accepted) (neighbors)

findSourceNeighborsWithEmptyNodes :: Eq a => a -> Graph a -> Graph a
findSourceNeighborsWithEmptyNodes x graph = foldg empty vertex overlay (\y z -> patternTarget x y z) graph

findSourceNeighbors :: Eq a => a -> Graph a -> Graph a
findSourceNeighbors customer graph = let neighbors = findSourceNeighborsWithEmptyNodes customer graph in
    let accepted = (foldg [] (\x -> [x]) (++) (++) (neighbors)) in
        induce (\x -> elem x accepted) (neighbors)

reachable :: Eq a => a -> a -> Graph a -> Bool
reachable x y graphLeft = let neighbors = removeVertex x (findTargetNeighbors x graphLeft) in
    if isEmpty(graphLeft) || isEmpty(neighbors) then False else 
            if hasVertex y neighbors then True else
                let targetList = nub $ (foldg [] (\x -> [x]) (++) (++) neighbors) in
                    let newGraph = removeVertex x graphLeft in
                        if isEmpty(newGraph) then False else
                            let booleanList = map (\z -> reachable z y newGraph) targetList in
                                elem True booleanList