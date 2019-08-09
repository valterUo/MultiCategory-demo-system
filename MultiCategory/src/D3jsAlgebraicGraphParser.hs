{-# LANGUAGE DeriveGeneric     #-}

module D3jsAlgebraicGraphParser where

import Data.List
import Algebra.Graph
import D3jsGraphParser
import Data.Text.Lazy (Text)
import Data.Aeson

-- Returns a list of vertices. Does not require Ord like Algebra.Graph package vertexList function.
unorderedVertices :: Eq a => Graph a -> [a]
unorderedVertices graph = nub $ foldg [] (\x -> [x]) (++) (++) graph

-- Create a algebraic graph int where each int is the index of corresponding element in the list.
createIndexGraph :: Eq a => Graph a -> Graph Int
createIndexGraph graph = foldg empty (\x -> case elemIndex x (unorderedVertices graph) of
    Just y -> vertex y
    Nothing -> empty) overlay connect graph

createNodes :: (Eq a, ToJSON a) => Graph a -> [Text]
createNodes graph = encodeListToJSONText $ unorderedVertices graph

createLinks :: Eq a => Graph a -> [Link]
createLinks graph = foldr (\x xs -> let (a,b) = x in (Link a b):xs) [] (edgeList $ createIndexGraph graph)

createD3Graph :: (Eq a, ToJSON a) => Graph a -> D3jsGraph
createD3Graph graph = D3jsGraph (createNodes graph) (createLinks graph)